(*
 * Translation of fixpoints and match statements to induction principles.
 * By Nate Yazdani, taken from the DEVOID code.
 *)

open Util
open Names
open Univ
open Context
open Term
open Constr
open Inductiveops
open CErrors
open Abstraction
open Inference
open Contextutils
open Apputils
open Defutils
open Funutils
open Envutils
open Utilities
open Debruijn
open Reducers
open Stateutils
open Hofs

(* --- Utilities for inductive types --- *)

(*
 * TODO move some/all of these into coq-plugin-lib, and make follow
 * the standard in that lib
 *)

(*
 * Extract the components of an inductive type
 *)
let decompose_indvect env ind_type sigma =
  let t = first_fun ind_type in
  let args = unfold_args ind_type in
  let t_red = unwrap_definition env t in
  let sigma, ind_type = reduce_term env sigma (mkAppl (t_red, args)) in
  let pind, args =
    try
      decompose_appvect ind_type |> on_fst destInd
    with _ ->
      failwith "type passed to decompose_indvect must be an inductive type"
  in
  let nparam = inductive_nparams (out_punivs pind) in
  let params, indices = Array.chop nparam args in
  sigma, (pind, params, indices)

(*
 * Same as decompose_indvect but converts the result arrays into lists.
 *)
let decompose_ind env ind_type =
  bind
    (decompose_indvect env ind_type)
    (fun arrs ->
      ret (on_pi3 Array.to_list (on_pi2 Array.to_list arrs)))
                                                               
(*
 * Construct a relative context, consisting of only local assumptions,
 * quantifying over instantiations of the inductive family.
 *
 * In other words, the output relative context assumes all indices (in standard
 * order) and then a value of the inductive type (at those indices).
 *
 * Note that an inductive family is an inductive name with parameter terms.
 *)
let build_inductive_context env ind_fam ind_name =
  let ind_type = build_dependent_inductive env ind_fam in
  let ind_decl = rel_assum (ind_name, ind_type) in
  get_arity env ind_fam |> fst |> Rel.add ind_decl |> Termops.smash_rel_context

(* --- Fixpoint to eliminator translation --- *)
                                                        
(*
 * Transform the relative context of a fixed-point function into a form suitable
 * for simple recursion (i.e., eliminator-style quantification).
 *
 * The transformed relative context only satisfies that guarantee (or even
 * well-formedness) when immediately preceded by the quantifying relative
 * context for the inductive type and then by a wrapping relative context
 * for the fixed point.
 *)
let build_recursive_context fix_ctxt params indices =
  let nb = Rel.length fix_ctxt in (* length of fixed-point context *)
  let nb' = nb + List.length indices + 1 in (* length of recursive context *)
  let par_rels = List.fold_left (free_rels 0) Int.Set.empty params in
  let idx_rels = 1 :: List.rev_map destRel indices in
  (* NOTE: DestKO raised (above) if any index was not bound fully abstracted. *)
  let is_rec i = i <= nb in
  let is_par i = not (Int.Set.mem i par_rels) in (* parameter independence *)
  assert (List.for_all is_rec idx_rels); (* Is every index bound recursively? *)
  assert (List.distinct idx_rels); (* Are the indices bound separately and... *)
  assert (List.for_all is_par idx_rels); (* ...independently of parameters? *)
  (* Abstract inductive quantification to the outer inductive context *)
  let buf = Termops.lift_rel_context nb' fix_ctxt |> Array.of_list in
  let abstract_rel j i = buf.(i - 1) <- define_rel_decl (mkRel j) buf.(i - 1) in
  (* Abstract each parameter-relevant binding to the wrapper context. *)
  Int.Set.iter (abstract_rel nb') (Int.Set.filter is_rec par_rels);
  (* Abstract the remaining inductive bindings to the eliminator context. *)
  List.iter2 abstract_rel (List.map_i (-) (nb + 1) idx_rels) idx_rels;
  Array.to_list buf

(*
 * Build the minor premise for elimination at a constructor from the
 * corresponding fixed-point case.
 *
 * In particular, insert recurrence bindings (for inductive hypotheses) in the
 * appropriate positions, substituting recursive calls with the recurrence
 * binding its value.
 *
 * The last argument provides the case's parameter context (quantifying
 * constructor arguments) with the case's body term.
 *)
let premise_of_case env ind_fam (ctxt, body) =
  let nb = Rel.length ctxt in
  let ind_head = dest_ind_family ind_fam |> on_fst mkIndU |> applist in
  let fix_name, fix_type = Environ.lookup_rel 1 env |> pair rel_name rel_type in
  let insert_recurrence i body decl =
    let i = unshift_i_by i nb in
    let j = shift_i i in
    let body' =
      match eq_constr_head (shift_by i ind_head) (rel_type decl) with
      | Some indices ->
        assert (is_rel_assum decl);
        let args = Array.append (Array.map shift indices) [|mkRel 1|] in
        let rec_type = prod_appvect (shift_by j fix_type) args in
        let fix_call = mkApp (mkRel j, args) in
        mkLambda (fix_name, rec_type, abstract_subterm fix_call body)
      | _ ->
        body
    in mkLambda_or_LetIn decl body'
  in List.fold_left_i insert_recurrence 0 body ctxt

(*
 * Given a constructor summary (cf., Inductiveops), build a parameter context
 * to quantify over constructor arguments (and thus values of that constructor)
 * and partially evaluate the functional applied to the constructed value's type
 * indices and (subsequently) to the constructed value itself.
 *
 * Partial evaluation reduces to beta/iota-normal form. Exclusion of delta
 * reduction is intentional (rarely beneficial, usually detrimental).
 *)
let split_case env sigma fun_term cons_sum =
  let cons = build_dependent_constructor cons_sum in
  let env = Environ.push_rel_context cons_sum.cs_args env in
  let body =
    let head = shift_by cons_sum.cs_nargs fun_term in
    let args = Array.append cons_sum.cs_concl_realargs [|cons|] in
    mkApp (head, args) |> Reduction.nf_betaiota env
  in sigma, deanonymize_context env sigma cons_sum.cs_args, body

(*
 * Eta-expand a case term according to the corresponding constructor's type.
 *)
let expand_case env sigma case_term cons_sum =
  let body =
    let head = shift_by cons_sum.cs_nargs case_term in
    let args = Rel.to_extended_list mkRel 0 cons_sum.cs_args in
    Reduction.beta_applist head args
  in sigma, deanonymize_context env sigma cons_sum.cs_args, body

(*
 * Build an elimination head (partially applied eliminator) including the
 * parameters and (sort-adjusted) motive for the given inductive family and
 * (dependent) elimination type.
 *
 * The sorts of the inductive family and of the elimination type are considered,
 * respectively, when adjusting the elimination type into a motive (by removing
 * dependency for Prop-sorted inductive families) and when selecting one of the
 * inductive family's eliminators.
 *
 * NOTE: Motive adjustment might be too overzealous; under some particular
 * conditions, Coq does allow dependency in the elimination motive for a Prop-
 * sorted inductive family.
 *
 * TODO possible to reuse any of our existing functions here? Or move some
 * of this back to lib?
 *)
let configure_eliminator env sigma ind_fam typ =
  let ind, params = dest_ind_family ind_fam |> on_fst out_punivs in
  let nb = inductive_nrealargs ind + 1 in
  let typ_ctxt, typ_body =
    let typ_ctxt, typ_body = decompose_prod_n_assum nb typ in
    let ind_sort = get_arity env ind_fam |> snd in
    if Sorts.family_equal ind_sort Sorts.InProp then
      List.tl typ_ctxt, unshift typ_body
    else
      typ_ctxt, typ_body
  in
  let sigma, elim =
    let typ_env = Environ.push_rel_context typ_ctxt env in
    let sigma, typ_sort = infer_sort typ_env sigma typ_body in
    let elim_trm = Indrec.lookup_eliminator ind typ_sort in
    new_global sigma elim_trm
  in
  let motive = recompose_lam_assum typ_ctxt typ_body in
  sigma, mkApp (elim, Array.append (Array.of_list params) [|motive|])

(*
 * Translate a fixed-point function using simple recursion (i.e., quantifying
 * the inductive type like an eliminator) into an elimination form.
 *)
let desugar_recursion env sigma ind_fam fix_name fix_type fix_term =
  (* Build the elimination head (eliminator with parameters and motive) *)
  let sigma, elim_head = configure_eliminator env sigma ind_fam fix_type in
  (* Build the minor premises *)
  let sigma, premises =
    let fix_env = push_local (fix_name, fix_type) env in
    let build_premise cons_sum sigma =
      let cons_sum = lift_constructor 1 cons_sum in
      let sigma, split_ctx, split = split_case fix_env sigma fix_term cons_sum in
      sigma, unshift (premise_of_case fix_env ind_fam (split_ctx, split))
    in map_state_array build_premise (get_constructors env ind_fam) sigma
  in sigma, mkApp (elim_head, premises)

(*
 * Translate a fixed-point function into an elimination form.
 *
 * This function works by transforming the fixed point to use simple recursion
 * (i.e., to quantify the inductive type like a dependent eliminator), calling
 * desugar_recusion, and then wrapping the translated elimination form to conform
 * to the original fixed point's type.
 *
 * Note that the resulting term will not satisfy definitional equality with the
 * original term but should satisfy most (all?) definitional equalities when
 * applied to all indices and a head-canonical discriminee. Still, this could
 * impact the well-typedness of inductive proof terms, particularly when
 * rewriting the unrolled recursive function by an inductive hypothesis. We will
 * know more after testing compositional translation of a complete module, which
 * will avoid incidental mixtures of the old version (by named constant) and the
 * new version (by expanded definition). (Such incidental mixtures arise, for
 * example, in some of the List module's proofs regarding the In predicate.)
 *)
let desugar_fixpoint env sigma fix_pos fix_name fix_type fix_term =
  let nb = fix_pos + 1 in (* number of bindings guarding recursion *)
  (* Pull off bindings through the parameter guarding structural recursion *)
  let fix_ctxt, fix_type = decompose_prod_n_zeta nb fix_type in
  let _, fix_term = decompose_lam_n_zeta nb fix_term in
  (* Gather information on the inductive type for recursion/elimination *)
  let ind_name, ind_type = Rel.lookup 1 fix_ctxt |> pair rel_name rel_type in
  let sigma, (pind, params, indices) = decompose_ind env (shift ind_type) sigma in
  let ind_fam = make_ind_family (pind, params) in
  let env = Environ.push_rel_context fix_ctxt env in (* for eventual wrapper *)
  let rec_ctxt, rec_args = (* quantify the inductive type like an eliminator *)
    let ind_ctxt = build_inductive_context env ind_fam ind_name in
    let fun_ctxt = build_recursive_context fix_ctxt params indices in
    fun_ctxt @ ind_ctxt,
    Array.of_list (indices @ (mkRel 1) :: Rel.to_extended_list mkRel 0 fun_ctxt)
  in
  let nb' = Rel.length rec_ctxt in
  let k = unshift_i_by nb nb' in (* always more bindings than before *)
  let rec_type =
    fix_type |> shift_local nb nb |> (* for external wrapper *)
    shift_local nb k |> smash_prod_assum rec_ctxt
  in
  let rec_term =
    let nb_rec = shift_i nb in (* include self reference *)
    let rec_env = Environ.push_rel (rel_assum (fix_name, rec_type)) env in
    let rec_ctxt = Termops.lift_rel_context 1 rec_ctxt in
    let fix_self = (* wrapper to adjust arguments for a recursive call *)
      recompose_lam_assum
        (Termops.lift_rel_context nb_rec fix_ctxt)
        (mkApp (mkRel nb_rec, rec_args))
    in
    fix_term |> shift_local nb_rec nb |> (* for external wrapper *)
    shift_local nb k |> smash_lam_assum rec_ctxt |>
    shift_local 1 1 |> Vars.subst1 fix_self |> Reduction.nf_betaiota rec_env
  in
  (* Desugar the simple recursive function into an elimination form *)
  let sigma, rec_elim = desugar_recursion env sigma ind_fam fix_name rec_type rec_term in
  (* Wrap the elimination form to reorder initial arguments *)
  sigma, recompose_lam_assum fix_ctxt (mkApp (rec_elim, rec_args))

(*
 * Given the components of a match expression, build an equivalent elimination
 * expression. The resulting term will not use any recurrence (i.e., inductive
 * hypothesis) bound in the minor elimination premises (i.e., case functions),
 * since the original term was non-recursive.
 *
 * Note that the resulting term may not satisfy definitional equality with the
 * original term, as Coq lacks eta-conversion between a non-recursive function
 * and its fixed point (i.e., f =\= fix[_.f]). Definitional equality should hold
 * (at least) when the discriminee term is head-canonical.
 *)
let desugar_match env sigma info pred discr cases =
  let typ = lambda_to_prod pred in
  let sigma, discr_typ = reduce_type_using whd env sigma discr in
  let sigma, (pind, params, indices) = decompose_indvect env discr_typ sigma in
  let ind_fam = make_ind_family (pind, Array.to_list params) in
  let sigma, elim_head = configure_eliminator env sigma ind_fam typ in
  let premises =
    let fix_env = push_local (Name.Anonymous, typ) env in
    let cases = Array.map shift cases in
    let build_premise cons_case cons_sum =
      let cons_sum = lift_constructor 1 cons_sum in
      let sigma, expanded_ctx, expanded = expand_case fix_env sigma cons_case cons_sum in
      unshift (premise_of_case fix_env ind_fam (expanded_ctx, expanded)) (* TODO sigma *)
    in get_constructors fix_env ind_fam |> Array.map2 build_premise cases
  in sigma, mkApp (elim_head, Array.concat [premises; indices; [|discr|]])

(*
 * Translate the given term into an equivalent, bisimulative (i.e., homomorpic
 * reduction behavior) version using eliminators instead of match or fix
 * expressions.
 *
 * Mutual recursion, co-recursion, and universe polymorphism are not supported.
 *
 * Talia: The combination of map_term_env_if and a recursive call basically says
 * that we continue to apply the condition to subterms even when the predicate
 * is successful. At some point, it may make sense to move this HOF into
 * the library, too. But using map_term_env_if saves us from all of the
 * boilerplate around threading things through terms with evars otherwise.
 *)
let desugar_constr env sigma trm =
  let rec aux env (sigma, trm) =
    map_term_env_if
      (fun _ sigma _ trm -> sigma, isFix trm || isCoFix trm || isCase trm)
      (fun env sigma _ trm ->
        match kind trm with
        | Fix (([|fix_pos|], 0), ([|fix_name|], [|fix_type|], [|fix_term|])) ->
           aux
             env
             (desugar_fixpoint env sigma fix_pos fix_name fix_type fix_term)
        | Fix _ ->
           user_err ~hdr:"desugar" (Pp.str "mutual recursion not supported")
        | CoFix _ ->
           user_err ~hdr:"desugar" (Pp.str "co-recursion not supported")
        | Case (info, pred, discr, cases) ->
           aux
             env
             (desugar_match env sigma info pred discr cases)
        | _ ->
           failwith "(Theoretically) impossible state in desugar_constr")
      (fun _ -> ())
      env
      sigma
      ()
      trm
  in
  let sigma, trm' = aux env (sigma, trm) in
  let _ = Typing.e_type_of env (ref sigma) (EConstr.of_constr trm') in
  sigma, trm'
