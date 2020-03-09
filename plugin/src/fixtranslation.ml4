DECLARE PLUGIN "fixtoelim"

open Stdarg
open Desugar
open Names
open Transform
open Nameutils
open Substitution
open Util
open Global
open Nametab
open Hofs
open Constr
open Utilities
open Envutils
open Declarations
open Preprocess_errors

module Globmap = Globnames.Refmap
module Globset = Globnames.Refset
module DPset = Set.Make(DirPath)

(*
 * Translate each fix or match subterm into an equivalent application of an
 * eliminator, defining the new term with the given name.
 *
 * Mutual fix or cofix subterms are not supported.
 * (By Nate Yazdani, from DEVOID)
 *)
let do_desugar_constant ident const_ref =
  ignore
    begin
      qualid_of_reference const_ref |> Nametab.locate_constant |>
      Global.lookup_constant |> transform_constant ident desugar_constr
    end

(*
 * Initialize the set of opaque constants and modules to ignore.
 *)
let initialize_opaque_set opaques =
  List.fold_left
    (fun (consts, mods) r ->
      let id = qualid_of_reference r in
      try
        Globset.add (ConstRef (locate_constant id)) consts, mods
      with Not_found ->
        try
          consts, DPset.add (ModPath.dp (locate_module id)) mods
        with
          Not_found ->
            user_err
              "initialize_opaque_set"
              (err_opaque_not_constant id)
              [try_check_typos; try_fully_qualify; try_alias]
              [cool_feature; problematic])
    (Globset.empty, DPset.empty)
    opaques
 
(*
 * Utility function for lists of terms
 *)
let append_dedupe l1 l2 =
  let l1_globs =
    List.fold_right
      (fun t -> Globset.add (ConstRef (fst (destConst t))))
      l1
      Globset.empty
  in
  List.append
    l1
    (List.filter
       (fun t -> not (Globset.mem (ConstRef (fst (destConst t))) l1_globs))
       l2)
    
(*
 * Get all constants a module refers to transitively.
 * Do not recurse into opaque modules (but do not yet filter out terms from
 * those modules).
 *)
let all_transitive_constants env m opaque_mods =
  let path = m.mod_mp in
  match m.mod_type with
  | MoreFunctor _ ->
     user_err
       "all_transitive_constants"
       err_functor
       [try_defs_only]
       [cool_feature; problematic]
  | NoFunctor fields ->
     let cs = List.map (fun (l, _) -> mkConst (Constant.make2 path l)) fields in
     let refs = List.map (fun c -> ConstRef (fst (destConst c))) cs in
     let in_module = List.fold_right Globset.add refs Globset.empty in
     let rec get_all_consts consts seen =
       match consts with
       | h :: tl ->
          let h_delta =
            try
              lookup_definition env h
            with _ ->
              h
          in
          let c = ConstRef (fst (destConst h)) in
          let seen = Globset.add c seen in
          let seen_hd = ref seen in
          let h_consts =
            let skip =
              try
                let dirpath = Nametab.dirpath_of_global c in
                DPset.mem dirpath opaque_mods
              with _ ->
                false
            in
            if skip then
              []  
            else
              all_const_subterms
                (fun _ t ->
                  let c = ConstRef (fst (destConst t)) in
                  if Globset.mem c (!seen_hd) then
                    false
                  else
                    let s = !seen_hd in
                    seen_hd := Globset.add c s;
                    true)
                (fun _ -> ())
                ()
                h_delta
          in
          append_dedupe
            (append_dedupe (get_all_consts h_consts seen) h_consts)
            (get_all_consts tl (!seen_hd))
       | _ ->
          [] 
     in get_all_consts cs in_module

(*
 * Translate fix and match expressions into eliminations, as in
 * do_desugar_constant, compositionally throughout a whole module.
 *
 * The optional argument is a list of constants to ignore in the translated
 * module. Otherwise, by default, this preprocesses all recursive subterms.
 * (By Nate Yazdani, from DEVOID, with later additions by Talia Ringer)
 *)
let do_desugar_module ?(opaques=[]) ident mod_ref =
  let m = lookup_module (locate_module (qualid_of_reference mod_ref)) in
  let env = Global.env () in
  let opaques, opaque_mods = initialize_opaque_set opaques in
  let include_constant subst trm =
    let glob = Globnames.global_of_constr trm in
    let glob_ident = Nametab.basename_of_global glob in
    let dp = Nametab.dirpath_of_global glob in
    let dirpath = DirPath.to_string dp in
    let prefixes = String.split_on_char '.' dirpath in
    let suffix = Id.to_string glob_ident in
    let ident = Id.of_string (String.concat "_" (snoc suffix prefixes)) in
    let const = fst (destConst trm) in
    let tr_constr env sigma = subst_globals subst %> desugar_constr env sigma in
    let c = lookup_constant const in
    if Globset.mem (ConstRef const) opaques || DPset.mem dp opaque_mods then
      subst
    else
      let _ = Feedback.msg_info (Pp.str (Printf.sprintf "Transforming dependency %s.%s" dirpath suffix)) in
      try
        let _, const' = transform_constant ident tr_constr c in
        Globmap.add (ConstRef const) (ConstRef const') subst
      with _ ->
        let _ = Feedback.msg_warning (Pp.str "Transformation failed, skipping dependency") in
        subst
  in
  let consts = all_transitive_constants env m opaque_mods in
  let init () = List.fold_left include_constant Globmap.empty consts in
  ignore (transform_module_structure ~init ~opaques ident desugar_constr m)

(* --- Commands --- *)

(* Desugar any/all fix/match subterms into eliminator applications *)
VERNAC COMMAND EXTEND TranslateMatch CLASSIFIED AS SIDEFF
| [ "Preprocess" reference(const_ref) "as" ident(id) ] ->
  [ do_desugar_constant id const_ref ]
| [ "Preprocess" "Module" reference(mod_ref) "as" ident(id) ] ->
  [ do_desugar_module id mod_ref ]
| [ "Preprocess" "Module" reference(mod_ref) "as" ident(id) "{" "opaque" ne_reference_list(opaq_refs) "}" ] ->
  [ do_desugar_module ~opaques:opaq_refs id mod_ref ]
END
