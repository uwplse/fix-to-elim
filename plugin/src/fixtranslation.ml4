DECLARE PLUGIN "fixtoelim"

open Stdarg
open Desugar
open Names
open Transform
open Nameutils
open Substitution
open Declarations

module Globmap = Globnames.Refmap

(* TODO move, check *)
let env_for_const const_body =
  let env =
    match const_body.const_universes with
    | Monomorphic_const univs ->
       Global.env () |> Environ.push_context_set univs
    | Polymorphic_const univs ->
       CErrors.user_err
         ~hdr:"transform_constant"
         Pp.(str "Universe polymorphism is not supported")
  in env, Evd.from_env env
                   
(*
 * Translate each fix or match subterm into an equivalent application of an
 * eliminator, defining the new term with the given name.
 *
 * Mutual fix or cofix subterms are not supported.
 * (By Nate Yazdani, from DEVOID)
 *)
let do_desugar_constant ident const_ref =
  let const_id = qualid_of_reference const_ref in
  let located = Nametab.locate_constant const_id in
  let const_body = Global.lookup_constant located in
  let env, sigma = env_for_const const_body in
  ignore (transform_constant env sigma ident desugar_constr const_body)

(*
 * Translate fix and match expressions into eliminations, as in
 * do_desugar_constant, compositionally throughout a whole module.
 *
 * The optional argument is a list of constants outside the module to include
 * in the translated module as if they were components in the input module.
 * (By Nate Yazdani, from DEVOID)
 *)
let do_desugar_module ?(incl=[]) ident mod_ref =
  let open Util in
  let consts = List.map (qualid_of_reference %> Nametab.locate_constant) incl in
  let include_constant subst const =
    let ident = Label.to_id (Constant.label const) in
    let tr_constr env evm = subst_globals subst %> desugar_constr env evm in
    let const_body = Global.lookup_constant const in
    let env, sigma = env_for_const const_body in
    let const' = transform_constant env sigma ident tr_constr const_body in
    Globmap.add (ConstRef const) (ConstRef const') subst
  in
  let init () = List.fold_left include_constant Globmap.empty consts in
  let mod_qualid = qualid_of_reference mod_ref in
  let located = Nametab.locate_module mod_qualid in
  let mod_body = Global.lookup_module located in
  let univs = mod_body.mod_constraints in
  let env = Global.env () |> Environ.push_context_set univs in
  let sigma = Evd.from_env env in
  ignore (transform_module_structure ~init env sigma ident desugar_constr)

(* --- Commands --- *)

(* Desugar any/all fix/match subterms into eliminator applications *)
VERNAC COMMAND EXTEND TranslateMatch CLASSIFIED AS SIDEFF
| [ "Preprocess" reference(const_ref) "as" ident(id) ] ->
  [ do_desugar_constant id const_ref ]
| [ "Preprocess" "Module" reference(mod_ref) "as" ident(id) ] ->
  [ do_desugar_module id mod_ref ]
| [ "Preprocess" "Module" reference(mod_ref) "as" ident(id) "{" "include" ne_reference_list_sep(incl_refs, ",") "}" ] ->
  [ do_desugar_module ~incl:incl_refs id mod_ref ]
END
