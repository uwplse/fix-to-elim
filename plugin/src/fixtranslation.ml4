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

module Globmap = Globnames.Refmap
module Globset = Globnames.Refset
        
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
 * Translate fix and match expressions into eliminations, as in
 * do_desugar_constant, compositionally throughout a whole module.
 *
 * The optional argument is a list of constants outside the module to include
 * in the translated module as if they were components in the input module.
 * (By Nate Yazdani, from DEVOID)
 *)
let do_desugar_module ?(opaques=[]) ident mod_ref =
  let env = Global.env () in
  let m = lookup_module (locate_module (qualid_of_reference mod_ref)) in
  let m_path = m.mod_mp in
  let m_type = m.mod_type in
  match m_type with
  | MoreFunctor _ ->
     CErrors.user_err (Pp.str "Preprocessing functors is not yet supported")
  | NoFunctor m_fields ->
     let m_consts =
       List.map
         (fun (l, _) -> mkConst (Constant.make2 m_path l))
         m_fields
     in
     let opaques =
       List.fold_left
         (fun s r ->
           let c = ConstRef (locate_constant (qualid_of_reference r)) in
           Globset.add c s)
         Globset.empty
         opaques
     in
     (* recursively get all constants *)
     let m_crefs = List.map (fun c -> ConstRef (fst (destConst c))) m_consts in
     let seen = ref (List.fold_right Globset.add m_crefs Globset.empty) in
     let rec get_all_consts consts =
       match consts with
       | h :: tl ->
          let h_consts =
            all_const_subterms
              (fun _ t ->
                let c = ConstRef (fst (destConst t)) in
                if Globset.mem c (!seen) then
                  false
                else
                  let s = !seen in
                  seen := Globset.add c s; 
                  true)
              (fun _ -> ())
              ()
              (unwrap_definition env h)
          in
          let h_consts_rec = get_all_consts h_consts in
          List.append h_consts_rec (List.append h_consts (get_all_consts tl))
       | _ ->
          [] 
     in
     let include_constant subst trm =
       let glob = Globnames.global_of_constr trm in
       let glob_ident = Nametab.basename_of_global glob in
       let dirpath = Nametab.dirpath_of_global glob in
       let prefixes = String.split_on_char '.' (DirPath.to_string dirpath) in
       let suffix = Id.to_string glob_ident in
       let ident = Id.of_string (String.concat "_" (snoc suffix prefixes)) in
       let const = fst (destConst trm) in
       let tr_constr env sigma = subst_globals subst %> desugar_constr env sigma in
       let c = lookup_constant const in
       if Globset.mem (ConstRef const) opaques then
         subst
       else
         let _, const' = transform_constant ident tr_constr c in
         Globmap.add (ConstRef const) (ConstRef const') subst
     in
     let consts = List.rev (get_all_consts m_consts) in
     let open Printing in
     debug_terms env consts "consts";
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
