open Constr
open Apputils

(* 
 * Replace all occurrences of the first term in the second term with Rel 1,
 * lifting de Bruijn indices as needed. The notion of term equality is modulo
 * alpha, casts, application grouping, and universes.
 *
 * By Nate Yazdani, from DEVOID.
 *)
let abstract_subterm sub term =
  (* Allocate a binding slot for the abstracted subterm *)
  let sub = Vars.lift 1 sub in
  let term = Vars.lift 1 term in
  let rec surgery (nb, sub) term =
    match eq_constr_head sub term with
    | Some args ->
      mkApp (mkRel (nb + 1), args)
    | None ->
      Constr.map_with_binders
        (fun (nb, sub) -> nb + 1, Vars.lift 1 sub)
        surgery
        (nb, sub)
        term
  in surgery (0, sub) term
