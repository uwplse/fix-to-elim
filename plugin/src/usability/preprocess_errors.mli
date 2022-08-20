open CErrors
open Environ
open Evd

(*
 * Error messages
 *)

(* --- Error descriptions --- *)

val err_mutual_recursion : Pp.t
val err_corecursion : Pp.t
val err_functor : Pp.t
val err_type : env -> evar_map -> Pretype_errors.pretype_error -> Pp.t
val err_opaque_not_constant : Libnames.qualid -> Pp.t

(* --- Possible workaround suggestions --- *)

val try_opaque : Pp.t
val try_change_ind : Pp.t
val try_defs_only : Pp.t
val try_check_typos : Pp.t
val try_fully_qualify : Pp.t
val try_alias : Pp.t

(* --- Reasons to cut an issue --- *)

val cool_feature : Pp.t
val problematic : Pp.t

(* --- Putting these together --- *)
    
(*
 * Our own user_err function to make it easier to present nice information
 * to the user
 *)
val user_err :
  String.t -> (* where you're calling it from *)
  Pp.t -> (* error description *)
  Pp.t list -> (* workaround suggestions *)
  Pp.t list -> (* reasons to cut an issue *)
  'a
       
