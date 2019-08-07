open Environ
open Constr
open Evd

(*
 * Translate the given term into an equivalent, bisimulative (i.e., homomorpic
 * reduction behavior) version using eliminators instead of match or fix
 * expressions.
 *
 * Mutual recursion and co-recursion are not supported.
 *)
val desugar_constr : Transform.constr_transformer
