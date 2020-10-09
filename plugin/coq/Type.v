Require Import Fixtranslation.Fixtoelim.

(*
 * Regression test for the bug Val reported
 *)

Definition sort (n : nat) := Type.

Preprocess sort as sort'.

Module SCS.

  Definition sort_typ := nat -> Type.

  Definition sort := (fun (n : nat) => Type) : sort_typ.

End SCS.

Preprocess Module SCS as SCS'.
