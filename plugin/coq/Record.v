Require Import Fixtranslation.Fixtoelim.

(*
 * Test that Preprocess Module preserves records.
 *)

Module Foo.

Record r := {
  p1 : nat;
  p2 : nat
}.

End Foo.

Preprocess Module Foo as Bar.
Preprocess Module Bar as Baz.