Require Import Fixtranslation.Fixtoelim.
Require List.

(*
 * This now recursively goes in and preprocesses dependent modules, ignoring
 * only the terms we tell it to ignore. Here, we document why we choose to ignore
 * certain terms we don't support yet.
 *)
Preprocess Module List as List' { opaque (* ignore these: *)
  (* dependent elimination: *)
  RelationClasses
  (* proofs about these match over the above opaque terms, and would fail: *)
  Nat.add
  Nat.sub
}.
Print List'.
