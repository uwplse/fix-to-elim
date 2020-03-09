Require Import Fixtranslation.Fixtoelim.
Require List.

Set Preprocess default opaque. (* <-- ignore all dependencies by default *)

(*
 * This now recursively goes in and preprocesses dependent modules, ignoring
 * only the terms we tell it to ignore. Here, we document why we choose to ignore
 * certain terms we don't support yet.
 *)
Preprocess Module List as List' { transparent
  (* list append and induction *)
  Coq.Init.Datatypes
}.
Print List'.
