Require Import Fixtranslation.Fixtoelim.
Require List.

(*
 * Test whole module preprocessing to convert fixpoints to induction principles.
 * By Nate Yazdani with later additions by Talia Ringer.
 *)

(*
 * This now recursively goes in and preprocesses dependent modules, ignoring
 * only the terms we tell it to ignore. Here, we document why we choose to ignore
 * certain terms we don't support yet.
 *)
Preprocess Module List as List' { opaque (* ignore these: *)
  (* dependent elimination only: *)
  RelationClasses.StrictOrder_Transitive
  RelationClasses.StrictOrder_Irreflexive
  RelationClasses.Equivalence_Symmetric
  RelationClasses.Equivalence_Transitive
  RelationClasses.PER_Symmetric
  RelationClasses.PER_Transitive
  RelationClasses.Equivalence_Reflexive
  
}.
Print List'.
