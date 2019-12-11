Require Import String.
Require Import Vectors.Vector.
Require Import Fixtranslation.Fixtoelim.

(*
 * Test for type aliases.
 *)

Module V.

  Definition Vec (n : nat) (a : Type) : Type := VectorDef.t a n.

  Fixpoint atWithDefault (n : nat) (a : Type) (default : a) (v : Vec n a) (index : nat) : a.
    refine (
        match v with
        | Vector.nil _ => default
        | Vector.cons _ h n' t =>
          match index with
          | O => h
          | S index' => atWithDefault n' _ default t index'
          end
        end
      ).
  Defined.

End V.

Preprocess Module V as V'.