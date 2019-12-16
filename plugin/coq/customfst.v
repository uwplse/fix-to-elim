Require Import String.
Require Import Vectors.Vector.
Require Import Fixtranslation.Fixtoelim.

Set Nonrecursive Elimination Schemes.

(* When these two lines are commented out, the lifting of [get_record_type]
works! *)
Definition fst {A B} := @fst A B.
Definition snd {A B} := @snd A B.

Definition Vec n t := Vector.t t n.
Definition seq : nat -> Type -> Type := Vec.

Module HandshakeAction.

  Definition cry_handshake_action : Type
    := (seq 8 bool * (seq 8 bool * seq 8 bool)).

  Record HandshakeAction := MkHandshakeAction
                            {
                            recordType  : seq 8 bool;
                            messageType : seq 8 bool;
                            writer      : seq 8 bool;
                            }.

  Definition get_record_type (ha : cry_handshake_action) : seq 8 bool :=
    fst ha.

  Definition get_message_type (ha : cry_handshake_action) : seq 8 bool :=
    fst (snd ha).

  Definition get_writer (ha : cry_handshake_action) : seq 8 bool :=
    snd (snd ha).

End HandshakeAction.

Preprocess Module HandshakeAction
  as HandshakeAction'.
