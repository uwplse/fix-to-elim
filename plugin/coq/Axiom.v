Require Import Fixtranslation.Fixtoelim.

Module HasAxiom.

Axiom A : True.

End HasAxiom.

Module HasAxiom'.

Definition f := HasAxiom.A.

End HasAxiom'.

Preprocess Module HasAxiom' as HasAxiom'' {opaque HasAxiom.A}.