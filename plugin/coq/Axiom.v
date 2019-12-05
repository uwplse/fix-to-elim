Require Import Fixtranslation.Fixtoelim.

Module HasAxiom.

Axiom A : True.

End HasAxiom.

Preprocess Module HasAxiom as HasAxiom' {opaque HasAxiom.A}.