From Coq Require Import ZArith.BinInt.
Require Import Fixtranslation.Fixtoelim.

Scheme Minimality for Coq.Classes.RelationClasses.StrictOrder Sort Prop.
Scheme Induction for Coq.Classes.RelationClasses.StrictOrder Sort Set.
Scheme Induction for Coq.Classes.RelationClasses.StrictOrder Sort Type.

Scheme Minimality for Coq.Classes.RelationClasses.Equivalence Sort Prop.

Scheme Minimality for Coq.Classes.RelationClasses.PER Sort Prop.

Preprocess Module Coq.ZArith.BinInt as BinInt'
  { opaque
      Coq.Init.Logic.eq_ind_r
      Coq.Init.Logic.eq_ind
      Coq.Init.Logic.eq_sym
      Coq.ZArith.BinInt.Z.add
      Coq.ZArith.BinInt.Z.mul
  }.
