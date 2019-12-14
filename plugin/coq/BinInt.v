From Coq Require Import ZArith.BinInt.
Require Import Fixtranslation.Fixtoelim.

Scheme Minimality for Coq.Classes.RelationClasses.StrictOrder Sort Prop.
Scheme Induction for Coq.Classes.RelationClasses.StrictOrder Sort Set.
Scheme Induction for Coq.Classes.RelationClasses.StrictOrder Sort Type.

Scheme Minimality for Coq.Classes.RelationClasses.Equivalence Sort Prop.

Scheme Minimality for Coq.Classes.RelationClasses.PER Sort Prop.

Preprocess Module Coq.ZArith.BinInt as BinInt'
  { opaque
      Coq.PArith.BinPos.Pos.add_reg_r 
      Coq.PArith.BinPos.Pos.add_no_neutral
      Coq.PArith.BinPos.Pos.compare_sub_mask  
      Coq.PArith.BinPos.Pos.sub_mask_carry_spec 
      Coq.PArith.BinPos.Pos.sub_mask_carry 
      Coq.PArith.BinPos.Pos.add_carry 
      Coq.PArith.BinPos.Pos.sub_mask 
      Coq.PArith.BinPos.Pos.peano_rect 
      Coq.PArith.BinPos.Pos.add
  }.