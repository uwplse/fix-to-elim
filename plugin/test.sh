#!/usr/bin/env bash
coqc coq/Preprocess.v
coqc coq/PreprocessModule.v
coqc coq/Type.v
coqc coq/Axiom.v
coqc coq/Alias.v
coqc coq/BinInt.v

