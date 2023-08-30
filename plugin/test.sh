#!/usr/bin/env bash
set -euxo pipefail
coqc coq/Preprocess.v
coqc coq/PreprocessModule.v
coqc coq/DefaultOpaque.v
coqc coq/Type.v
coqc coq/Axiom.v
coqc coq/Alias.v
coqc coq/BinInt.v
coqc coq/customfst.v
coqc coq/Record.v
