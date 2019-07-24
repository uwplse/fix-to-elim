#!/usr/bin/env bash
coqc coq/Preprocess.v
coqc coq/PreprocessModule.v
echo "All tests passed."
