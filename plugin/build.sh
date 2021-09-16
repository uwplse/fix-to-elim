#!/usr/bin/env bash
git submodule init
git submodule update
opam pin coq-serapi 8.9.0+0.6.1
opam pin dune 2.7.1
dune clean
dune build @all
dune install

