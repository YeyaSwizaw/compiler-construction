#!/bin/bash

echo "SFL:"
../../sfl.native ../../doc/examples/times.sfl
time ./a.out

echo
echo "Python:"
time python times.py

echo
echo "OCaml:"
time ocaml times.ml

echo
echo "Compiled OCaml:"
ocamlbuild times.native
time ./times.native

echo
echo "C:"
cc times.c
time ./a.out

echo
echo "Rust:"
rustc times.rs -o a.out
time ./a.out

rm a.out
rm times.native
rm -rf _build
