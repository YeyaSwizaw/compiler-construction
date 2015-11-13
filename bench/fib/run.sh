#!/bin/bash

echo "SFL:"
../../sfl.native ../../doc/examples/fib.sfl
time ./a.out

echo
echo "Python:"
time python fib.py

echo
echo "OCaml:"
time ocaml fib.ml

echo
echo "Compiled OCaml:"
ocamlbuild fib.native
time ./fib.native

echo
echo "C:"
cc fib.c
time ./a.out

echo
echo "Rust:"
rustc fib.rs -o a.out
time ./a.out

rm a.out
rm fib.native
rm -rf _build
