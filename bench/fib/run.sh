#!/bin/bash

echo "SFL:"
../../sfl.native fib.sfl
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
echo "Java:"
javac fib.java
time java fib

echo
echo "C:"
cc fib.c
time ./a.out

echo
echo "C (-O3):"
cc fib.c -O3
time ./a.out

echo
echo "Rust:"
rustc fib.rs -o a.out
time ./a.out

echo
echo "Rust (-O):"
rustc fib.rs -o a.out -O
time ./a.out

rm a.out
rm fib.class
rm fib.native
rm -rf _build
