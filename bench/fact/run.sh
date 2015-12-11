#!/bin/bash

echo "SFL:"
../../sfl.native fact.sfl
time ./a.out > /dev/null

echo
echo "Python:"
time python fact.py > /dev/null

echo
echo "OCaml:"
time ocaml fact.ml > /dev/null

echo
echo "Compiled OCaml:"
ocamlbuild fact.native
time ./fact.native > /dev/null

echo
echo "Java:"
javac fact.java
time java fact > /dev/null

echo
echo "C:"
cc fact.c
time ./a.out > /dev/null

echo
echo "C (-O3):"
cc fact.c -O3
time ./a.out > /dev/null

echo
echo "Rust:"
rustc fact.rs -o a.out
time ./a.out > /dev/null

echo
echo "Rust (-O):"
rustc fact.rs -o a.out -O
time ./a.out > /dev/null

rm a.out
rm fact.class
rm fact.native
rm -rf _build
