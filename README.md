This is a compiler for a functional stack-based programming language, 
implemented in OCaml for my third year Compiler Construction module.

## Compilation

There are two dependancies, which can easily be installed using opam:

```
# opam install core
# opam install ansiterminal
```

A Makefile is provided for easy compilation of both the compiler and the tests:

```
# make
```

To run the tests, run ```./test.native```, and to run the program, run ```./sfl.native <filename>```. 
When compiling, it will complain about reduce/reduce conflicts - these are only involved in error handling, 
and are not actually a problem.

## Language

As mentioned, the language is both stack-based and functional 
(or at least, functions are first class and can be be partially applied). 

Let's start with a very simple program:

```
25 17 + ()
```

This program consists of 4 expressions, which are executed in order.
```25``` and ```17``` push their respective integer values to the stack,
```+``` is a built in operator that pushes the addition function to the stack, and
```()``` is a built in operator that pops a function of the stack, and then pops and applies arguments 
until it is fully applied, in this case pushing 42 to the stack.

Arbitrary transformations of the stack can be performed by defining your own function, 
for example, here is a function that swaps the top of the stack

```
a -> b -> {
  a b
}
```

This pushed the defined function to the stack for it to be applied in the same way as any built in functions.

Finally, named values can be created using the following syntax:

```
swap: a -> b -> {
  a b
}
```

This can also be used with non-function values, however it is most useful for defining reusable functions.

Runnable examples can be found in ```doc/examples``` in the repository.

## Reference
Values can be integers, "strings" 'chars' or functions.

The built in operators are as follows 
* Basic Maths: ```+```, ```-```, ```*```, ```/```
* Comparisons: ```>```, ```<```, ```=```
* Control Flow: ```?: If -> Then -> Else -> { ... }```
