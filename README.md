This is a compiler for a functional stack-based programming language, 
implemented in OCaml for my third year Compiler Construction module.

## Compilation

The compiler depends on LLVM (tested on 3.7), which should be installed using
your system package manager of choice.

It then has three OCaml dependecies, which can be easily installed with opam:

```
# opam install core
# opam install ansiterminal
# opam install llvm
```

A Makefile is provided for easy compilation of everything

```
# make
```

This will compile the target and the tests, and then run the tests

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

Runnable examples can be found in ```doc/examples``` in the repository. Try changing the numbers and seeing
how quickly things break!

## Optimisations
The compiler takes advantage of aggresive optimisation, and will fully execute all functions and operations with known values.
This means any fully pure operation will compile down to the pushing of a single value onto the stack. In addition, any output is also folded.

It works primarily as two parts. Constant folding, which attempts to inline or execute all simple mathematical expressions, and
function inlining, which attempts to do the same thing but to functions defined in the language. Unfortunately, function
inlining is currently broken when recursion is involved, so this optimisation is disabled unless the flag ```--function-inlining``` is provided.

In addition, the compiler tries as hard as possible to compile the language in such a way that a stack (other than
the standard assembly stack) is not actually needed, which dramatically increases the efficiency of the language.
The process for this is described in ```doc/compiler-machine.md```

## Flags
* ```--output | -o``` Set the filename of the compiled binary.
* ```--emit-{ast,fns,llvm}``` Output an internal stage to stdout instead of compiling a binary.
* ```--function-inlining``` Enables function inlining - DO NOT enable if your program contains any recursion.

## Reference
Values can be integers, "strings" 'chars' or functions.

The built in operators are as follows 
* Basic Maths: ```+```, ```-```, ```*```, ```/```
* Comparisons: ```>```, ```<```, ```=```
* Control Flow: ```?: If -> Then -> Else -> { ... }```
* IO: ```,```, ```.```, ```~``` (output as character, output as integer, input character)
