This is a compiler for a functional stack-based programming language, 
implemented in OCaml for my third year Compiler Construction module.

## Compilation

The compiler has two dependencies, which can be easily installed with opam:

```
# opam install core
# opam install ansiterminal
```

A Makefile is provided for easy compilation of everything

```
# make
```

This will compiler the target and the tests, and then run the tests

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
This means any fully pure operation (such as the examples in ```doc/examples``` will compile down to the pushing of a single
value onto the stack.

There are two main parts of this optimisation:
* Constant folding, which executes simple expressions of the form ```x y + ()``` where ```x``` and ```y``` are constant values.
  This optimisation can be disabled with the flag ```--no-constant-folding```

* Function execution, which inlines and executes user-defined functions with known arguments.
  As an example, ```3 x -> { x } ()``` would compile to just ```3```.
  With large recursive operations, this can take a very long time, so it is recommended to disable this in those cases.
  This optimisation can be disabled with the flag ```--no-function-execution```

## Flags
* ```--no-constant-folding``` disables the constant folding optimisation.

* ```--no-function-execution``` disables the function inlining and executing optimisation.

* ```--no-storage-cleaning``` disables the cleaning of storage memory when it is accessed. This will likely fix
some bugs in compiled code, but will result in much more memory usage.

* ```--stack-size <int>``` sets the size of the initial stack.

* ```--storage-size <int>``` sets the size of the initial storage stack.

## Reference
Values can be integers, "strings" 'chars' or functions.

The built in operators are as follows 
* Basic Maths: ```+```, ```-```, ```*```, ```/```
* Comparisons: ```>```, ```<```, ```=```
* Control Flow: ```?: If -> Then -> Else -> { ... }```
