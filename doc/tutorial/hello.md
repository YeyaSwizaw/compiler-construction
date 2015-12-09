Let's write a hello world program!

This tutorial should teach you about IO and looping, which in addition to basic maths is sufficient to write many programs.

# Basics
So, firstly, there is no concept of strings in this language. In fact, there is barely a concept of characters.
Characters are stored on the stack as integers, and strings are simply a pile of characters one-by-one on the stack, usually with
some item (usually 0 like null terminated strings in C) to indicate the string is finished.

String literals in this language are simply syntatic sugar for pushing each character to the stack one-by-one, in reverse.

The program:
```
0 "Hello\n"
```

results in the stack:
```
<bottom> 0 10 111 108 108 101 72 <top>
```

# Printing
The first thing to do is to write a loop that prints a string from the top of the stack, or more specifically, a loop that
pops a character off the stack and prints it, until it pops a 0.

In a C-style language, this could be written as something like:
```
print() {
  let c = pop();
  if(c != 0) {
    write(c);
    print();
  }
}
```

This is written recursively, as it is how it will be written in this language.

The important thing to remember about functions in this language is that they do things to the stack. 
A 'function' is a transformation of the top of the stack. In this case, we want a function that continually prints the top
of the stack, so it will be a function that operates on one element, uses that element, and then calls itself to operate
on the next element on the stack - therefore the outer shell of the C-style function can be replaced with:
```
print: c -> {
  # if c != 0 (write(c); print())
}
```

When this function is called, it pops one value off the stack and assigns it the name 'c' which can then be used like any
other value inside this function.

Now we need to write the if statement. If, in this language, is a function like any other function. Instead of being control
flow like in C, it is a transformation of the stack. It pops three values, and pushes the second if the first is a true value,
and pushes the third otherwise. In order to make it function like control flow, the values it pushes will be functions. If the
character is not 0, it should print it and then recurse, and if it is not 0 it should return. Because functions operate on the
stack, we need to push c back onto the stack so these functions can operate on it, so this is the first thing our print function
will do.

```
print: c -> {
  c # push c to the stack to be operated on
  # TODO: <some if statement returning a function>
  () # apply the result of the if statement
}
```

We can also fill in the if operator:

```
print: c -> {
  c # push c to the stack to be operated on
  # TODO: function if false
  # TODO: function if true
  # TODO: boolean comparison 
  ? () # push and apply the if operator
  () # apply the result of the if operator
}
```

Writing the comparison is easy. The equals operator pops two values off the stack when applied, 
then pushes 1 if they are equal, and 0 otherwise.
```
print: c -> {
  c # push c to the stack to be operated on
  # TODO: function if false
  # TODO: function if true
  0 c = () # boolean comparison
  ? () # push and apply the if operator
  () # apply the result of the if operator
}
```

Now for the functions. If c is 0, then we want to simply remove it from the stack and return.
This is implemented simply by a function that takes one value off the stack and does nothing else.
```
print: c -> {
  c # push c to the stack to be operated on
  # TODO: function if false
  c -> {} # function if true - do nothing and return
  0 c = () # boolean comparison
  ? () # push and apply the if operator
  () # apply the result of the if operator
}
```

If c is non-zero, we want to print it out, and then recurse to repeat this function on the next item on the stack:
```
print: c -> {
  c # push c to the stack to be operated on
  c -> { # function if true - print and repead
    # TODO: somehow output c
    print () # recurse
  }
  c -> {} # function if true - do nothing and return
  0 c = () # boolean comparison
  ? () # push and apply the if operator
  () # apply the result of the if operator
}
```

Character output is performed by the comma operator, which (along with integer output) is a special exception. 
It is not a function, it does not pop the value off the stack and print it out, it simply magically prints the top of the
stack, leaving it there. This means that this function needs to push c onto the stack, apply this operator, and then 
manually pop it off. Manual popping is something we have already done - it is a function that takes one item, and does nothing
to it.
```
print: c -> {
  c # push c to the stack to be operated on
  c -> { # function if true - print and repead
    c , # push c and output it
    c -> {} () # pop c
    print () # recurse
  }
  c -> {} # function if true - do nothing and return
  0 c = () # boolean comparison
  ? () # push and apply the if operator
  () # apply the result of the if operator
}
```

And there we have it, a complete function to print a string from the stack! Now we just need to use it.

# Hello World
To use it, we push a null terminator to the stack, push a string to the stack, and then push and apply this function.
The final hello world program is therefore:
```
print: c -> {
  c # push c to the stack to be operated on
  c -> { # function if true - print and repead
    c , # push c and output it
    c -> {} () # pop c
    print () # recurse
  }
  c -> {} # function if true - do nothing and return
  0 c = () # boolean comparison
  ? () # push and apply the if operator
  () # apply the result of the if operator
}

0 "Hello World!\n" print ()
```
