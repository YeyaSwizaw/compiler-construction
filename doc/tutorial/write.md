This tutorial builds on the hello world tutorial to make it friendlier, so if you haven't done that one I recommend
doing so.

# Input
Input, like output, is done on a character by character basis. This means we want to write a loop that continually reads
characters until a newline is read (which is when the user presses enter). We also want to ensure that these characters are 
pushed on the stack backwards, to make printing them correctly possible

Written imperatively, this function will be:
```
input() {
  c = read();
  if(c != '\n') {
    input();
  }
  push(c);
}
```

Functions in this langauge need to operate on the stack, so out input function needs to take at least one argument.
We could just make an unused parameter, however in this case we can parameterise it over the termination character.
This lets us, for example, split the input on spaces. For our use we will just read an entire line however.

The input operator, like the output operator, is not a function. In this case, it magically pushes a character to the stack
as soon as it is applied. To then use this value, we will want to call a function. The basic shell of our input loop is then:
```
input: t -> {
  t ~ # Push terminator, push input character
  c -> t -> { # Function to use character
    # TODO: if (c == t) { return } else { input(t) }
    # TODO: also push c
  }
  ()
}
```

The if usage will be very similar to the if in the print function we wrote. We just need to ensure we push the input character
to the stack after the recursion, so the string gets formed backwards. This implementation also pushes the final terminator
on with the string, which may or may not be what you want. By moving the pushing of c to the line after the recursive call,
this can be fixed. Try it - you'll need to do more than just move that line to ensure you can actually use c inside that
function.
```
input: t -> {
  t ~ # Push terminator, push input character
  c -> t -> { # Function to use character
    t # Push parameter for later
    t -> {
      t input () # Recursive call
    }
    t -> {} # Pop and return
    t c = () # Check character and terminator
    ? () # Push and apply if
    () # Apply result of if
    c # Push character to the stack
  }
  ()
}
```

And there, a complete loop for getting string input. Let's use it to make a friendly greeting.

# Hello Again!
Calling this input function results in a string on the stack, however the string is not null terminated. We need to ensure
a 0 is pushed to the stack before using it.

```
print: c -> {
  c
  c -> {
    c ,
    c -> {} ()
    print ()
  }
  c -> {}
  0 c = ()
  ? ()
  ()
}

input: t -> {
  t ~
  c -> t -> {
    t
    t -> {
      t input ()
    }
    t -> {}
    t c = ()
    ? ()
    ()
    c
  }
  ()
}

0 "What is your name? " print () # Print prompt
0 # Push 0 before asking for input
'\n' input () # Get input until newline
"Hello, " # Push greeting before name
print () # Print greeting
```
