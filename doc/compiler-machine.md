#Input:
```
code: [Statement]
env: {String x Value}
```

#Machine:
```
<C: [Statement] | E: {String x Value} | T: bool | S: int | I: [Instruction] | V: [(Type, Value)] | K: [State] | R: {String x int x bool x [Instruction]}>
```

#Initial State:
```
<code | env | false | 0 | [] | [] | [] | {}>
```

#Rules:
##Values:
```
<Int(i) :: C | E | T | S | I | V | K | R> 
	=> <C | E | T | S | I | (Int, i) :: V | K | R>
	
<Char(c) :: C | E | T | S | I | V | K | R> 
	=> <C | E | T | S | I | (Int, c) :: V | K | R>
	
<String(c_0 ... c_n) :: C | E | T | S | I | V | K | R> 
	=> <C | E | T | S | I | (Int, c_0) :: ... :: (Int, c_n) :: V | K | R>
	
<Arg(i) :: C | E | T | S | I | V | K | R>
	=> <C | E | T | S | I | (-, Arg(i)) :: V | K | R>
```

##Name Lookup:
```
<Name(i) :: C | (i, v) ∈ E | T | S | I | V | K | R> 
	=> <v :: C | E | T | S | I | V | K | R>
```

##Operators:
```
<BinOp(op) :: C | E | T | S | I | V | K | R> 
	=> <C | E | T | S | I | (Fn(2, 1, false), op) :: V | K | R>
	
<TriOp(op) :: C | E | T | S | I | V | K | R> 
	=> <C | E | T | S | I | (Fn(3, 1, false), op) :: V | K | R>
```

##Output:
```
<Write :: C | E | T | S | I | (U, v) :: V | K | R>
	=> <C | E | T | S + 1 | Write(Stored(S)) :: Store(v) :: I | (U, Stored(S)) :: V | K | R>
	
<Write :: C | E | T | S | I | [] | K | R>
	=> <C | E | true | S + 1 | Write(Stored(S)) :: Store(Pop) :: I | (U, Stored(S)) :: V | K | R>
```

##Function Pushing:
```
<Fn(f, [a_1, ..., a_n], _) :: C | E | T | S | I | V | K | (f, FN, FT, _) ∈ R>
	=> <C | E | T | S | I | (Fn(n, FN, FT), f) :: V | K | R>
	
<Fn(f, [a_1, ..., a_n], FC) :: C | E | T | S | I | V | K | R>
	=> <FC, {(a_1, Arg(1), ..., (a_n, Arg(n))} ∪ E | T | S | [] | [] | <Fn(f, [a_1, ..., a_n], FC) :: C | T | E | I | V> :: K | R>
```

##Operator Application:
```
<Apply :: C | E | T | S | I | (_, +) :: (_, x) :: (_, y) :: V | K | R>
	=> <C | E | T | S | I | (Int, x + y) :: V | K | R>
	
<Apply :: C | E | T | S | I | (_, -) :: (_, x) :: (_, y) :: V | K | R>
	=> <C | E | T | S | I | (Int, x - y) :: V | K | R>
	
<Apply :: C | E | T | S | I | (_, *) :: (_, x) :: (_, y) :: V | K | R>
	=> <C | E | T | S | I | (Int, x * y) :: V | K | R>

<Apply :: C | E | T | S | I | (_, /) :: (_, x) :: (_, y) :: V | K | R>
	=> <C | E | T | S | I | (Int, x / y) :: V | K | R>

<Apply :: C | E | T | S | I | (_, =) :: (_, x) :: (_, y) :: V | K | R>
	=> <C | E | T | S | I | (Int, x = y ? 1 : 0) :: V | K | R>

<Apply :: C | E | T | S | I | (_, <) :: (_, x) :: (_, y) :: V | K | R>
	=> <C | E | T | S | I | (Int, x < y ? 1 : 0) :: V | K | R>

<Apply :: C | E | T | S | I | (_, >) :: (_, x) :: (_, y) :: V | K | R>
	=> <C | E | T | S | I | (Int, x > y ? 1 : 0) :: V | K | R>
	
<Apply :: C | E | T | S | I | (_, ?) :: (_, b) :: (T, x) :: (U, y) :: V | K | R>
	=> <C | E | T | S | I | (T == U ? T : -, ?(b, x, y)) :: V | K | R>  
```

##Function Application:
```
<Apply :: C | E | T | S | I | (Fn(n, m, false), f) :: (_, a_1) :: ... :: (_, a_n) :: V | K | R> 
	=> <C | E | T | S + m | Apply(f, [a_1 ... a_n]) :: I | (-, Stored(S + 0)) :: ... :: (-, Stored(S + m - 1)) :: V | K | R>

<Apply :: C | E | T | S | I | (Fn(n, m, true), f) :: (_, a_1) :: ... :: (_, a_n) :: (_, v_0) :: ... :: (_, v_m) :: [] | K | R> 
	=> <C | E | true | S + m | Apply(f, [a_1 ... a_n]) :: Push(v_0) :: ... :: Push(v_m) :: I | (-, Stored(S + 0)) :: ... :: (-, Stored(S + m - 1)) :: [] | K | R>
```

##Stack Arguments:
```
<Apply :: C | E | T | S | I | (Fn(n, _), f) :: (_, a_1) :: ... :: (_, a_(m < n)) :: [] | K | R>
	=> <C | E | true | S | I | (Fn(n, _), f) :: (_, a_1) :: ... :: (_, a_(m < n)) :: (-, Pop) :: [] | K | R>
```

##Function Return:
```
<[] | E | T | S | I | [v_1, ..., v_n] | <Fn(f, [a_1, ..., a_m], _) :: C | KT | KE | KI | V> :: K | R>
	=> <C | KE | KT | S | KI | (Fn(m, n, T), f) :: V | K | {(f, n, T, Return([v_1, ..., v_n]) :: I)} ∪ R>
```

##Accepting State:
```
<[] | E | T | S | I | V | [] | R> 
```

The main function instructions are stored in the stack ```I```, the rest of the functions are in the set ```R```.

#Example:
Given the progam:
```
add: y -> {
	y + ()
}

2 8 add () ,
```

Here is the initial state of the machine:
```
<2 8 add () , | (add, ..) | false | 0 | [] | [] | [] | {}>
```


First, we push the two integers to the value stack:
```
<8 add () , | (add, ..) | false | 0 | [] | (Int 2) | [] | {}>
<add () , | (add, ..) | false | 0 | [] | (Int 8) (Int 2) | [] | {}>
```

Then, we lookup add in the environment:
```
<Fn(add, [y], ..) () , | (add, ..) | false | 0 | [] | (Int 8) (Int 2) | [] | {}>
```

We save the state on the continuation stack, and evaluate the function:
```
<y + () | (y, Arg(0)) (add, ..) | false | 0 | [] | [] | <Fn(add, [y], ..) () , | false | (add, ..) | [] | (Int 8) (Int 2)> | {}>
```

Lookup y in the environment:
```
<Arg(0) + () | (y, Arg(0)) (add, ..) | false | 0 | [] | [] | <Fn(add, [y], ..) () , | false | (add, ..) | [] | (Int 8) (Int 2)> | {}>
```

Push the argument to the value stack:
```
<+ () | (y, Arg(0)) (add, ..) | false | 0 | [] | (-, Arg(0)) | <Fn(add, [y], ..) () , | false | (add, ..) | [] | (Int 8) (Int 2)> | {}>
```

Push the addition operator to the value stack:
```
<() | (y, Arg(0)) (add, ..) | false | 0 | [] | Fn((2, 1, false), +) (-, Arg(0)) | <Fn(add, [y], ..) () , | false | (add, ..) | [] | (Int 8) (Int 2)> | {}>
```

Add a pop argument to the value stack - this function in now tainted:
```
<() | (y, Arg(0)) (add, ..) | true | 0 | [] | Fn((2, 1, false), +) (-, Arg(0)) (-, Pop) | <Fn(add, [y], ..) () , | false | (add, ..) | [] | (Int 8) (Int 2)> | {}>
```

Apply the addition function:
```
< | (y, Arg(0)) (add, ..) | true | 0 | [] | (Int, Arg(0) + Pop) | <Fn(add, [y], ..) () , | false | (add, ..) | [] | (Int 8) (Int 2)> | {}>
```

Pop the top of the continuation stack, and insert the function into the result set:
```
<() , | (add, ..) | false | 0 | [] | (Fn(1, 1, true), add) (Int 8) (Int 2) | [] | {(add, 2, true, Return([Arg(0) + Pop]))}>
```

Apply the function, pushing values to the stack as it is tainted:
```
<, | (add, ..) | true | 1 | Apply(add, [8]) Push(2) | (-, Stored(0)) | [] | {(add, 2, true, Return([Arg(0) + Pop]))}>
```

Apply the write instruction:
```
< | (add, ..) | true | 2 | Write(Stored(1)) Store(Stored(0)) Apply(add, [8]) Push(2) | (-, Stored(1)) | [] | | {(add, 2, true, Return([Arg(0) + Pop]))}>
```

Done! The resulting instructions are as follows:
```
Push(2); Apply(add, [8]); Store(Stored(0)); Write(Stored(1));
add: Return([Arg(0) + Pop]);
```

Translated into a C like language, this would be:
```
main() {
	push(2);
	val = add(8);
	write(val);
}

add(x) {
	return [pop() + x];
}
```
