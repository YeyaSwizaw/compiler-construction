#Input:
```
code: [Statement]
env: {String x Value}
```

#Machine:
```
<C: [Statement] | E: {String x Value} | T: bool | A: bool | P: bool | S: int | I: [Instruction] | V: [(Type, Value)] | K: [State] | U: {String x int} | R: {String x int x bool x bool x [Instruction]}>
```

#Initial State:
```
<code | env | false | false | false | 0 | [] | [] | [] | {} | {}>
```

#Rules:
##Values:
```
<Int(i) :: C | E | T | A | P | S | I | V | K | U | R> 
	=> <C | E | T | A | P | S | I | (Int, i) :: V | K | U | R>
	
<Char(c) :: C | E | T | A | P | S | I | V | K | U | R> 
	=> <C | E | T | A | P | S | I | (Int, c) :: V | K | U | R>
	
<String(c_0 ... c_n) :: C | E | T | A | P | S | I | V | K | U | R> 
	=> <C | E | T | A | P | S | I | (Int, c_0) :: ... :: (Int, c_n) :: V | K | U | R>
	
<Arg(i) :: C | E | T | A | P | S | I | V | K | U | R>
	=> <C | E | T | A | P | S | I | (-, Arg(i)) :: V | K | U | R>
```

##Name Lookup:
```
<Name(i) :: C | (i, v) ∈ E | T | A | P | S | I | V | K | U | R> 
	=> <v :: C | E | T | A | P | S | I | V | K | U | R>
```

##Operators:
```
<BinOp(op) :: C | E | T | A | P | S | I | V | K | U | R> 
	=> <C | E | T | A | P | S | I | (Fn(2, 1, false, false), op) :: V | K | U | R>
	
<TriOp(op) :: C | E | T | A | P | S | I | V | K | U | R> 
	=> <C | E | T | A | P | S | I | (Fn(3, 1, false, false), op) :: V | K | U | R>
```

##Output:
```
<Write :: C | E | T | A | P | S | I | (t, v) :: V | K | U | R>
	=> <C | E | T | A | P | S + 1 | Write(Stored(S)) :: Store(v) :: I | (t, Stored(S)) :: V | K | U | R>
	
<Write :: C | E | T | A | P | S | I | [] | K | U | R>
	=> <C | E | true | A | P | S + 1 | Write(Stored(S)) :: Store(Pop) :: I | (-, Stored(S)) :: V | K | U | R>
```

##Function Pushing:
```
Already evaluated:
<Fn(f, [a_1, ..., a_n], _) :: C | E | T | A | P | S | I | V | K | U | (f, FN, FT, FA, _) ∈ R>
	=> <C | E | T | A | P | S | I | (Fn(n, FN, FT, FA), f) :: V | K | U | R>
	
Recursion:
<Fn(f, [a_1, ..., a_n], FC) :: C | E | T | A | P | S | I | V | K | (f, n) ∈ U | R>
	=> <C | E | T | A | true | S | I | (Fn(n, -1, T, A), f :: V | K | U | R> 

Otherwise:
<Fn(f, [a_1, ..., a_n], FC) :: C | E | T | A | P | S | I | V | K | U | R>
	=> <FC, {(a_1, Arg(1), ..., (a_n, Arg(n))} ∪ E | T | A | P | S | [] | [] | <Fn(f, [a_1, ..., a_n], FC) :: C | T | A | P | E | I | V> :: K | {(f x n)} ∪ U | R>
```

##Operator Application:
```
<Apply :: C | E | T | A | P | S | I | (_, +) :: (_, x) :: (_, y) :: V | K | U | R>
	=> <C | E | T | A | P | S | I | (Int, x + y) :: V | K | U | R>
	
<Apply :: C | E | T | A | P | S | I | (_, -) :: (_, x) :: (_, y) :: V | K | U | R>
	=> <C | E | T | A | P | S | I | (Int, x - y) :: V | K | U | R>
	
<Apply :: C | E | T | A | P | S | I | (_, *) :: (_, x) :: (_, y) :: V | K | U | R>
	=> <C | E | T | A | P | S | I | (Int, x * y) :: V | K | U | R>

<Apply :: C | E | T | A | P | S | I | (_, /) :: (_, x) :: (_, y) :: V | K | U | R>
	=> <C | E | T | A | P | S | I | (Int, x / y) :: V | K | U | R>

<Apply :: C | E | T | A | P | S | I | (_, =) :: (_, x) :: (_, y) :: V | K | U | R>
	=> <C | E | T | A | P | S | I | (Int, x = y ? 1 : 0) :: V | K | U | R>

<Apply :: C | E | T | A | P | S | I | (_, <) :: (_, x) :: (_, y) :: V | K | U | R>
	=> <C | E | T | A | P | S | I | (Int, x < y ? 1 : 0) :: V | K | U | R>

<Apply :: C | E | T | A | P | S | I | (_, >) :: (_, x) :: (_, y) :: V | K | U | R>
	=> <C | E | T | A | P | S | I | (Int, x > y ? 1 : 0) :: V | K | U | R>
	
<Apply :: C | E | T | A | P | S | I | (_, ?) :: (_, i) :: (Fn(a, b, c, d), x) :: (Fn(a, b, e, f), y) :: V | K | U | R>
	=> <C | E | T | A | P | S | I | (Fn(a, b, c || e, d || f), ?(i, x, y)) :: V | K | U | R>  
	
<Apply :: C | E | T | A | P | S | I | (_, ?) :: (_, b) :: (T, x) :: (U, y) :: V | K | U | R>
	=> <C | E | T | A | P | S | I | (T == U ? T : -, ?(b, x, y)) :: V | K | U | R>  
```

##Function Application:
```
<Apply :: C | E | T | A | P | S | I | (Fn(n, m, false, false), f) :: (_, a_1) :: ... :: (_, a_n) :: V | K | U | R> 
	=> <C | E | T | A | P | S + m | Apply(f, [a_1 ... a_n]) :: I | (-, Stored(S + 0)) :: ... :: (-, Stored(S + m - 1)) :: V | K | U | R>

<Apply :: C | E | T | A | P | S | I | (Fn(n, m, true, false), f) :: (_, a_1) :: ... :: (_, a_n) :: (_, v_0) :: ... :: (_, v_m) :: [] | K | U | R> 
	=> <C | E | true | A | P | S + m | Apply(f, [a_1 ... a_n]) :: Push(v_0) :: ... :: Push(v_m) :: I | (-, Stored(S + 0)) :: ... :: (-, Stored(S + m - 1)) :: [] | K | U | R>

<Apply :: C | E | T | A | P | S | I | (Fn(n, -1, _, false), f) :: (_, a_1) :: ... :: (_, a_n) :: (_, v_0) :: ... :: (_, v_m) :: [] | K | U | R> 	
	=> <C | E | true | A | P | S | Apply(f, [a_1 ... a_n]) :: Push(v_0) :: ... :: Push(v_m) :: I | [] | K | U | R>
	
Allocate arguments and fix functions:
<Apply :: C | E | T | A | P | S | I | (Fn(n, -1, _, true), f) :: (t_1, a_1) :: ... :: (t_n, a_n) :: (_, v_0) :: ... :: (_, v_m) :: [] | K | U | R>
	=> <C | E | true | A | true | S | Apply(f, [t_1 == Fn(..) ? Alloc(a_1) : a_1, ...]) :: Push(v_0) :: ... :: Push(v_m) :: I | [] | K | U | {(f, -1, b, true, Push(x_0) :: ... Push(x_1) :: FNI) | i <- 1 .. n && f <- a_i && (f, n > 0, b, false | Return([x_0, ..., x_1] :: FNI) ∈ R} ∪ R>
	
Apply dynamic function:
<Apply :: C | E | T | A | P | S | I | (_, f) :: (_, v_0) :: ... (_, v_n) :: [] | K | U | R>
	=> <C | E | true | true | true | S | Apply(f, Dynamic) :: Push(v_0) :: ... :: Push(v_n) :: I | [] | K | U | R>

#TODO: false, true / true, true
```

##Stack Arguments:
```
<Apply :: C | E | T | A | P | S | I | (Fn(n, _), f) :: (_, a_1) :: ... :: (_, a_(m < n)) :: [] | K | U | R>
	=> <C | E | true | A | P | S | I | (Fn(n, _), f) :: (_, a_1) :: ... :: (_, a_(m < n)) :: (-, Pop) :: [] | K | U | R>
```

##Function Return:
```
<[] | E | T | A | false | S | I | [v_1, ..., v_n] | <Fn(f, [a_1, ..., a_m], _) :: C | KT | KA | P | KE | KI | V> :: K | U | R>
	=> <C | KE | KT | KA | P | S | KI | (Fn(m, n, T, A), f) :: V | K | U | {(f, n, T, A, Return([v_1, ..., v_n]) :: I)} ∪ R>

<[] | E | T | A | true | S | I | [v_1, ..., v_n] | <Fn(f, [a_1, ..., a_m], _) :: C | KT | KA | P | KE | KI | V> :: K | U | R>
	=> <C | KE | KT | KA | true | S | KI | (Fn(m, -1, T, A), f) :: V | K | U | {(f, 0, T, A, Push(v_1) :: ... :: Push(v_n) :: I)} ∪ R>
```

##Accepting State:
```
<[] | E | T | A | P | S | I | V | [] | U | R> 
```

The main function instructions are stored in the stack ```I```, the rest of the functions are in the set ```R```.

#Simple Example:
Given the progam:
```
add: y -> {
	y + ()
}

2 8 add () ,
```

Here is the initial state of the machine:
```
<2 8 add () , | (add, ..) | false | false | false | 0 | [] | [] | [] | {} | {}>
```


First, we push the two integers to the value stack:
```
<8 add () , | (add, ..) | false | false | false | 0 | [] | (Int 2) | [] | {} | {}>
<add () , | (add, ..) | false | false | false | 0 | [] | (Int 8) (Int 2) | [] | {} | {}>
```

Then, we lookup add in the environment:
```
<Fn(add, [y], ..) () , | (add, ..) | false | false | false | 0 | [] | (Int 8) (Int 2) | [] | {} | {}>
```

We save the state on the continuation stack, and evaluate the function:
```
<y + () | (y, Arg(0)) (add, ..) | false | false | false | 0 | [] | [] | <Fn(add, [y], ..) () , | false | false | false | (add, ..) | [] | (Int 8) (Int 2)> | {(add, 1)} | {}>
```

Lookup y in the environment:
```
<Arg(0) + () | (y, Arg(0)) (add, ..) | false | false | false | 0 | [] | [] | <Fn(add, [y], ..) () , | false | false | false | (add, ..) | [] | (Int 8) (Int 2)> | | {(add, 1)} | {}>
```

Push the argument to the value stack:
```
<+ () | (y, Arg(0)) (add, ..) | false | false | false | 0 | [] | (-, Arg(0)) | <Fn(add, [y], ..) () , | false | false | false | (add, ..) | [] | (Int 8) (Int 2)> | {(add, 1)} | {}>
```

Push the addition operator to the value stack:
```
<() | (y, Arg(0)) (add, ..) | false | false | false | 0 | [] | Fn((2, 1, false, false), +) (-, Arg(0)) | <Fn(add, [y], ..) () , | false | false | false | (add, ..) | [] | (Int 8) (Int 2)> | | {(add, 1)} | {}>
```

Add a pop argument to the value stack - this function in now tainted:
```
<() | (y, Arg(0)) (add, ..) | true | false | false | 0 | [] | Fn((2, 1, false, false), +) (-, Arg(0)) (-, Pop) | <Fn(add, [y], ..) () , | false | false | false | (add, ..) | [] | (Int 8) (Int 2)> | {(add, 1} | {}>
```

Apply the addition function:
```
< | (y, Arg(0)) (add, ..) | true | false | false | 0 | [] | (Int, Arg(0) + Pop) | <Fn(add, [y], ..) () , | false | false | false | (add, ..) | [] | (Int 8) (Int 2)> | {(add, 1)} | {}>
```

Pop the top of the continuation stack, and insert the function into the result set:
```
<() , | (add, ..) | false | false | false | 0 | [] | (Fn(1, 1, true, false), add) (Int 8) (Int 2) | [] | {(add, 1)} | {(add, 2, true, Return([Arg(0) + Pop]))}>
```

Apply the function, pushing values to the stack as it is tainted:
```
<, | (add, ..) | true | false | false | 1 | Apply(add, [8]) Push(2) | (-, Stored(0)) | [] | {(add, 1)} | {(add, 2, true, Return([Arg(0) + Pop]))}>
```

Apply the write instruction:
```
< | (add, ..) | true | false | false | 2 | Write(Stored(1)) Store(Stored(0)) Apply(add, [8]) Push(2) | (-, Stored(1)) | [] | | {(add, 1)} | {(add, 2, true, Return([Arg(0) + Pop]))}>
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

#Complex Example:
Given the program:
```
f: x -> {
	x
	
	x -> {
		x
		1 x - ()
		f ()
		* ()
	}
	
	x -> { 1 }
	
	x 0 = ()
	
	? () ()
}

3 f () ,
```

The inital state is:
```
<3 f () , | {(f, ..)} | false | false | false | 0 | [] | [] | [] | {} | {}>
```

Push 3:
```
<f () , | {(f, ..)} | false | false | false | 0 | [] | (Int 3) | [] | {} | {}>
```

Lookup f:
```
<Fn(f, [x], ..) () , | {(f, ..)} | false | false | false | 0 | [] | (Int 3) | [] | {} | {}>
```

Save state and evaluate f:
```
<x Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | {(x, Arg(0)) (f, ..)} | false | false | false | 0 | [] | [] | <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(f, 1)} | {}> 
```

Lookup and push x:
```
<Arg(0) Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | {(x, Arg(0)) (f, ..)} | false | false | false | 0 | [] | [] | <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(f, 1)} | {}>

<Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | {(x, Arg(0)) (f, ..)} | false | false | false | 0 | [] | (-, Arg(0)) | <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(f, 1)} | {}> 
```

Save state and evaluate anon1:
```
<x 1 x - () f () * () | {(x, Arg(0)) (f, ..)} | false | false | false | 0 | [] | [] | <Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | false | false | {(x, Arg(0)) (f, ..)} | [] | (-, Arg(0))>, <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon1, 1) (f, 1)} | {}>
```

Lookup and push initial values (steps skipped for shortness)
```
<- () f () * () | {(x, Arg(0)) (f, ..)} | false | false | false | 0 | [] | (-, Arg(0)) (Int 1) (-, Arg(0)) | <Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | false | false | {(x, Arg(0)) (f, ..)} | [] | (-, Arg(0))>, <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon1, 1) (f, 1)} | {}>
```

Push and apply minus:
```
<() f () * () | {(x, Arg(0)) (f, ..)} | false | false | false | 0 | [] | (Fn(2, 1, false, false), -) (-, Arg(0)) (Int 1) (-, Arg(0)) | <Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | false | false | {(x, Arg(0)) (f, ..)} | [] | (-, Arg(0))>, <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon1, 1) (f, 1)} | {}>

<f () * () | {(x, Arg(0)) (f, ..)} | false | false | false | 0 | [] | (Int Arg(0) - 1) (-, Arg(0)) | <Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | false | false | {(x, Arg(0)) (f, ..)} | [] | (-, Arg(0))>, <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon1, 1) (f, 1)} | {}>
```

Lookup f:
```
<Fn(f, [x], ..) () * () | {(x, Arg(0)) (f, ..)} | false | false | false | 0 | [] | (Int Arg(0) - 1) (-, Arg(0)) | <Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | false | false | {(x, Arg(0)) (f, ..)} | [] | (-, Arg(0))>, <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon1, 1) (f, 1)} | {}>
```

Push as recursion:
```
<() * () | {(x, Arg(0)) (f, ..)} | false | false | true | 0 | [] | (Fn(1, -1, false, false), f) (Int Arg(0) - 1) (-, Arg(0)) | <Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | false | false | {(x, Arg(0)) (f, ..)} | [] | (-, Arg(0))>, <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon1, 1) (f, 1)} | {}>
```

Apply function, tainting the current state
```
<* () | {(x, Arg(0)) (f, ..)} | true | false | true | 0 | Apply(f, [Arg(0) - 1]) Push(Arg(0)) | [] | <Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | false | false | {(x, Arg(0)) (f, ..)} | [] | (-, Arg(0))>, <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon1, 1) (f, 1)} | {}>
```

Push and apply *:
```
< | {(x, Arg(0)) (f, ..)} | true | false | true | 0 | Apply(f, [Arg(0) - 1]) Push(Arg(0)) | (Int, Pop * Pop) | <Fn(anon1, [x], ..) Fn(anon2, [x], ..) x 0 = () ? () () | false | false | {(x, Arg(0)) (f, ..)} | [] | (-, Arg(0))>, <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon1, 1) (f, 1)} | {}>
```

Return, pushing the values:
```
<Fn(anon2, [x], ..) x 0 = () ? () () | {(x, Arg(0)) (f, ..)} | false | false | true | 0 | [] | (Fn(1, -1, true, false), anon1) (-, Arg(0)) | <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon1, 1) (f, 1)} | {(anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>
```

Save state and evaluate anon2:
```
<1 | {(x, Arg(0)) (f, ..)} | false | false | true | 0 | [] | [] | <Fn(anon2, [x], ..) x 0 = () ? () () | false | false | true | [] | (Fn(1, -1, true, false), anon1) (-, Arg(0))> <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)>| {(anon2, 1) (anon1, 1) (f, 1)} | {(anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>

< | {(x, Arg(0)) (f, ..)} | false | false | true | 0 | [] | (Int, 1) | <Fn(anon2, [x], ..) x 0 = () ? () () | false | false | true | [] | (Fn(1, -1, true, false), anon1) (-, Arg(0))> <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)>| {(anon2, 1) (anon1, 1) (f, 1)} | {(anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>

<x 0 = () ? () () | {(x, Arg(0)) (f, ..)} | false | false | true | 0 | [] | (Fn(1, -1, false, false), anon2) (Fn(1, -1, true, false), anon1) (-, Arg(0)) | <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon2, 1) (anon1, 1) (f, 1)} | {(anon2, Push(1)) (anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>
```

Evaluate x = 0 (shortened):
```
<? () () | {(x, Arg(0)) (f, ..)} | false | false | true | 0 | [] | (Int, 0 == Arg(0) ? 1 : 0) (Fn(1, -1, false, false), anon2) (Fn(1, -1, true, false), anon1) (-, Arg(0)) | <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon2, 1) (anon1, 1) (f, 1)} | {(anon2, Push(1)) (anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>
``` 

Push ?:
```
<() () | {(x, Arg(0)) (f, ..)} | false | false | true | 0 | [] | (Fn(3, 1, false, false), ?) (Int, 0 == Arg(0) ? 1 : 0) (Fn(1, -1, false, false), anon2) (Fn(1, -1, true, false), anon1) (-, Arg(0)) | <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon2, 1) (anon1, 1) (f, 1)} | {(anon2, Push(1)) (anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>
```

Apply ?:
```
<() | {(x, Arg(0)) (f, ..)} | false | false | true | 0 | [] | (Fn(1, -1, true, false), ?(0 == Arg(0) ? 1 : 0, anon2, anon1)) (-, Arg(0)) | <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon2, 1) (anon1, 1) (f, 1)} | {(anon2, Push(1)) (anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>
```

Apply result:
```
< | {(x, Arg(0)) (f, ..)} | true | false | true | 0 | Apply(?(0 == Arg(0) ? 1 : 0, anon2, anon1)), [Arg(0)]) | [] | <Fn(f, [x], ..) () , | false | false | {(f, ..)} | [] | (Int 3)> | {(anon2, 1) (anon1, 1) (f, 1)} | {(anon2, Push(1)) (anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>
```

Return, pushing the result:
```
<() , | {(f, ..)} | false | false | true | 0 | [] | (Fn(1, -1, true, false), f) (Int 3) | [] | {(anon2, 1) (anon1, 1) (f, 1)} | {(f, Apply(?(0 == Arg(0) ? 1 : 0, anon2, anon1)), [Arg(0)])) (anon2, Push(1)) (anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>
```

Apply f:
```
<, | {(f, ..)} | true | false | true | 0 | Apply(f, [3]) | [] | [] | {(anon2, 1) (anon1, 1) (f, 1)} | {(f, Apply(?(0 == Arg(0) ? 1 : 0, anon2, anon1)), [Arg(0)])) (anon2, Push(1)) (anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>
```

Apply write instruction:
```
< | {(f, ..)} | true | false | true | 1 | Write(Stored(0)) Store(Pop) Apply(f, [3]) | Stored(0) | [] | {(anon2, 1) (anon1, 1) (f, 1)} | {(f, Apply(?(0 == Arg(0) ? 1 : 0, anon2, anon1)), [Arg(0)])) (anon2, Push(1)) (anon1, Push(Pop * Pop) Apply(f, [Arg(0) - 1]) Push(Arg(0))}>
```

And finally we are done! The instructions are:
```
Apply(f, [3]); Store(Pop); Write(Stored(0));
f: Apply(?(Arg(0) == 0 ? 1 : 0, anon2, anon1), [Arg(0)])
anon2: Push(1);
anon1: Push(Arg[0]); Apply(f, [Arg(0) - 1]); Push(Pop * Pop);
```

Which translated to an iterative style is:
```
main() {
	f(3);
	val = pop();
	write(val);
}

f(x) {
	if(x == 0) {
		anon2(x);
	} else {
		anon1(x);
	}
}

anon2(x) {
	push(1);
}

anon1(x) {
	push(x);
	f(x - 1);
	push(pop() * pop());
}
```

#Futher example
Given the program

```
double: x -> {
	x x + ()
}

apply: f -> x -> {
	x f ()
}

5 double apply () ,
```

The initial state is:

```
<5 double apply () , | {(double, ..) (apply, ..)} | false | false | false | 0 | [] | [] | [] | {} | {}>
```

First, we push 5:
```
<double apply () , | {(double, ..) (apply, ..)} | false | false | false | 0 | [] | (Int, 5) | [] | {} | {}>
```

Lookup double:
```
<Fn(double, [x], ..) apply () , | {(double, ..) (apply, ..)} | false | false | false | 0 | [] | (Int, 5) | [] | {} | {}>
```

Save the state and evaluate double (process shortened):
```
<x x + () | {(x, Arg(0)) (double, ..) (apply, ..)} | false | false | false | 0 | [] | [] | <Fn(double, [x], ..) apply () , | false | false | false | {(double, ..) (apply, ..)} | [] | (Int, 5)> | {(double, 1)} | {}>

< | {(x, Arg(0)) (double, ..) (apply, ..)} | false | false | false | 0 | [] | (Int, Arg(0) + (Arg(0)) | <Fn(double, [x], ..) apply () , | false | false | false | {(double, ..) (apply, ..)} | [] | (Int, 5)> | {(double, 1)} | {}>

<apply () , | {(double, ..) (apply, ..)} | false | false | false | 0 | [] | (Fn(1, 1, false, false), double) (Int, 5) | {(double, 1)} | {(double, 1, false, false, Return([Arg(0) + (Arg(0)]))}> 
```

Lookup apply:
```
<Fn(apply, [f, x], ..) () , | {(double, ..) (apply, ..)} | false | false | false | 0 | [] | (Fn(1, 1, false, false), double) (Int, 5) | {(double, 1)} | {(double, 1, false, false, Return([Arg(0) + (Arg(0)]))}> 
```

Save the state to evaluate apply:
```
<x f () | {(x, Arg(1)) (f, Arg(0)) (double, ..) (apply, ..)} | false | false | false | 0 | [] | [] | <Fn(apply, [f, x], ..) () , | false | false | false | {(double, ..) (apply, ..)} | [] | (Fn(1, 1, false, false), double) (Int, 5)> {(apply, 2) (double, 1) | {(double, 1, false, false, Return([Arg(0) + (Arg(0)]))}>
```

Lookup and push x and f (shortened):
```
<() | {(x, Arg(1)) (f, Arg(0)) (double, ..) (apply, ..)} | false | false | false | 0 | [] | (-, Arg(0)) (-, Arg(1)) | <Fn(apply, [f, x], ..) () , | false | false | false | {(double, ..) (apply, ..)} | [] | (Fn(1, 1, false, false), double) (Int, 5)> {(apply, 2) (double, 1) | {(double, Return([Arg(0) + (Arg(0)]))}>
```

Apply f, which means the function requires this arguments information allocated:
```
< | {(x, Arg(1)) (f, Arg(0)) (double, ..) (apply, ..)} | true | true | true | 0 | Apply(Arg(0), Dynamic) Push(Arg(1)) | [] | <Fn(apply, [f, x], ..) () , | false | false | false | {(double, ..) (apply, ..)} | [] | (Fn(1, 1, false, false), double) (Int, 5)> {(apply, 2) (double, 1) | {(double, 1, false, false, Return([Arg(0) + (Arg(0)]))}>
```

Save apply and pop the state
```
<() , | {(double, ..) (apply, ..)} | false | false | true | 0 | [] | (Fn(2, -1, true, true), apply) (Fn(1, 1, false, false), double) (Int, 5) | [] {(apply, 2) (double, 1)} | {(apply, 0, true, true, Apply(Arg(0), Dynamic) Push(Arg(1))) (double, 1, false, false, Return([Arg(0) + (Arg(0)]))}> 
```

Apply apply - this requires arguments to be allocating, and a version of double creating that pushes values instead of returns:
```
<, | {(double, ..) (apply, ..)} | true | false | true | 0 | Apply(apply, [Alloc(double), 5]) | [] | {(apply, 2) (double, 1)} | {(double, -1, false, true, Push(Arg(0) + Arg(0))) (apply, 0, true, true, Apply(Arg(0), Dynamic) Push(Arg(1))) (double, 1, false, false, Return([Arg(0) + (Arg(0)]))}>
```

Finally, apply the write instruction:
```
< | {(double, ..) (apply, ..)} | true | false | true | 0 | Write(Stored(0)) Store(Pop) Apply(apply, [Alloc(double), 5]) | Stored(0) | {(apply, 2) (double, 1)} | {(double, -1, false, true, Push(Arg(0) + Arg(0))) (apply, 0, true, true, Apply(Arg(0), Dynamic) Push(Arg(1))) (double, 1, false, false, Return([Arg(0) + (Arg(0)]))}>
```

And done! The final instructions are:
```
Apply(apply, [Alloc(double), 5]); Store(Pop); Write(Stored(0));
double_1: Push(Arg(0) + Arg(0));
apply: Push(Arg(1)); Apply(Arg(0));
```

Which converted would be similar to:
```
main() {
	f = [double_1; 1];
	apply(f, 5);
	val = pop();
	write(val);
}

double_1(x) {
	push(x + x);
}

apply(f, x) {
	push(x);
	args = [];
	for _ in 0 .. f[1] {
		args.push(pop());
	}
	f[0](args);
}
```
