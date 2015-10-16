This is the simple grammar for the language. 
The lexer and parser in code are much more complicated but deal with complex error handling.

```
program -> expr program | ε 

expr -> value | operator | callspec | assignment

assignment -> ident ':' value

value -> function | int | ident | char | string

operator -> '+' | '-' | '/' | '*' | '>' | '<' | '=' | '?'

callspec -> '(' callspec_type ')'
callspec_type -> ε | '*' | int

function -> args? '{' program '}'
args -> arg args | ε 
arg -> ident '->'

int -> ['0' - '9']+
ident -> ['a' - 'z' 'A' - 'Z' '_']['0' - '9' 'a' - 'z' 'A' - 'Z' '_']*

```
