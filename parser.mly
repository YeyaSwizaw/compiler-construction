%token <int> INT
%token <string> IDENT
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COLON
%token ARROW
%token SUB
%token DIVIDE
%token PLUS
%token STAR
%token EOF

%start <Syntax.expr list> program
%%

program:
    | EOF { [] }
    | e = expr; p = program { e::p }

expr:
    | v = value { Syntax.Value v }
    | o = op { Syntax.Op o }
    | LPAREN; c = callspec; RPAREN { Syntax.Apply c }
    | a = assignment { a }

value:
    | f = func { f }
    | i = INT { Syntax.Int i }
    | i = IDENT { Syntax.Ident i }

op:
    | SUB { Syntax.Minus }
    | DIVIDE { Syntax.Divide }
    | PLUS { Syntax.Plus }
    | STAR { Syntax.Star }

callspec:
    | i = INT { Syntax.Partial(i) }
    | STAR { Syntax.Total }
    | { Syntax.Full }

assignment:
    | i = IDENT; COLON; v = value { Syntax.Assignment (i, v) }

func:
    | a = func_arg_list; LBRACE; b = func_body; RBRACE { Syntax.Function (a, b) }

func_arg_list:
    | { [] }
    | i = IDENT; ARROW; a = func_arg_list { i::a }

func_body:
    | { [] }
    | e = expr; f = func_body { e::f }
