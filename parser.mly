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

value:
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
