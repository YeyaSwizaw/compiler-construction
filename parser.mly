%{ 
open Errors
%}

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
%token <string> ERROR

%start <(Syntax.expr list) Errors.parse_result> program

%%

program:
    | EOF { Ok([]) }
    | e = expr; p = program { 
        match (e, p) with
            | (Errors.Ok(e'), Errors.Ok(p')) -> Ok(e' :: p')
            | (Errors.Ok(_), Errors.Err(err))
            | (Errors.Err(err), Errors.Ok(_)) -> Errors.Err(err)
            | (Errors.Err(e1), Errors.Err(e2)) -> Errors.Err(e1 @ e2)
    }

expr:
    | v = value { v }
    | o = op { o }
    | a = assignment { a }
    | LPAREN; c = callspec_tail { c }

    | error { Errors.Err([Errors.ExpectedExpression $startpos]) }

value:
    | f = func { f }
    | i = INT { Errors.Ok(Syntax.Value (Syntax.Int i)) }
    | i = IDENT { Errors.Ok(Syntax.Value (Syntax.Ident i)) }

    | error { Errors.Err([Errors.ExpectedValue $startpos]) }

op:
    | SUB { Errors.Ok(Syntax.Op Syntax.Minus) }
    | DIVIDE { Errors.Ok(Syntax.Op Syntax.Divide) }
    | PLUS { Errors.Ok(Syntax.Op Syntax.Plus) }
    | STAR { Errors.Ok(Syntax.Op Syntax.Star) }

callspec_tail:
    | c = callspec; RPAREN {
        match c with
            | Errors.Ok(c') -> Errors.Ok(Syntax.Apply c')
            | Errors.Err(err) -> Errors.Err(err)
    }

    | error { Errors.Err([Errors.ExpectedRParen $startpos]) }

callspec:
    | i = INT { Errors.Ok(Syntax.Partial(i)) }
    | STAR { Errors.Ok(Syntax.Total) }
    | { Errors.Ok(Syntax.Full) }

    | error { Errors.Err([Errors.ExpectedCallspec $startpos]) }

assignment:
    | i = IDENT; COLON; v = value { 
        match v with
            | Errors.Ok(Syntax.Value v') -> Errors.Ok(Syntax.Assignment (i, v'))
            | Errors.Err(err) -> Errors.Err(err)
    }

func:
    | a = func_arg_list; LBRACE; b = func_body; RBRACE { 
        match b with
            | Errors.Ok(b') -> Errors.Ok(Syntax.Value (Syntax.Function (a, b'))) 
            | Errors.Err(err) -> Errors.Err(err)
    }

func_arg_list:
    | { [] }
    | i = IDENT; ARROW; a = func_arg_list { i::a }

func_body:
    | { Errors.Ok([]) }
    | e = expr; f = func_body { 
        match (e, f) with
            | (Errors.Ok(e'), Errors.Ok(f')) -> Ok(e' :: f')
            | (Errors.Ok(_), Errors.Err(err))
            | (Errors.Err(err), Errors.Ok(_)) -> Errors.Err(err)
            | (Errors.Err(e1), Errors.Err(e2)) -> Errors.Err(e1 @ e2)
    }
