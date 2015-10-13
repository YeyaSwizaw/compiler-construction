(* Compiler Construction - parser.mly *)
(* Samuel Sleight *)

%{ 
open Errors
%}

(* Symbol Tokens *)
%token <int> INT
%token <string> IDENT
%token <string> STRING
%token LPAREN
%token RPAREN
%token <Lexing.position> LBRACE (* Position for unterminated brace errors *)
%token RBRACE
%token COLON
%token ARROW
%token SUB
%token DIVIDE
%token PLUS
%token STAR
%token LT
%token GT
%token EQ
%token ITE

(* End of file *)
%token EOF

(* Error handling tokens *)
%token <Lexing.position> UNTERMINATED_STRING
%token <string> ERROR (* Generic catch all to stop lexer errors *)

(* Start symbol - either a list of expressions or a list of errors *)
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

    (* This (with the case in value) produces reduce/reduce conflicts.
     * They are harmless and only in error handling *)
    | error { Errors.Err([Errors.expected_expression $startpos $endpos]) }

value:
    | f = func { f }
    | i = INT { Errors.Ok(Syntax.Value (Syntax.Int i)) }
    | i = IDENT { Errors.Ok(Syntax.Value (Syntax.Ident i)) }
    | s = STRING { Errors.Ok(Syntax.Value (Syntax.String s)) }

    | p = UNTERMINATED_STRING { Errors.Err([Errors.unterminated_string p]) }

    (* This (with the case in expr) produces reduce/reduce conflicts.
     * They are harmless and only in error handling *)
    | error { Errors.Err([Errors.expected_value $startpos $endpos]) }

op:
    | SUB { Errors.Ok(Syntax.Op Syntax.Minus) }
    | DIVIDE { Errors.Ok(Syntax.Op Syntax.Divide) }
    | PLUS { Errors.Ok(Syntax.Op Syntax.Plus) }
    | STAR { Errors.Ok(Syntax.Op Syntax.Star) }
    | LT { Errors.Ok(Syntax.Op Syntax.Lt) }
    | GT { Errors.Ok(Syntax.Op Syntax.Gt) }
    | EQ { Errors.Ok(Syntax.Op Syntax.Eq) }
    | ITE { Errors.Ok(Syntax.Op Syntax.IfThenElse) }

callspec_tail:
    | c = callspec; RPAREN {
        match c with
            | Errors.Ok(c') -> Errors.Ok(Syntax.Apply c')
            | Errors.Err(err) -> Errors.Err(err)
    }

    | error { Errors.Err([Errors.expected_rparen $startpos $endpos]) }

callspec:
    | i = INT { Errors.Ok(Syntax.Partial(i)) }
    | STAR { Errors.Ok(Syntax.Total) }
    | { Errors.Ok(Syntax.Full) }

    | error { Errors.Err([Errors.expected_callspec $startpos $endpos]) }

assignment:
    | i = IDENT; COLON; v = value { 
        match v with
            | Errors.Ok(Syntax.Value v') -> Errors.Ok(Syntax.Assignment (i, v'))
            | Errors.Err(err) -> Errors.Err(err)
    }

func:
    | a = func_arg_list; p = LBRACE; b = func_body; EOF { 
        match b with
            | Errors.Ok(_) -> Errors.Err([Errors.unterminated_lbrace p])
            | Errors.Err(err) -> Errors.Err((Errors.unterminated_lbrace p) :: err)
    }

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
