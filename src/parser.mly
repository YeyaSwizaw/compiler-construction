(* Compiler Construction - parser.mly *)
(* Samuel Sleight *)

%{ 
open Errors
open Syntax
open Syntax.Env
%}

(* Symbol Tokens *)
%token <int> INT
%token <char> CHAR
%token <string> IDENT
%token <string> STRING
%token <Lexing.position> LBRACE (* Position for unterminated brace errors *)
%token <int> PARTIAL_CALLSPEC
%token FULL_CALLSPEC
%token TOTAL_CALLSPEC
%token RBRACE
%token COLON
%token DOT
%token COMMA
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
%token <Lexing.position> UNTERMINATED_CHAR
%token <Lexing.position> INVALID_CALLSPEC
%token <string> ERROR (* Generic catch all to stop lexer errors *)

(* Start symbol - either a list of expressions or a list of errors *)
%start <Syntax.program_t Errors.parse_result> program

%%

program:
    | EOF { Ok({env = Syntax.Env.empty; code = []}) }

    | e = expr; p = program { 
        match (e, p) with
            | (Errors.Ok(e'), Errors.Ok(p')) -> Errors.Ok({p' with code = (e' :: p'.code)})
            | (Errors.Ok(_), Errors.Err(err))
            | (Errors.Err(err), Errors.Ok(_)) -> Errors.Err(err)
            | (Errors.Err(e1), Errors.Err(e2)) -> Errors.Err(e1 @ e2)
    }

    | a = assignment; p = program {
        match (a, p) with
            | (Errors.Ok((name, value)), Errors.Ok(p')) -> (
                if (Syntax.Env.mem name p'.env) then
                    Errors.Err([Errors.redefined_name name $startpos (Syntax.Env.find name p'.env).location])
                else
                    Errors.Ok({p' with env = (Syntax.Env.add name value p'.env)})
            )

            | (Errors.Ok(_), Errors.Err(err))
            | (Errors.Err(err), Errors.Ok(_)) -> Errors.Err(err)
            | (Errors.Err(e1), Errors.Err(e2)) -> Errors.Err(e1 @ e2)
    }

expr:
    | v = value { match v with
        | Errors.Ok (value, pos) -> Errors.Ok ({location=pos; data=(Syntax.Value value)})
        | Errors.Err errs -> Errors.Err errs
    }

    | o = op { o }
    | c = callspec { c }

    | COMMA { Errors.Ok ({location=$startpos; data=(Syntax.Write Syntax.AsChar)}) }
    | DOT { Errors.Ok ({location=$startpos; data=(Syntax.Write Syntax.AsInt)}) }

    | error { Errors.Err([Errors.expected_expression $startpos $endpos]) }

value:
    | f = func { f }
    | i = INT { Errors.Ok (Syntax.Int i, $startpos) }
    | c = CHAR { Errors.Ok (Syntax.Char c, $startpos) }
    | i = IDENT { Errors.Ok (Syntax.Ident i, $startpos) }
    | s = STRING { Errors.Ok (Syntax.String s, $startpos) }

    | p = UNTERMINATED_STRING { Errors.Err([Errors.unterminated_string p]) }
    | p = UNTERMINATED_CHAR { Errors.Err([Errors.unterminated_char p]) }

op:
    | SUB { Errors.Ok (Syntax.op_chunk Syntax.Minus $startpos) }
    | DIVIDE { Errors.Ok (Syntax.op_chunk Syntax.Divide $startpos) }
    | PLUS { Errors.Ok (Syntax.op_chunk Syntax.Plus $startpos) }
    | STAR { Errors.Ok (Syntax.op_chunk Syntax.Times $startpos) }
    | LT { Errors.Ok (Syntax.op_chunk Syntax.Lt $startpos) }
    | GT { Errors.Ok (Syntax.op_chunk Syntax.Gt $startpos) }
    | EQ { Errors.Ok (Syntax.op_chunk Syntax.Eq $startpos) }
    | ITE { Errors.Ok (Syntax.op_chunk Syntax.IfThenElse $startpos) }

callspec:
    | i = PARTIAL_CALLSPEC { Errors.Ok (Syntax.callspec_chunk (Syntax.Partial i) $startpos) }
    | FULL_CALLSPEC { Errors.Ok (Syntax.callspec_chunk Syntax.Full $startpos) }
    | TOTAL_CALLSPEC { Errors.Ok (Syntax.callspec_chunk Syntax.Total $startpos) }

    | e = INVALID_CALLSPEC { Errors.Err([Errors.invalid_callspec e]) }

assignment:
    | i = IDENT; COLON; v = exp_value { 
        match v with
            | Errors.Ok (value, position) -> Errors.Ok((i, {location=$startpos; data=value}))
            | Errors.Err err -> Errors.Err(err)
    }

exp_value:
    | v = value { v }
    | error { Errors.Err([Errors.expected_value $startpos $endpos]) }

func:
    | a = func_arg_list; p = LBRACE; b = func_body; EOF { 
        match b with
            | Errors.Ok(_) -> Errors.Err([Errors.unterminated_lbrace p])
            | Errors.Err(err) -> Errors.Err((Errors.unterminated_lbrace p) :: err)
    }

    | a = func_arg_list; LBRACE; b = func_body; RBRACE { 
        match b with
            | Errors.Ok(b') -> Errors.Ok (Syntax.Function (a, b'), $startpos) 
            | Errors.Err(err) -> Errors.Err(err)
    }

func_body:
    | { Ok({env = Syntax.Env.empty; code = []}) }

    | e = expr; p = func_body { 
        match (e, p) with
            | (Errors.Ok(e'), Errors.Ok(p')) -> Errors.Ok({p' with code = (e' :: p'.code)})
            | (Errors.Ok(_), Errors.Err(err))
            | (Errors.Err(err), Errors.Ok(_)) -> Errors.Err(err)
            | (Errors.Err(e1), Errors.Err(e2)) -> Errors.Err(e1 @ e2)
    }

    | a = assignment; p = func_body {
        match (a, p) with
            | (Errors.Ok((name, value)), Errors.Ok(p')) -> (
                if (Syntax.Env.mem name p'.env) then
                    Errors.Err([Errors.redefined_name name $startpos (Syntax.Env.find name p'.env).location])
                else
                    Errors.Ok({p' with env = (Syntax.Env.add name value p'.env)})
            )

            | (Errors.Ok(_), Errors.Err(err))
            | (Errors.Err(err), Errors.Ok(_)) -> Errors.Err(err)
            | (Errors.Err(e1), Errors.Err(e2)) -> Errors.Err(e1 @ e2)
    }

func_arg_list:
    | { [] }
    | i = IDENT; ARROW; a = func_arg_list { i::a }

