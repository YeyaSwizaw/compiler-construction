(* Compiler Construction - lexer.mll *)
(* Samuel Sleight *)

{

open Lexing
open Parser

(* Increment lexbuf line number (taken from RWO) *)
let next_line lexbuf = 
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
        { pos with pos_bol = lexbuf.lex_curr_pos;
                   pos_lnum = pos.pos_lnum + 1
        }

}

let int = '-'? ['0'-'9']+
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let ws = [' ' '\t']+
let line = '\n' | '\r' | "\r\n"

rule read =
    parse
        | ws { read lexbuf }
        | line { next_line lexbuf; read lexbuf }
        | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
        | ident { IDENT (Lexing.lexeme lexbuf) }
        | '(' { LPAREN }
        | ')' { RPAREN }
        | '{' { LBRACE ({lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_pos - 1}) }
        | '}' { RBRACE }
        | ':' { COLON }
        | '"' { read_string ({lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_pos - 1}) (Buffer.create 17) lexbuf }
        | "->" { ARROW }
        | '-' { SUB }
        | '/' { DIVIDE }
        | '+' { PLUS }
        | '*' { STAR }
        | '#' { read_comment lexbuf } 
        | eof { EOF }
        | _ { ERROR (Lexing.lexeme lexbuf) }

and read_string start buf =
    parse
        | '"' { STRING (Buffer.contents buf) }

        | [^ '"' '\\']+ {
            Buffer.add_string buf (Lexing.lexeme lexbuf);
            read_string start buf lexbuf
        }

        | eof { UNTERMINATED_STRING start }

and read_comment =
    parse
        | line { next_line lexbuf; read lexbuf }
        | eof { EOF }
        | _ { read_comment lexbuf }
