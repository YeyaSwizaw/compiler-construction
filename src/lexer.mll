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

(* Position of previous char (for error handling) *)
let prev_pos lexbuf = { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_pos - 1 }

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
        | '(' { read_callspec (prev_pos lexbuf) lexbuf }
        | '{' { LBRACE (prev_pos lexbuf) }
        | '}' { RBRACE }
        | ':' { COLON }
        | '.' { DOT }
        | ',' { COMMA }
        | '~' { TILDE } 
        | '"' { read_string (prev_pos lexbuf) (Buffer.create 17) lexbuf }
        | ''' { read_character (prev_pos lexbuf) lexbuf }
        | "->" { ARROW }
        | '-' { SUB }
        | '/' { DIVIDE }
        | '+' { PLUS }
        | '*' { STAR }
        | '<' { LT }
        | '>' { GT }
        | '=' { EQ }
        | '?' { ITE }
        | '#' { read_comment lexbuf } 
        | eof { EOF }
        | _ { ERROR (Lexing.lexeme lexbuf) }

and read_string start buf =
    parse
        | '"' { STRING (Buffer.contents buf) }

        | line { 
            next_line lexbuf; 
            Buffer.add_char buf '\n';
            read_string start buf lexbuf
        }

        | "\\n" { 
            Buffer.add_char buf '\n';
            read_string start buf lexbuf
        }

        | "\\\\" {
            Buffer.add_char buf '\\';
            read_string start buf lexbuf
        }

        | [^ '"' '\\']+ {
            Buffer.add_string buf (Lexing.lexeme lexbuf);
            read_string start buf lexbuf
        }

        | eof { UNTERMINATED_STRING start }

and read_character start =
    parse
        | "\\n" { end_character start '\n' lexbuf }
        | "\\\\" { end_character start '\\' lexbuf }
        | line { next_line lexbuf; end_character start '\n' lexbuf }
        | eof { UNTERMINATED_CHAR start }
        | _ { end_character start (Lexing.lexeme lexbuf).[0] lexbuf }

and end_character start ch =
    parse
        | ''' { CHAR ch }
        | line { next_line lexbuf; UNTERMINATED_CHAR start }
        | eof { UNTERMINATED_CHAR start }
        | _ { UNTERMINATED_CHAR start }

and read_callspec start = 
    parse
        | ')' { FULL_CALLSPEC }
        | '*' { end_callspec start TOTAL_CALLSPEC lexbuf }
        | int { end_callspec start (PARTIAL_CALLSPEC (int_of_string (Lexing.lexeme lexbuf))) lexbuf }
        | line { next_line lexbuf; INVALID_CALLSPEC start }
        | eof { INVALID_CALLSPEC start }
        | _ { INVALID_CALLSPEC start }

and end_callspec start tok =
    parse
        | ')' { tok }
        | line { next_line lexbuf; INVALID_CALLSPEC start }
        | eof { INVALID_CALLSPEC start }
        | _ { INVALID_CALLSPEC start }

and read_comment =
    parse
        | line { next_line lexbuf; read lexbuf }
        | eof { EOF }
        | _ { read_comment lexbuf }
