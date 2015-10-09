{

open Lexing
open Parser

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
        | '{' { LBRACE }
        | '}' { RBRACE }
        | ':' { COLON }
        | "->" { ARROW }
        | '-' { SUB }
        | '/' { DIVIDE }
        | '+' { PLUS }
        | '*' { STAR }
        | '#' { comment lexbuf } 
        | eof { EOF }
        | _ { ERROR (Lexing.lexeme lexbuf) }

and comment =
    parse
        | line { next_line lexbuf; read lexbuf }
        | eof { EOF }
        | _ { comment lexbuf }
