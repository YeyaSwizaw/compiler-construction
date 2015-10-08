open Core.Std
open Lexing

type syntax_error =
    | ExpectedExpression of Lexing.position
    | ExpectedValue of Lexing.position
    | ExpectedCallspec of Lexing.position
    | ExpectedRParen of Lexing.position

type 'a parse_result = 
    | Ok of 'a
    | Err of (syntax_error list)

let print_position p = 
    print_string (p.pos_fname ^ ":");
    print_int p.pos_lnum;
    print_string ":";
    print_int (p.pos_cnum - p.pos_bol);
    print_string ":"

let print_nth file pos =
    In_channel.seek file (Int64.of_int pos);
    match In_channel.input_char file with
        | Some c -> print_char c
        | None -> print_string "<error>"

let rec print_errors file = function
    | [] -> ()
    | e :: es -> (
        begin match e with
            | ExpectedExpression p -> print_position p; print_string " Expected expression, found: '"; print_nth file p.pos_cnum; print_string "'"
            | ExpectedValue p -> print_position p; print_string " Expected value, found: '"; print_nth file p.pos_cnum; print_string "'"
            | ExpectedCallspec p -> print_position p; print_string " Expected number, '*', or ')', found: '"; print_nth file p.pos_cnum; print_string "'"
            | ExpectedRParen p -> print_position p; print_string " Expected ')', found: '"; print_nth file p.pos_cnum; print_string "'"
        end;

        print_newline ();
        print_errors file es;
    )

