(* Compiler Construction - errors.ml *)
(* Samuel Sleight *)

open Core.Std
open Lexing

type syntax_error_t =
    | ExpectedExpression
    | ExpectedValue
    | ExpectedCallspec
    | ExpectedRParen

type error =
    | SyntaxError of Lexing.position * Lexing.position * syntax_error_t

type ('a, 'b) result =
    | Ok of 'a
    | Err of 'b

type 'a parse_result = ('a, (error list)) result

let expected_expression b e = SyntaxError (b, e, ExpectedExpression)
let expected_value b e = SyntaxError (b, e, ExpectedValue)
let expected_callspec b e = SyntaxError (b, e, ExpectedCallspec)
let expected_rparen b e = SyntaxError (b, e, ExpectedRParen)

let print_position p = 
    print_string (p.pos_fname ^ ":");
    print_int p.pos_lnum;
    print_string ":";
    print_int (p.pos_cnum - p.pos_bol)

let print_error_in_file file pos epos =
    In_channel.seek file (Int64.of_int pos);

    let rec print_loop len = 
        begin match In_channel.input_char file with
            | Some c -> print_char c
            | None -> ()
        end;

        if len > 1 then print_loop (len - 1) else ()
    in

    print_loop (epos - pos)


let print_error_location file start = 
    In_channel.seek file (Int64.of_int start.pos_bol);
    match In_channel.input_line ?fix_win_eol:(Some true) file with
        | Some line -> (
            print_newline ();
            print_endline ("[1;26m" ^ line ^ "[0m");
            print_endline ((String.make (start.pos_cnum - start.pos_bol) ' ') ^ "[1;31m^[0m")
        )

        | None -> ()

let rec print_errors file = function
    | [] -> ()
    | e :: es -> (
        print_string "[1;31m[Error][0m";

        begin match e with
            | SyntaxError (start, finish, ExpectedExpression) -> (
                print_string "[1;35m["; print_position start; print_string "][0m ";
                print_newline ();
                print_string "Expected expression, found: [1;37m'"; 
                print_error_in_file file start.pos_cnum finish.pos_cnum;
                print_endline "[0m'";
                print_error_location file start
            )

            | SyntaxError (start, finish, ExpectedValue) -> (
                print_string "[1;35m["; print_position start; print_string "][0m ";
                print_newline ();
                print_string "Expected value, found: [1;37m'"; 
                print_error_in_file file start.pos_cnum finish.pos_cnum;
                print_endline "[0m'";
                print_error_location file start
            )

            | SyntaxError (start, finish, ExpectedCallspec) -> (
                print_string "[1;35m["; print_position start; print_string "][0m ";
                print_newline ();
                print_string "Expected number, '*', or ')', found: [1;37m'";
                print_error_in_file file start.pos_cnum finish.pos_cnum;
                print_endline "[0m'";
                print_error_location file start
            )

            | SyntaxError (start, finish, ExpectedRParen) -> (
                print_string "[1;35m["; print_position start; print_string "][0m ";
                print_newline ();
                print_string "Expected ')', found: [1;37m'";
                print_error_in_file file start.pos_cnum finish.pos_cnum;
                print_endline "[0m'";
                print_error_location file start
            )
        end;

        print_newline ();
        print_errors file es;
    )
