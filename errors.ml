open Core.Std
open Lexing

type error_t =
    | ExpectedExpression
    | ExpectedValue
    | ExpectedCallspec
    | ExpectedRParen

type error = {
    start: Lexing.position;
    finish: Lexing.position;
    kind: error_t
}

type 'a parse_result = 
    | Ok of 'a
    | Err of (error list)

let expected_expression b e = { 
    start = b; 
    finish = e; 
    kind = ExpectedExpression 
}

let expected_value b e = { 
    start = b; 
    finish = e; 
    kind = ExpectedValue 
}

let expected_callspec b e = { 
    start = b; 
    finish = e; 
    kind = ExpectedCallspec 
}

let expected_rparen b e = { 
    start = b; 
    finish = e; 
    kind = ExpectedRParen 
}

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

let rec print_errors file = function
    | [] -> ()
    | e :: es -> (
        print_string "[1;31m[Error][0m";
        print_string "[1;35m["; print_position e.start; print_string "][0m ";

        begin match e.kind with
            | ExpectedExpression -> (
                print_newline ();
                print_string "Expected expression, found: [1;37m'"; 
                print_error_in_file file e.start.pos_cnum e.finish.pos_cnum;
                print_string "'[0m"
            )

            | ExpectedValue -> (
                print_newline ();
                print_string "Expected value, found: [1;37m'"; 
                print_error_in_file file e.start.pos_cnum e.finish.pos_cnum;
                print_string "[0m'"
            )

            | ExpectedCallspec -> (
                print_newline ();
                print_string "Expected number, '*', or ')', found: [1;37m'";
                print_error_in_file file e.start.pos_cnum e.finish.pos_cnum;
                print_string "'[0m"
            )

            | ExpectedRParen -> (
                print_newline ();
                print_string "Expected ')', found: [1;37m'";
                print_error_in_file file e.start.pos_cnum e.finish.pos_cnum;
                print_string "'[0m"
            )
        end;

        print_newline ();

        In_channel.seek file (Int64.of_int e.start.pos_bol);
        begin match In_channel.input_line ?fix_win_eol:(Some true) file with
            | Some line -> (
                print_newline ();
                print_endline ("[1;26m" ^ line ^ "[0m");
                print_endline ((String.make (e.start.pos_cnum - e.start.pos_bol) ' ') ^ "[1;31m^[0m")
            )

            | None -> ()
        end;

        print_newline ();
        print_errors file es;
    )
