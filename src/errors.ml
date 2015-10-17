(* Compiler Construction - errors.ml *)
(* Samuel Sleight *)

open Core.Std
open Lexing

module AT = ANSITerminal

(* Generic syntax errors *)
type syntax_error_t =
    | ExpectedExpression
    | ExpectedValue

type error =
    | UnterminatedLBrace of Lexing.position
    | UnterminatedString of Lexing.position
    | UnterminatedChar of Lexing.position
    | InvalidCallspec of Lexing.position
    | SyntaxError of Lexing.position * Lexing.position * syntax_error_t

(* Generic result type *)
type ('a, 'b) result =
    | Ok of 'a
    | Err of 'b

type 'a parse_result = ('a, (error list)) result

let unterminated_lbrace p = UnterminatedLBrace p
let unterminated_string p = UnterminatedString p
let unterminated_char p = UnterminatedChar p
let invalid_callspec p = InvalidCallspec p
let expected_expression b e = SyntaxError (b, e, ExpectedExpression)
let expected_value b e = SyntaxError (b, e, ExpectedValue)

(* Helpful error pretty printing functions *)
let string_of_position p = p.pos_fname ^ ":" ^ (string_of_int p.pos_lnum) ^ ":" ^ (string_of_int (p.pos_cnum - p.pos_bol))

(* Print complete error token *)
let string_of_error_in_file file pos epos =
    In_channel.seek file (Int64.of_int pos);

    let buf = Buffer.create 17 in

    let rec print_loop len = 
        begin match In_channel.input_char file with
            | Some c -> Buffer.add_char buf c
            | None -> ()
        end;

        if len > 1 then print_loop (len - 1) else (Buffer.contents buf);
    in

    print_loop (epos - pos)

(* Print line in file with arrow underneath *)
let print_error_location file start = 
    In_channel.seek file (Int64.of_int start.pos_bol);
    match In_channel.input_line ?fix_win_eol:(Some true) file with
        | Some line -> (
            print_newline ();
            AT.print_string [AT.Bold] line;
            print_newline ();
            AT.print_string [AT.Bold; AT.red] ((String.make (start.pos_cnum - start.pos_bol) ' ') ^ "^")
        )

        | None -> ()

(* Pretty print all errors *)
let rec print_errors file = function
    | [] -> ()
    | e :: es -> (
        AT.print_string [AT.Bold; AT.red] "[Error]";

        begin match e with
            | UnterminatedString pos -> (
                AT.print_string [AT.Bold; AT.blue] ("[" ^ string_of_position pos ^ "]");
                print_newline ();
                print_string "Unterminated string literal: expected closing ";
                AT.print_string [AT.Bold] "'\"'";
                print_newline ();
                print_error_location file pos
            )

            | UnterminatedChar pos -> (
                AT.print_string [AT.Bold; AT.blue] ("[" ^ string_of_position pos ^ "]");
                print_newline ();
                print_string "Unterminated character literal: expected closing ";
                AT.print_string [AT.Bold] "'''";
                print_newline ();
                print_error_location file pos
            )

            | UnterminatedLBrace pos -> (
                AT.print_string [AT.Bold; AT.blue] ("[" ^ string_of_position pos ^ "]");
                print_newline ();
                print_string "Unterminated opening brace: expected ";
                AT.print_string [AT.Bold] "'}'";
                print_newline ();
                print_error_location file pos
            )

            | InvalidCallspec pos -> (
                AT.print_string [AT.Bold; AT.blue] ("[" ^ string_of_position pos ^ "]");
                print_newline ();
                print_string "Invalid application: expected ";
                AT.print_string [AT.Bold] "'()'";
                print_string ", ";
                AT.print_string [AT.Bold] "'(*)'";
                print_string ", or";
                AT.print_string [AT.Bold] "'(n)'";
                print_newline ();
                print_error_location file pos
            )

            | SyntaxError (start, finish, ExpectedExpression) -> (
                AT.print_string [AT.Bold; AT.blue] ("[" ^ string_of_position start ^ "]");
                print_newline ();
                print_string "Expected expression, found: ";
                AT.print_string [AT.Bold] ("'" ^ (string_of_error_in_file file start.pos_cnum finish.pos_cnum) ^ "'");
                print_newline ();
                print_error_location file start
            )

            | SyntaxError (start, finish, ExpectedValue) -> (
                AT.print_string [AT.Bold; AT.blue] ("[" ^ string_of_position start ^ "]");
                print_newline ();
                print_string "Expected value, found: ";
                AT.print_string [AT.Bold] ("'" ^ (string_of_error_in_file file start.pos_cnum finish.pos_cnum) ^ "'");
                print_newline ();
                print_error_location file start
            )
        end;

        print_newline ();
        print_errors file es;
    )
