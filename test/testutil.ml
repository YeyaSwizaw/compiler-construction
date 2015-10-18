open List
open Core.Std
open Syntax
open Errors
open Lexing

type 'a test_result_t =
    | Success
    | Failure of 'a

(* Generic test function *)
let test_expect fail_f test_f expected = 
    fun () -> (
        let result = test_f () in
        if result = expected then Success else Failure (fail_f result expected)
    )

let rec stringify_errs = function
    | [] -> ""
    | ((sl, sc), (fl, fc)) :: errs -> "Err[" ^ (string_of_int sl) ^ ":" ^ (string_of_int sc) ^ "-" ^ (string_of_int fc) ^ "] " ^ (stringify_errs errs)

let rec check_tests name n = function
    | [] -> ()
    | test::tests -> (
        try
            begin match test () with
                | Success -> (
                    AT.restore_cursor ();

                    AT.print_string [AT.Bold; AT.green] ("[" ^ name ^ "Test " ^ (string_of_int n) ^ "]");
                    print_string " Passed";
                    AT.erase AT.Eol;
                )

                | Failure f -> (
                    AT.restore_cursor ();

                    AT.print_string [AT.Bold; AT.red] ("[" ^ name ^ "Test " ^ (string_of_int n) ^ "]");
                    print_string " Failed";
                    AT.erase AT.Eol;
                    print_newline ();
                    print_endline (f ());
                    print_newline ();

                    AT.save_cursor ();
                )
            end;

            check_tests name (n + 1) tests
        with
            e -> (
                AT.restore_cursor ();

                AT.print_string [AT.Bold; AT.red] ("[" ^ name ^ "Test " ^ (string_of_int n) ^ "]");
                print_string (" Threw: " ^ (Sexp.to_string_hum (sexp_of_exn e)));
                AT.erase AT.Eol;
                print_newline ();

                AT.save_cursor ();
                check_tests name (n + 1) tests
            )
    )

