open Core.Std
open Syntax

type 'a test_result_t =
    | Success
    | Failure of 'a

let test_expect fail_f test_f expected = 
    fun () -> (
        let result = test_f () in
        if result = expected then Success else Failure (fail_f result expected)
    )

let rec stringify = function
    | [] -> ""
    | expr :: es -> match expr with
        | Value (Int n) -> "Val[" ^ (string_of_int n) ^ "] " ^ (stringify es)
        | Value (Ident s) -> "Val[" ^ s ^ "] " ^ (stringify es)
        | Value (Function (args, es')) -> "Val[" ^ (String.concat ~sep:" -> " args) ^ " -> {" ^ (stringify es') ^ "}] " ^ (stringify es)

        | Op Minus -> "Op[-]" ^ (stringify es)
        | Op Divide -> "Op[/]" ^ (stringify es)
        | Op Plus -> "Op[+]" ^ (stringify es)
        | Op Star -> "Op[*]" ^ (stringify es)

        | _ -> "_" ^ (stringify es)

let correct_test code output =
    let fail_f res exp () = 
        "    Input: " ^ code 
        ^ "\n    Expected: " ^ (stringify exp)
        ^ "\n    Actual: " ^ (stringify res)
    in

    let test_f () =
        let test_filename = "_test.sfl" in
        let test_file = Out_channel.create test_filename in
        Out_channel.output_string test_file (code);
        Out_channel.close test_file;

        let test_file = In_channel.create test_filename in
        let result = ref [] in

        Compiler.run ~parser_callback:(fun res -> result := res) test_file;

        In_channel.close test_file;
        Sys.remove test_filename;

        !result
    in

    test_expect fail_f test_f output

let () =
    let rec check_tests n = function
        | [] -> ()
        | test::tests -> (
            begin match test () with
                | Success -> (
                    print_string "[1;37m[Test ";
                    print_int n;
                    print_string "][0m Passed";
                )

                | Failure f -> (
                    print_string "[1;31m[Test ";
                    print_int n;
                    print_endline "][0m";
                    print_string (f ())
                )
            end;

            print_newline ();
            check_tests (n + 1) tests
        )
    in

    let tests = [
        correct_test "5" [Syntax.Value (Syntax.Int 5)];
        correct_test "apple" [Syntax.Value (Syntax.Int 5)]
    ] in

    check_tests 1 tests

