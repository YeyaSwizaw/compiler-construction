(* Compiler Construction - test.ml *)
(* Samuel Sleight *)

open List
open Core.Std
open Syntax
open Errors
open Lexing

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
        | Value (String s) -> "Str[" ^ s ^ "] " ^ (stringify es)
        | Value (Function (args, es')) -> "Val[" ^ (String.concat ~sep:" -> " args) ^ " -> {" ^ (stringify es') ^ "}] " ^ (stringify es)

        | Op Minus -> "Op[-] " ^ (stringify es)
        | Op Divide -> "Op[/] " ^ (stringify es)
        | Op Plus -> "Op[+] " ^ (stringify es)
        | Op Star -> "Op[*] " ^ (stringify es)

        | Apply (Partial n) -> "Apply[" ^ (string_of_int n) ^ " ]" ^ (stringify es)
        | Apply Full -> "Apply[] " ^ (stringify es)
        | Apply Total -> "Apply[*] " ^ (stringify es)

        | Assignment (name, v) -> "Var[" ^ name ^ ":" ^ (stringify [Value v]) ^ "] " ^ (stringify es)

let rec stringify_errs = function
    | [] -> ""
    | ((sl, sc), (fl, fc)) :: errs -> "Err[" ^ (string_of_int sl) ^ ":" ^ (string_of_int sc) ^ "-" ^ (string_of_int fc) ^ "] " ^ (stringify_errs errs)

let simple_test code output =
    let fail_f res exp () = 
        "    Input: " ^ code 
        ^ "\n    Expected: " ^ (match exp with
            | Ok(es) -> stringify es
            | Err(errs) -> stringify_errs errs
        )

        ^ "\n    Actual: " ^ (match res with
            | Ok(es) -> stringify es
            | Err(errs) -> stringify_errs errs        
        )
    in

    let test_f () =
        let test_filename = "_test.sfl" in
        let test_file = Out_channel.create test_filename in
        Out_channel.output_string test_file (code);
        Out_channel.close test_file;

        let test_file = In_channel.create test_filename in
        let result = ref (Ok []) in

        begin match Compiler.run ~parser_callback:(fun res -> result := Ok(res)) test_file with
            | Ok(()) -> ()
            | Err(errs) -> result := Err (map (fun err -> match err with
                | UnterminatedLBrace p
                | UnterminatedString p -> ((p.pos_lnum, p.pos_cnum - p.pos_bol), (p.pos_lnum, p.pos_cnum - p.pos_bol))
                | SyntaxError (sp, fp, _) -> ((sp.pos_lnum, sp.pos_cnum - sp.pos_bol), (fp.pos_lnum, fp.pos_cnum - fp.pos_bol))
            ) errs)
        end;

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
                    print_string "\r[1;37m[Test ";
                    print_int n;
                    print_string "][0m Passed";
                    flush stdout
                )

                | Failure f -> (
                    print_string "\r[1;31m[Test ";
                    print_int n;
                    print_endline "][0m Failed";
                    print_endline (f ());
                    print_newline ()
                )
            end;

            check_tests (n + 1) tests
        )
    in

    let tests = [
        simple_test "" (Ok []);

        simple_test "5" (Ok [Value (Int 5)]);
        simple_test "1355" (Ok [Value (Int 1355)]);
        simple_test "e" (Ok [Value (Ident "e")]);
        simple_test "apple" (Ok [Value (Ident "apple")]);
        simple_test "a54fd32le" (Ok [Value (Ident "a54fd32le")]);
        simple_test "\"hello there\"" (Ok [Value (String "hello there")]);

        simple_test "+" (Ok [Op Plus]);
        simple_test "-" (Ok [Op Minus]);
        simple_test "*" (Ok [Op Star]);
        simple_test "/" (Ok [Op Divide]);

        simple_test "()" (Ok [Apply Full]);
        simple_test "(*)" (Ok [Apply Total]);
        simple_test "(8)" (Ok [Apply (Partial 8)]);
        simple_test "(815)" (Ok [Apply (Partial 815)]);

        simple_test "r: 17" (Ok [Assignment ("r", (Int 17))]);
        simple_test "corn: bacon" (Ok [Assignment ("corn", (Ident "bacon"))]);

        simple_test "{}" (Ok [Value (Function ([], []))]);
        simple_test "{2}" (Ok [Value (Function ([], [Value (Int 2)]))]);
        simple_test "{\n2\n}" (Ok [Value (Function ([], [Value (Int 2)]))]);
        simple_test "{146}" (Ok [Value (Function ([], [Value (Int 146)]))]);
        simple_test "{beige}" (Ok [Value (Function ([], [Value (Ident "beige")]))]);
        simple_test "a -> {14}" (Ok [Value (Function (["a"], [Value (Int 14)]))]);
        simple_test "bard -> a -> {\na bard\n}" (Ok [Value (Function (["bard"; "a"], [Value (Ident "a"); Value (Ident "bard")]))]);

        simple_test "475 cord" (Ok [Value (Int 475); Value (Ident "cord")]);
        simple_test "84\n856\n+" (Ok [Value (Int 84); Value (Int 856); Op Plus]);
        simple_test "74\ndapper\n*\n()" (Ok [Value (Int 74); Value (Ident "dapper"); Op Star; Apply Full]);

        simple_test "15 arc\nswap: a -> b -> {\n    a b\n}\nswap (*)" (Ok [Value (Int 15); Value (Ident "arc"); Assignment ("swap", Function (["a"; "b"], [Value (Ident "a"); Value (Ident "b")])); Value (Ident "swap"); Apply Total]);

        simple_test ";" (Err [(1, 0), (1, 1)]);
        simple_test "1\n(corn" (Err [(2, 1), (2, 5)]);
        simple_test "12 13 ~\n(" (Err [(1, 6), (1, 7); (2, 1), (2, 1)]);
        simple_test "56\n45 \"hello\n\narc" (Err [(2, 3), (2, 3)]);
        simple_test "{\n6" (Err [(1, 0), (1, 0)]);
        simple_test "6\na -> {" (Err [(2, 5), (2, 5)]);
    ] in

    check_tests 1 tests

