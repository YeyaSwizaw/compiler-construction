(* Compiler Construction - test.ml *)
(* Samuel Sleight *)

open List
open Core.Std
open Syntax
open Errors
open Lexing

module AT = ANSITerminal

type 'a test_result_t =
    | Success
    | Failure of 'a

(* Generic test function *)
let test_expect fail_f test_f expected = 
    fun () -> (
        let result = test_f () in
        if result = expected then Success else Failure (fail_f result expected)
    )

(* Print test result expression *)
let rec stringify_prog prog = (stringify_env (Syntax.Env.bindings prog.env)) ^ "\n" ^ (stringify prog.code)

and stringify_env = function
    | [] -> ""
    | (name, value) :: items -> name ^ ": " ^ (
            match value with
                | Int n -> "Val[" ^ (string_of_int n) ^ "] " ^ (stringify_env items)
                | Char c -> "Char[" ^ (String.make 1 c) ^ "] " ^ (stringify_env items)
                | Ident s -> "Val[" ^ s ^ "] " ^ (stringify_env items)
                | String s -> "Str[" ^ s ^ "] " ^ (stringify_env items)
                | Function (args, es') -> "Fn[" ^ (String.concat ~sep:" -> " args) ^ " -> {" ^ (stringify_prog es') ^ "}] " ^ (stringify_env items)
    )

and stringify = function
    | [] -> ""
    | expr :: es -> match expr with
        | Value (Int n) -> "Val[" ^ (string_of_int n) ^ "] " ^ (stringify es)
        | Value (Char c) -> "Char[" ^ (String.make 1 c) ^ "] " ^ (stringify es)
        | Value (Ident s) -> "Val[" ^ s ^ "] " ^ (stringify es)
        | Value (String s) -> "Str[" ^ s ^ "] " ^ (stringify es)
        | Value (Function (args, es')) -> "Fn[" ^ (String.concat ~sep:" -> " args) ^ " -> {" ^ (stringify_prog es') ^ "}] " ^ (stringify es)

        | Op Minus -> "Op[-] " ^ (stringify es)
        | Op Divide -> "Op[/] " ^ (stringify es)
        | Op Plus -> "Op[+] " ^ (stringify es)
        | Op Star -> "Op[*] " ^ (stringify es)
        | Op Lt -> "Op[<] " ^ (stringify es)
        | Op Gt -> "Op[>] " ^ (stringify es)
        | Op Eq -> "Op[=] " ^ (stringify es)
        | Op IfThenElse -> "Op[?] " ^ (stringify es)

        | Apply (Partial n) -> "Apply[" ^ (string_of_int n) ^ " ]" ^ (stringify es)
        | Apply Full -> "Apply[] " ^ (stringify es)
        | Apply Total -> "Apply[*] " ^ (stringify es)

let rec stringify_errs = function
    | [] -> ""
    | ((sl, sc), (fl, fc)) :: errs -> "Err[" ^ (string_of_int sl) ^ ":" ^ (string_of_int sc) ^ "-" ^ (string_of_int fc) ^ "] " ^ (stringify_errs errs)

(* Run parser test on given code *)
let parser_test code output =
    let fail_f res exp () = 
        "    Input: " ^ code 
        ^ "\n    Expected: " ^ (match exp with
            | Ok(es) -> stringify_prog es
            | Err(errs) -> stringify_errs errs
        )

        ^ "\n    Actual: " ^ (match res with
            | Ok(es) -> stringify_prog es
            | Err(errs) -> stringify_errs errs        
        )
    in

    let test_f () =
        let test_filename = "_test.sfl" in
        let test_file = Out_channel.create test_filename in
        Out_channel.output_string test_file (code);
        Out_channel.close test_file;

        let test_file = In_channel.create test_filename in
        let result = ref (Ok {env=Syntax.Env.empty; code=[]}) in

        begin match Compiler.run ~parser_callback:(fun prog -> result := Ok prog) test_file with
            | Ok(()) -> ()
            | Err(errs) -> result := Err (map (fun err -> match err with
                | RedefinedName (_, p)
                | UnterminatedLBrace p
                | UnterminatedChar p
                | UnterminatedString p
                | InvalidCallspec p -> ((p.pos_lnum, p.pos_cnum - p.pos_bol), (p.pos_lnum, p.pos_cnum - p.pos_bol))
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
            try
                begin match test () with
                    | Success -> (
                        AT.restore_cursor ();

                        AT.print_string [AT.Bold; AT.green] ("[Test " ^ (string_of_int n) ^ "]");
                        print_string " Passed";
                        AT.erase AT.Eol;
                    )

                    | Failure f -> (
                        AT.print_string [AT.Bold; AT.red] ("[Test " ^ (string_of_int n) ^ "]");
                        print_string " Failed";
                        AT.erase AT.Eol;
                        print_newline ();
                        print_endline (f ());
                        print_newline ();

                        AT.save_cursor ();
                    )
                end;

                check_tests (n + 1) tests
            with
                e -> (
                    AT.print_string [AT.Bold; AT.red] ("[Test " ^ (string_of_int n) ^ "]");
                    print_string (" Threw: " ^ (Sexp.to_string_hum (sexp_of_exn e)));
                    AT.erase AT.Eol;
                    print_newline ();

                    AT.save_cursor ();
                    check_tests (n + 1) tests
                )
        );
    in

    let from_list = fold_left (fun acc (name, value) -> Syntax.Env.add name value acc) Syntax.Env.empty in

    let tests = [
        parser_test "" (Ok {env=from_list []; code=[]});

        parser_test "5" (Ok {env=from_list []; code=[Value (Int 5)]});
        parser_test "1355" (Ok {env=from_list []; code=[Value (Int 1355)]});
        parser_test "'c'" (Ok {env=from_list []; code=[Value (Char 'c')]});
        parser_test "e" (Ok {env=from_list []; code=[Value (Ident "e")]});
        parser_test "apple" (Ok {env=from_list []; code=[Value (Ident "apple")]});
        parser_test "a54fd32le" (Ok {env=from_list []; code=[Value (Ident "a54fd32le")]});
        parser_test "\"hello there\"" (Ok {env=from_list []; code=[Value (String "hello there")]});
        parser_test "\"apple\npie\"" (Ok {env=from_list []; code=[Value (String "apple\npie")]});

        parser_test "+" (Ok {env=from_list []; code=[Op Plus]});
        parser_test "-" (Ok {env=from_list []; code=[Op Minus]});
        parser_test "*" (Ok {env=from_list []; code=[Op Star]});
        parser_test "/" (Ok {env=from_list []; code=[Op Divide]});
        parser_test "<" (Ok {env=from_list []; code=[Op Lt]});
        parser_test ">" (Ok {env=from_list []; code=[Op Gt]});
        parser_test "=" (Ok {env=from_list []; code=[Op Eq]});
        parser_test "?" (Ok {env=from_list []; code=[Op IfThenElse]});

        parser_test "()" (Ok {env=from_list []; code=[Apply Full]});
        parser_test "(*)" (Ok {env=from_list []; code=[Apply Total]});
        parser_test "(8)" (Ok {env=from_list []; code=[Apply (Partial 8)]});
        parser_test "(815)" (Ok {env=from_list []; code=[Apply (Partial 815)]});

        parser_test "r: 17" (Ok {env=from_list ["r", Int 17]; code=[]});
        parser_test "corn: bacon" (Ok {env=from_list ["corn", Ident "bacon"]; code=[]});

        parser_test "{}" (Ok {env=from_list []; code=[Value (Function ([], {env=from_list []; code=[]}))]});
        parser_test "{2}" (Ok {env=from_list []; code=[Value (Function ([], {env=from_list []; code=[Value (Int 2)]}))]});
        parser_test "{\n2\n}" (Ok {env=from_list []; code=[Value (Function ([], {env=from_list []; code=[Value (Int 2)]}))]});
        parser_test "{146}" (Ok {env=from_list []; code=[Value (Function ([], {env=from_list []; code=[Value (Int 146)]}))]});
        parser_test "{beige}" (Ok {env=from_list []; code=[Value (Function ([], {env=from_list []; code=[Value (Ident "beige")]}))]});
        parser_test "a -> {14}" (Ok {env=from_list []; code=[Value (Function (["a"], {env=from_list []; code=[Value (Int 14)]}))]});
        parser_test "bard -> a -> {\na bard\n}" (Ok {env=from_list []; code=[Value (Function (["bard"; "a"], {env=from_list []; code=[Value (Ident "a"); Value (Ident "bard")]}))]});
        parser_test "a -> { b: 5 }" (Ok {env=from_list []; code=[Value (Function (["a"], {env=from_list ["b", Int 5]; code=[]}))]});

        parser_test "475 cord" (Ok {env=from_list []; code=[Value (Int 475); Value (Ident "cord")]});
        parser_test "84\n856\n+" (Ok {env=from_list []; code=[Value (Int 84); Value (Int 856); Op Plus]});
        parser_test "74\ndapper\n*\n()" (Ok {env=from_list []; code=[Value (Int 74); Value (Ident "dapper"); Op Star; Apply Full]});
        parser_test "15 arc\nswap: a -> b -> {\n    a b\n}\nswap (*)" (Ok {env=from_list ["swap", Function (["a"; "b"], {env=from_list []; code=[Value (Ident "a"); Value (Ident "b")]})]; code=[Value (Int 15); Value (Ident "arc"); Value (Ident "swap"); Apply Total]});

        parser_test ";" (Err [(1, 0), (1, 1)]);
        parser_test "1\n(corn" (Err [(2, 0), (2, 0)]);
        parser_test "'ab'" (Err [(1, 0), (1, 0); (1, 3), (1, 3)]);
        parser_test "'\n15 \"a" (Err [(1, 0), (1, 0); (2, 3), (2, 3)]);
        parser_test "12 13 ~\n(" (Err [(1, 6), (1, 7); (2, 0), (2, 0)]);
        parser_test "56\n45 \"hello\n\narc" (Err [(2, 3), (2, 3)]);
        parser_test "{\n6" (Err [(1, 0), (1, 0)]);
        parser_test "6\na -> {" (Err [(2, 5), (2, 5)]);
        parser_test "(" (Err [(1, 0), (1, 0)]);
        parser_test "54;\na -> {\n  5 4 + ( );\n}" (Err [(1, 2), (1, 3); (3, 8), (3, 8); (3, 10), (3, 11); (3, 11), (3, 12)]);
        parser_test "a: 5\nb: 7\na: 12" (Err [(1, 0), (1, 0)]);
    ] in

    AT.save_cursor ();
    check_tests 1 tests;
    print_newline ();

