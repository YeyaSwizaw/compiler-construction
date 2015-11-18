open List
open Core.Std
open Testutil
open Instr
open Errors
open Lexing

let instr_test ?(cf=true) code output =
    let fail_f res exp () = 
        "    Input: " ^ code 
        ^ "\n    Expected: " ^ (match exp with
            | Ok(es) -> es
            | Err(errs) -> stringify_errs errs
        )

        ^ "\n    Actual: " ^ (match res with
            | Ok(es) -> es
            | Err(errs) -> stringify_errs errs        
        )
    in

    let test_f () = 
        let test_filename = "_test.sfl" in
        let test_file = Out_channel.create test_filename in
        Out_channel.output_string test_file (code);
        Out_channel.close test_file;

        let test_file = In_channel.create test_filename in
        let result = ref (Ok "") in

        let opt_flags = {
            Flag.default_opt_flags with
                Flag.cf = cf;
        } in

        begin match Compiler.run ~instr_callback:(fun prog -> result := Ok (string_of_fns prog); false) ~opt_flags:opt_flags test_file with
            | Ok(()) -> ()
            | Err(errs) -> result := Err (map (fun err -> match err with
                | UndefinedName (_, p)
                | RedefinedName (_, _, p)
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

let run () = 
    let tests = [
        instr_test "1" (Ok ":[0]:Push[Int[1]]\n");
        instr_test "\"hello\"" (Ok ":[0]:Push[Str[hello]]\n");
        instr_test "'x'" (Ok ":[0]:Push[Char[x]]\n");

        instr_test "+" (Ok ":[0]:Push[Fn[+]]\n");
        instr_test "-" (Ok ":[0]:Push[Fn[-]]\n");
        instr_test "*" (Ok ":[0]:Push[Fn[*]]\n");
        instr_test "/" (Ok ":[0]:Push[Fn[/]]\n");
        instr_test "=" (Ok ":[0]:Push[Fn[=]]\n");
        instr_test ">" (Ok ":[0]:Push[Fn[>]]\n");
        instr_test "<" (Ok ":[0]:Push[Fn[<]]\n");

        instr_test "()" (Ok ":[0]:Apply[]\n");

        instr_test "num\nnum: 7" (Ok ":[0]:Push[Int[7]]\n");

        (* Simple constant folding *)
        instr_test "2 9 +" (Ok ":[0]:Push[Fn[+]]Push[Int[9]]Push[Int[2]]\n");
        instr_test "2 9 + ()" (Ok ":[0]:Push[Int[11]]\n");
        instr_test "14 13 * ()" (Ok ":[0]:Push[Int[182]]\n");
        instr_test "2 6 / ()" (Ok ":[0]:Push[Int[3]]\n");
        instr_test "6 1 - ()" (Ok ":[0]:Push[Int[-5]]\n");
        instr_test "2 9 11 + () / ()" (Ok ":[0]:Push[Int[10]]\n");

        instr_test ~cf:false "2 9 +" (Ok ":[0]:Push[Fn[+]]Push[Int[9]]Push[Int[2]]\n");
        instr_test ~cf:false "2 9 + ()" (Ok ":[0]:Apply[]Push[Fn[+]]Push[Int[9]]Push[Int[2]]\n");
        instr_test ~cf:false "14 13 * ()" (Ok ":[0]:Apply[]Push[Fn[*]]Push[Int[13]]Push[Int[14]]\n");
        instr_test ~cf:false "2 6 / ()" (Ok ":[0]:Apply[]Push[Fn[/]]Push[Int[6]]Push[Int[2]]\n");
        instr_test ~cf:false "6 1 - ()" (Ok ":[0]:Apply[]Push[Fn[-]]Push[Int[1]]Push[Int[6]]\n");
        instr_test ~cf:false "2 9 11 + () / ()" (Ok ":[0]:Apply[]Push[Fn[/]]Apply[]Push[Fn[+]]Push[Int[11]]Push[Int[9]]Push[Int[2]]\n");

        (* Functions *)
        instr_test "a -> { a }" (Ok ":[]:Push[Fn[anon1]]\nanon1:[a]:Push[Arg[a]]\n");
        instr_test "fun: a -> { a }\nfun" (Ok ":[]:Push[Fn[fun1]]\nfun1:[a]:Push[Arg[a]]\n");
        instr_test "fun: a -> { a 1 2 + () + () }\n5 fun ()" (Ok ":[]:Apply[]Push[Fn[fun1]]Push[Int[5]]\nfun1:[a]:Apply[]Push[Fn[+]]Push[Int[3]]Push[Arg[a]]\n");
        instr_test "fun: a -> { a fun () }\n1 fun ()" (Ok ":[]:Apply[]Push[Fn[fun1]]Push[Int[1]]\nfun1:[a]:Apply[]Push[Self]Push[Arg[a]]\n");
        instr_test "fun: a -> b -> c -> {}\nfun ()" (Ok ":[]:Apply[]Push[Fn[fun1]]\nfun1:[a, b, c]:\n");

        instr_test ~cf:false "a -> { a }" (Ok ":[]:Push[Fn[anon1]]\nanon1:[a]:Push[Arg[a]]\n");
        instr_test ~cf:false "fun: a -> { a }\nfun" (Ok ":[]:Push[Fn[fun1]]\nfun1:[a]:Push[Arg[a]]\n");
        instr_test ~cf:false "fun: a -> { a 1 2 + () + () }\n5 fun ()" (Ok ":[]:Apply[]Push[Fn[fun1]]Push[Int[5]]\nfun1:[a]:Apply[]Push[Fn[+]]Apply[]Push[Fn[+]]Push[Int[2]]Push[Int[1]]Push[Arg[a]]\n");
        instr_test ~cf:false "fun: a -> { a fun () }\n1 fun ()" (Ok ":[]:Apply[]Push[Fn[fun1]]Push[Int[1]]\nfun1:[a]:Apply[]Push[Self]Push[Arg[a]]\n");
        instr_test ~cf:false "fun: a -> b -> c -> {}\nfun ()" (Ok ":[]:Apply[]Push[Fn[fun1]]\nfun1:[a, b, c]:\n");

        instr_test "a" (Err [(1, 0), (1, 0)]);
        instr_test "a -> { b }" (Err [(1, 7), (1, 7)]);
        instr_test "a -> { b }\na -> { b: 5 }" (Err [(1, 7), (1, 7)]);
    ] in

    check_tests "Instr " 1 tests;

