open List
open Core.Std
open Testutil
open Instr
open Errors
open Lexing

let instr_test code output =
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

        begin match Compiler.run ~instr_callback:(fun prog -> result := Ok (string_of_fns prog); false) test_file with
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
        instr_test "1" (Ok ":Push[Int[1]]\n");
        instr_test "\"hello\"" (Ok ":Push[Str[hello]]\n");
        instr_test "'x'" (Ok ":Push[Char[x]]\n");

        instr_test "+" (Ok ":Push[Fn[+]]\n");
        instr_test "-" (Ok ":Push[Fn[-]]\n");
        instr_test "*" (Ok ":Push[Fn[*]]\n");
        instr_test "/" (Ok ":Push[Fn[/]]\n");
        instr_test "=" (Ok ":Push[Fn[=]]\n");
        instr_test ">" (Ok ":Push[Fn[>]]\n");
        instr_test "<" (Ok ":Push[Fn[<]]\n");

        instr_test "()" (Ok ":Apply[]\n");

        instr_test "num\nnum: 7" (Ok ":Push[Int[7]]\n");

        (* Simple constant folding *)
        instr_test "2 9 +" (Ok ":Push[Fn[+]]Push[Int[9]]Push[Int[2]]\n");
        instr_test "2 9 + ()" (Ok ":Push[Int[11]]\n");
        instr_test "14 13 * ()" (Ok ":Push[Int[182]]\n");
        instr_test "2 6 / ()" (Ok ":Push[Int[3]]\n");
        instr_test "6 1 - ()" (Ok ":Push[Int[-5]]\n");
        instr_test "2 9 11 + () / ()" (Ok ":Push[Int[10]]\n");

        (* Functions *)
        instr_test "a -> { a }" (Ok ":Push[Fn[1]]\n1:Push[Arg[a]]\n");
        instr_test "fun: a -> { a }\nfun" (Ok ":Push[Fn[fun1]]\nfun1:Push[Arg[a]]\n");
        instr_test "fun: a -> { a 1 2 + () + () }\n5 fun ()" (Ok ":Apply[]Push[Fn[fun1]]Push[Int[5]]\nfun1:Apply[]Push[Fn[+]]Push[Int[3]]Push[Arg[a]]\n");
        instr_test "fun: a -> { a fun () }\n1 fun ()" (Ok ":Apply[]Push[Fn[fun1]]Push[Int[1]]\nfun1:Apply[]Push[Self]Push[Arg[a]]\n");

        instr_test "a" (Err [(1, 0), (1, 0)]);
        instr_test "a -> { b }" (Err [(1, 7), (1, 7)]);
    ] in

    check_tests "Instr " 1 tests;

