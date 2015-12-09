open List
open Core.Std
open Testutil
open Instr
open Errors
open Lexing

let instr_test ?(fe=false) code output =
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
                Flag.fe = fe;
        } in

        begin match Compiler.run ~instr_callback:(fun prog -> result := Ok (string_of_fns prog); false) ~opt_flags:opt_flags test_file with
            | Ok(()) -> ()
            | Err(errs) -> result := Err (map (fun err -> match err with
                | NotImplemented p
                | NotEnoughArgs p
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
        instr_test "1" (Ok ":0:[Return [1]]\n");
        instr_test "\"hello\"" (Ok ":0:[Return [104:101:108:108:111]]\n");
        instr_test "'\\n'" (Ok ":0:[Return [10]]\n");

        instr_test "+" (Ok ":0:[Return [Add]]\n");
        instr_test "-" (Ok ":0:[Return [Sub]]\n");
        instr_test "*" (Ok ":0:[Return [Mul]]\n");
        instr_test "/" (Ok ":0:[Return [Div]]\n");
        instr_test "=" (Ok ":0:[Return [Eq]]\n");
        instr_test ">" (Ok ":0:[Return [Gt]]\n");
        instr_test "<" (Ok ":0:[Return [Lt]]\n");

        instr_test "num\nnum: 7" (Ok ":0:[Return [7]]\n");

        (* Simple constant folding *)
        instr_test "2 9 +" (Ok ":0:[Return [Add:9:2]]\n");
        instr_test "2 9 + ()" (Ok ":0:[Return [11]]\n");
        instr_test "14 13 * ()" (Ok ":0:[Return [182]]\n");
        instr_test "2 6 / ()" (Ok ":0:[Return [3]]\n");
        instr_test "6 1 - ()" (Ok ":0:[Return [-5]]\n");
        instr_test "2 9 11 + () / ()" (Ok ":0:[Return [10]]\n");
        instr_test "1 '1' + ()" (Ok ":0:[Return [50]]\n");

        (* Functions *)
        instr_test "a -> { a }" (Ok ":0:[Return [_anon_0]]\n_anon_0:1:[Return [Arg[0]]]\n");
        instr_test "fun: a -> { a }\nfun" (Ok ":0:[Return [_fun]]\n_fun:1:[Return [Arg[0]]]\n");
        instr_test "fun: a -> { a 1 2 + () + () }\n5 fun ()" (Ok ":0:[Apply _fun[5]][Return [Stored[0]]]\n_fun:1:[Return [Add[3:Arg[0]]]]\n");
        instr_test ~fe:true "fun: a -> { a 1 2 + () + () }\n5 fun ()" (Ok ":0:[Return [8]]\n_fun:1:[Return [Add[3:Arg[0]]]]\n");
        instr_test "fun: a -> b -> c -> {}\nfun ()" (Ok ":0:[Apply _fun[Pop:Pop:Pop]][Return []]\n_fun:3:[Return []]\n");

        instr_test "fun: a -> { a a + () fun () } 5 fun ()" (Ok ":0:[Apply _fun[5]][Return []]\n_fun:1:[Apply _fun[Add[Arg[0]:Arg[0]]]][Return []]\n");

        instr_test "a: x -> { x x + () }\nb: x -> y -> { y a () }\n5 1 b ()" (Ok ":0:[Apply _b[1:5]][Return [Stored[0]]]\n_a:1:[Return [Add[Arg[0]:Arg[0]]]]\n_b:2:[Apply _a[Arg[1]]][Return [Stored[0]]]\n");
        instr_test ~fe:true "a: x -> { x x + () }\nb: x -> y -> { y a () }\n5 1 b ()" (Ok ":0:[Return [10]]\n_a:1:[Return [Add[Arg[0]:Arg[0]]]]\n_b:2:[Store Arg[1]][Return [Add[Stored[0]:Stored[0]]]]\n");

        (* Write *)
        instr_test "5 ." (Ok ":0:[Write[Int:5]][Return [5]]\n");
        instr_test "line .\nline: 10" (Ok ":0:[Write[Int:10]][Return [10]]\n");
        instr_test "2 4 + () ." (Ok ":0:[Write[Int:6]][Return [6]]\n");
        instr_test "'a' ," (Ok ":0:[Write[Char:97]][Return [97]]\n");

        (* Read *)
        instr_test "~" (Ok ":0:[Return [Read[Char]]]\n");
        instr_test "2 ~ + ()" (Ok ":0:[Return [Add[Read[Char]:2]]]\n");
    ] in

    check_tests "Instr " 1 tests;

