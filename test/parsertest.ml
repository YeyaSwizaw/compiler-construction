open List
open Core.Std
open Testutil
open Syntax
open Errors
open Lexing

(* Run parser test on given code *)
let parser_test code output =
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

        begin match Compiler.run ~parser_callback:(fun prog -> result := Ok (string_of_prog prog); false) test_file with
            | Ok(()) -> ()
            | Err(errs) -> result := Err (map (fun err -> match err with
                | NotImplemented p
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
        parser_test "" (Ok "");

        parser_test "5" (Ok "Int[5]");
        parser_test "1355" (Ok "Int[1355]");
        parser_test "'c'" (Ok "Char[c]");
        parser_test "'\\n'" (Ok "Char[\n]");
        parser_test "e" (Ok "Id[e]");
        parser_test "apple" (Ok "Id[apple]");
        parser_test "a54fd32le" (Ok "Id[a54fd32le]");
        parser_test "\"hello there\"" (Ok "Str[hello there]");
        parser_test "\"apple\npie\"" (Ok "Str[apple\npie]");

        parser_test "+" (Ok "Op[+]");
        parser_test "-" (Ok "Op[-]");
        parser_test "*" (Ok "Op[*]");
        parser_test "/" (Ok "Op[/]");
        parser_test "<" (Ok "Op[<]");
        parser_test ">" (Ok "Op[>]");
        parser_test "=" (Ok "Op[=]");
        parser_test "?" (Ok "Op[?]");

        parser_test "()" (Ok "Apply[]");
        parser_test "(*)" (Ok "Apply[*]");
        parser_test "(8)" (Ok "Apply[8]");
        parser_test "(815)" (Ok "Apply[815]");

        parser_test "." (Ok "Write[Int]");

        parser_test "r: 17" (Ok "Name[r:Int[17]]");
        parser_test "corn: bacon" (Ok "Name[corn:Id[bacon]]");

        parser_test "{}" (Ok "Fn[{}]");
        parser_test "{2}" (Ok "Fn[{Int[2]}]");
        parser_test "{\n2\n}" (Ok "Fn[{Int[2]}]");
        parser_test "{146}" (Ok "Fn[{Int[146]}]");
        parser_test "{beige}" (Ok "Fn[{Id[beige]}]");
        parser_test "a -> {14}" (Ok "Fn[a -> {Int[14]}]");
        parser_test "bard -> a -> {\na bard\n}" (Ok "Fn[bard -> a -> {Id[a]Id[bard]}]");
        parser_test "a -> { b: 5 }" (Ok "Fn[a -> {Name[b:Int[5]]}]");

        parser_test "475 cord" (Ok "Int[475]Id[cord]");
        parser_test "84\n856\n+" (Ok "Int[84]Int[856]Op[+]");
        parser_test "74\ndapper\n*\n()" (Ok "Int[74]Id[dapper]Op[*]Apply[]");
        parser_test "15 arc\nswap: a -> b -> {\n    a b\n}\nswap (*)" (Ok "Name[swap:Fn[a -> b -> {Id[a]Id[b]}]]Int[15]Id[arc]Id[swap]Apply[*]");

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
        parser_test "a: 5\nb: 7\na: 12" (Err [(3, 0), (3, 0)]);
    ] in

    check_tests "Parser " 1 tests;
