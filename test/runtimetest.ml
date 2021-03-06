open List
open Core.Std
open Testutil
open Instr
open Errors
open Lexing

(* http://rosettacode.org/wiki/Execute_a_system_command#OCaml *)
let execute_and_get_stdout cmd =
    let (ic, oc) = Unix.open_process cmd in
    let buf = Buffer.create 4 in
    begin try
        while true do
            Buffer.add_channel buf ic 1
        done
    with End_of_file -> ()
    end;
    let _ = Unix.close_process (ic, oc) in
    Buffer.contents buf

let runtime_test ?(fe=false) code output =
    let fail_f res exp () =
        "    Input: " ^ code
        ^ "\n    Expected: " ^ exp
        ^ "\n    Actual: " ^ res
    in

    let test_f () =
        let test_filename = "_test.sfl" in
        let test_outfile = "_test.out" in
        let test_file = Out_channel.create test_filename in
        Out_channel.output_string test_file (code);
        Out_channel.close test_file;

        let opt_flags = { 
            Flag.default_opt_flags with
                Flag.fe=fe;
        } in

        let test_file = In_channel.create test_filename in
        begin match Compiler.run ~output:test_outfile ~opt_flags:opt_flags test_file with
            | Ok(()) -> begin
                let result = execute_and_get_stdout ("./" ^ test_outfile) in
                Sys.remove test_outfile;
                Sys.remove test_filename;
                result
            end

            | Err(_) -> begin
                Sys.remove test_filename;
                ""
            end
        end
    in

    test_expect fail_f test_f output

let run () =
    let tests = [
        runtime_test "1 ." "1";
        runtime_test "5 7 ." "7";
        runtime_test "number .\nnumber: 13" "13";
        runtime_test "'f' ." "102";
        runtime_test "'f' ," "f";
        runtime_test "6 3 + () ." "9";
        runtime_test "2 11 * () ." "22";
        runtime_test "15 5 - () ." "-10";
        runtime_test "2 8 / () ." "4";
        runtime_test "2 3 1 ? () ." "3";
        runtime_test "2 3 0 ? () ." "2";
        runtime_test "5 2 < () ." "1";
        runtime_test "1 7 < () ." "0";
        runtime_test "5 2 > () ." "0";
        runtime_test "1 7 > () ." "1";
        runtime_test "5 5 = () ." "1";
        runtime_test "5 2 = () ." "0";
        runtime_test "3 6 = () ." "0";
        runtime_test "17 4 add () .\nadd: a -> b -> { a b + () }" "21";
        runtime_test "add: x -> y -> { x y + () }\napply: f -> x -> { x x f () }\n3 add apply () ." "6";

        runtime_test ~fe:true "17 4 add () .\nadd: a -> b -> { a b + () }" "21";
        runtime_test ~fe:true "add: x -> y -> { x y + () }\napply: f -> x -> { x x f () }\n3 add apply () ." "6";
    ] in

    check_tests "Runtime " 1 tests;
