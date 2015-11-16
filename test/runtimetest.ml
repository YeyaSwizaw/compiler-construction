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

let runtime_test ?(cf=true) code output =
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
                Flag.cf=cf
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
        runtime_test "1" "1\n";
        runtime_test "5 7" "7\n";
        runtime_test "number\nnumber: 13" "13\n";
        runtime_test "6 3 + ()" "9\n";
        runtime_test "2 11 * ()" "22\n";
        runtime_test "15 5 - ()" "-10\n";
        runtime_test "2 8 / ()" "4\n";
        runtime_test "2 3 1 ? ()" "3\n";
        runtime_test "2 3 0 ? ()" "2\n";
        runtime_test "5 2 < ()" "1\n";
        runtime_test "1 7 < ()" "0\n";
        runtime_test "5 2 > ()" "0\n";
        runtime_test "1 7 > ()" "1\n";
        runtime_test "5 5 = ()" "1\n";
        runtime_test "5 2 = ()" "0\n";
        runtime_test "3 6 = ()" "0\n";
        runtime_test "17 4 add ()\nadd: a -> b -> { a b + () }" "21\n";

        runtime_test ~cf:false "1" "1\n";
        runtime_test ~cf:false "5 7" "7\n";
        runtime_test ~cf:false "number\nnumber: 13" "13\n";
        runtime_test ~cf:false "6 3 + ()" "9\n";
        runtime_test ~cf:false "2 11 * ()" "22\n";
        runtime_test ~cf:false "15 5 - ()" "-10\n";
        runtime_test ~cf:false "2 8 / ()" "4\n";
        runtime_test ~cf:false "2 3 1 ? ()" "3\n";
        runtime_test ~cf:false "2 3 0 ? ()" "2\n";
        runtime_test ~cf:false "5 2 < ()" "1\n";
        runtime_test ~cf:false "1 7 < ()" "0\n";
        runtime_test ~cf:false "5 2 > ()" "0\n";
        runtime_test ~cf:false "1 7 > ()" "1\n";
        runtime_test ~cf:false "5 5 = ()" "1\n";
        runtime_test ~cf:false "5 2 = ()" "0\n";
        runtime_test ~cf:false "3 6 = ()" "0\n";
        runtime_test ~cf:false "17 4 add ()\nadd: a -> b -> { a b + () }" "21\n";
    ] in

    check_tests "Runtime " 1 tests;
