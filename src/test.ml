type 'a test_result_t =
    | Success
    | Failure of 'a

let test_expect fail_f f expected = 
    fun () -> (
        let result = f () in
        if result == expected then Success else Failure (fail_f result expected)
    )

let simple_test str =
    test_expect 
        (fun res exp () -> (
            "    Expected: " ^ exp ^ "\n" ^
            "    Got: " ^ res ^ "\n"
        ))
        (fun () -> str ^ "a")
        str

let () =
    let rec check_tests n = function
        | [] -> ()
        | test::tests -> (
            begin match test () with
                | Success -> ()
                | Failure f -> (
                    print_string "[1;32m[Test ";
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
        simple_test "hello";
        simple_test "world";
    ] in

    check_tests 1 tests

