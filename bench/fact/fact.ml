let fact n =
    let rec fact' n acc = 
        if n = 0 then 
            acc 
        else 
            fact' (n - 1) (acc * n)
    in

    fact' n 1

let () =
    for i = 1 to 620 do
        print_string (string_of_int (fact 12));
    done;
    print_newline ();
