let fib n = 
    let rec fib_rec n acc1 acc2 = if n == 0 then
        acc2
    else
        fib_rec (n - 1) (acc1 + acc2) acc1
    in

    fib_rec n 0 1

let () =
    print_endline (string_of_int (fib 47));
