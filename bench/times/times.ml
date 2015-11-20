let add_five x = x + 5

let rec times n f x =
    if n = 0 then
        x
    else
        times (n - 1) f (f x)

let () =
    print_endline (string_of_int (times 1000 add_five 0))
