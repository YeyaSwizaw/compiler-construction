(* Compiler Construction - sfl.ml *)
(* Samuel Sleight *)

open Core.Std

let run filename () =
    try
        (* Open a file *)
        let chan = In_channel.create filename in

        (* Run the compiler *)
        let result = Compiler.run 
            ~parser_callback:(fun prog -> print_endline (Syntax.string_of_prog prog); true) (* Print parsed expressions *)
            ~instr_callback:(fun code -> print_endline (Instr.string_of_fns code); true) (* Print instructions *)
            ~filename: filename
            chan 
        in

        begin match result with
            | Errors.Ok(()) -> ()
            | Errors.Err(es) -> Errors.print_errors chan es
        end;

        In_channel.close chan
    with
        | Sys_error e -> print_endline "[1;31m[Error][0m Unable to open file"

let () =
  Command.basic ~summary:"Compiler"
    Command.Spec.(empty +> anon ("filename" %: file))
    run 
  |> Command.run
