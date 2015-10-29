(* Compiler Construction - sfl.ml *)
(* Samuel Sleight *)

open Core.Std
open Flags

let run filename output opt_flags () =
    try
        (* Open a file *)
        let chan = In_channel.create filename in

        (* Run the compiler *)
        let result = Compiler.run 
            (*~parser_callback:(fun prog -> print_endline (Syntax.string_of_prog prog); true) (* Print parsed expressions *)*)
            (*~instr_callback:(fun code -> print_endline (Instr.string_of_fns code); true) (* Print instructions *)*)
            (*~codegen_callback:(fun code -> print_endline code; true)*)
            ~filename: filename
            ~opt_flags: opt_flags
            ~output: output
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
    Command.Spec.(
        empty 
        +> anon ("filename" %: file)
        +> flag ~aliases:["-o"] "--output" (optional_with_default "a.out" file) ~doc:"filename The output filename"
        +> flag "--disable-cf" no_arg ~doc:" Disable constant folding optimisations"
    )
    (fun filename output disable_cf ->
        run filename output { 
            cf=(not disable_cf) 
        }
    )
  |> Command.run
