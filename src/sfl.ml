(* Compiler Construction - sfl.ml *)
(* Samuel Sleight *)

open Core.Std
open Flags

let run filename opt_flags () =
    try
        (* Open a file *)
        let chan = In_channel.create filename in

        (* Run the compiler *)
        let result = Compiler.run 
            (*~parser_callback:(fun prog -> print_endline (Syntax.string_of_prog prog); true) (* Print parsed expressions *)*)
            (*~instr_callback:(fun code -> print_endline (Instr.string_of_fns code); true) (* Print instructions *)*)
            (*~codegen_callback:(fun code -> print_endline ":)"; true)*)
            ~filename: filename
            ~opt_flags: opt_flags
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
        +> flag "--disable-cf" no_arg ~doc:" Disable constant folding optimisations"
    )
    (fun filename disable_cf ->
        run filename { 
            cf=(not disable_cf) 
        }
    )
  |> Command.run
