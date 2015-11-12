(* Compiler Construction - sfl.ml *)
(* Samuel Sleight *)

open Core.Std
open Flag

let run filename emit_asm emit_fns emit_ast output opt_flags () =
    try
        (* Open a file *)
        let chan = In_channel.create filename in

        (* Run the compiler *)
        let result = Compiler.run 
            ~parser_callback:(fun prog -> 
                if emit_ast then (
                    print_endline (Syntax.string_of_prog prog);
                    false
                ) else
                    true
            )

            ~instr_callback:(fun code -> 
                if emit_fns then (
                    print_endline (Instr.string_of_fns code);
                    false
                ) else
                    true
            )

            ~codegen_callback:(fun code -> 
                if emit_asm then (
                    print_endline code;
                    false
                ) else
                    true
            )

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
        +> flag "--emit-asm" no_arg ~doc:" Output asm to stdout"
        +> flag "--emit-fns" no_arg ~doc:" Output fn instrs to stdout"
        +> flag "--emit-ast" no_arg ~doc:" Output ast representation to stdout"
        +> flag ~aliases:["-o"] "--output" (optional_with_default "a.out" file) ~doc:"filename The output filename"
        +> flag "--disable-cf" no_arg ~doc:" Disable constant folding optimisations"
    )
    (fun filename emit_asm emit_fns emit_ast output disable_cf ->
        run filename emit_asm emit_fns emit_ast output { 
            cf=(not disable_cf) 
        }
    )
  |> Command.run
