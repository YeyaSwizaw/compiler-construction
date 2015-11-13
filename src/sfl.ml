(* Compiler Construction - sfl.ml *)
(* Samuel Sleight *)

open Core.Std
open Flag

let run filename emit_asm emit_fns emit_ast output size_flags opt_flags () =
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
            ~size_flags: size_flags
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
        +> flag "--stack-size" (optional_with_default 1024 int) ~doc:"size The size of the program stack"
        +> flag "--storage-size" (optional_with_default 4096 int) ~doc:"size The size of the program storage"
        +> flag "--no-constant-folding" no_arg ~doc:" Disable constant folding optimisations"
        +> flag "--no-storage-cleaning" no_arg ~doc:" Disable storage cleaning optimisation (will fix some crashes, but use more storage)"
    )
    (fun filename emit_asm emit_fns emit_ast output stack_size storage_size disable_cf disable_sc ->
        run filename emit_asm emit_fns emit_ast output {
            stack=stack_size;
            storage=storage_size
        } { 
            cf=(not disable_cf);
            sc=(not disable_sc)
        }
    )
  |> Command.run
