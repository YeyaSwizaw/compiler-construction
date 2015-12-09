(* Compiler Construction - sfl.ml *)
(* Samuel Sleight *)

open Core.Std
open Flag

let run filename emit_llvm emit_fns emit_ast output opt_flags () =
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
                if emit_llvm then (
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
  Command.basic ~summary:"SFL Compiler"
    Command.Spec.(
        empty 
        +> anon ("filename" %: file)
        +> flag "--emit-llvm" no_arg ~doc:" Output llvm ir to stdout"
        +> flag "--emit-fns" no_arg ~doc:" Output fn instrs to stdout"
        +> flag "--emit-ast" no_arg ~doc:" Output ast representation to stdout"
        +> flag ~aliases:["-o"] "--output" (optional_with_default "a.out" file) ~doc:"filename The output filename"
        +> flag "--function-inlining" no_arg ~doc: " Enable inlining of functions where possible. Enabling this will currently break with programs that use recursion"
    )
    (fun filename emit_llvm emit_fns emit_ast output enable_fe ->
        run filename emit_llvm emit_fns emit_ast output {
            fe=enable_fe
        }
    )
  |> Command.run
