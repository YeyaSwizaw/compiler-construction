(* Compiler Construction - compiler.ml *)
(* Samuel Sleight *)

open Lexing

let run ?parser_callback ?filename file =
    let lexbuf = Lexing.from_channel file in
    begin match filename with
         | Some(filename) -> lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
         | None -> ()
    end;

    match Parser.program Lexer.read lexbuf with
        | Errors.Ok(prog) -> begin match parser_callback with
            | Some(f) -> f prog; Errors.Ok(())
            | None -> Errors.Ok(())
        end

        | Errors.Err(es) -> Errors.Err(es)
