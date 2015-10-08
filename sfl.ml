open Core.Std
open Lexer
open Lexing
open Errors

let run filename () =
        let chan = In_channel.create filename in
        let lexbuf = Lexing.from_channel chan in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
        let result = Parser.program Lexer.read lexbuf in

        match result with
            | Errors.Ok(prog) -> Syntax.print_prog prog
            | Errors.Err(es) -> Errors.print_errors chan es;

        In_channel.close chan

let () =
  Command.basic ~summary:"Compiler"
    Command.Spec.(empty +> anon ("filename" %: file))
    run 
  |> Command.run
