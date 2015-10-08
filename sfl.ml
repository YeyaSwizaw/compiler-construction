open Core.Std
open Lexer
open Lexing

let run filename () =
    try
        print_endline filename;
        let chan = In_channel.create filename in
        let lexbuf = Lexing.from_channel chan in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
        let prog = Parser.program Lexer.read lexbuf in
        In_channel.close chan;
        Syntax.print_prog prog
    with
        | SyntaxError s -> print_endline ("Fuck: " ^ s)

let () =
  Command.basic ~summary:"Compiler"
    Command.Spec.(empty +> anon ("filename" %: file))
    run 
  |> Command.run
