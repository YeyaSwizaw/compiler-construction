(* Compiler Construction - test.ml *)
(* Samuel Sleight *)

module AT = ANSITerminal

let () =
    AT.save_cursor ();
    Parsertest.run ();
    print_newline ();
    AT.save_cursor ();
    Instrtest.run ();
    print_newline ()
