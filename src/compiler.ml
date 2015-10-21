(* Compiler Construction - compiler.ml *)
(* Samuel Sleight *)

open Lexing

type 'a run_status =
    | Continue of 'a
    | Terminate

(* Run the compiler on a given file *)
let run ?parser_callback ?instr_callback ?filename file =
    let lexbuf = Lexing.from_channel file in
    begin match filename with
         | Some(filename) -> lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
         | None -> ()
    end;

    (* Run the parser *)
    let parse_result = match Parser.program Lexer.read lexbuf with
        | Errors.Ok prog -> begin match parser_callback with
            | Some f -> if f prog then Continue (Errors.Ok prog) else Terminate
            | None -> Continue (Errors.Ok prog)
        end

        | Errors.Err es -> Continue (Errors.Err es)
    in

    (* Run the first optimisation stage *)
    let instr_result = match parse_result with
        | Continue (Errors.Ok prog) -> begin match Instr.generate_instructions prog with
            | Errors.Ok code -> begin match instr_callback with
                | Some f -> if f code then Continue (Errors.Ok code) else Terminate
                | None -> Continue (Errors.Ok code)
            end

            | Errors.Err es -> Continue (Errors.Err es)
        end

        | Continue (Errors.Err errs) -> Continue (Errors.Err errs)
        | Terminate -> Terminate
    in

    match instr_result with
        | Continue (Errors.Ok _) -> Errors.Ok ()
        | Continue (Errors.Err es) -> Errors.Err es
        | Terminate -> Errors.Ok ()
