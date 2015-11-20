(* Compiler Construction - syntax.ml *)
(* Samuel Sleight *)

open List

(* AST Types *)
module Env = Map.Make(String)

type 'a chunk = {
    location: Lexing.position;
    data: 'a
}

let rec dechunk = function
    | [] -> []
    | h :: t -> h.data :: dechunk t

type write_item =
    | AsChar

type apply_spec =
    | Partial of int
    | Full
    | Total

type op_item =
    | Minus
    | Divide
    | Plus
    | Times
    | Lt
    | Gt
    | Eq
    | IfThenElse

type value_item =
    | Int of int
    | Char of char
    | Ident of string
    | String of string
    | Function of string list * program_t

and expr =
    | Value of value_item
    | Op of op_item
    | Apply of apply_spec
    | Write of write_item
    | PopEnv

and program_t = {
    env: (value_item chunk) Env.t;
    code: (expr chunk) list;
}

(* Chunk functions *)
let op_chunk op pos = { location=pos; data=(Op op) }
let callspec_chunk spec pos = { location=pos; data=(Apply spec) }

(* To string functions *)
let rec print_args = function
    | [] -> ()
    | a :: args -> print_string a; print_string " -> "; print_args args;;

let rec string_of_expr = function
    | Value v -> begin match v with
        | Int i -> "Int[" ^ (string_of_int i) ^ "]"
        | Char c -> "Char[" ^ (String.make 1 c) ^ "]"
        | Ident s -> "Id[" ^ s ^ "]"
        | String s -> "Str[" ^ s ^ "]"

        | Function (args, body) -> 
            (fold_left (fun acc arg -> acc ^ arg ^ " -> ") "Fn[" args) ^ "{" ^ (string_of_prog body) ^ "}]"
    end

    | Op o -> begin match o with
        | Minus -> "Op[-]"
        | Divide -> "Op[/]"
        | Plus -> "Op[+]"
        | Times -> "Op[*]"
        | Lt -> "Op[<]"
        | Gt -> "Op[>]"
        | Eq -> "Op[=]"
        | IfThenElse -> "Op[?]"
    end

    | Apply s -> begin match s with
        | Partial n -> "Apply[" ^ (string_of_int n) ^ "]"
        | Full -> "Apply[]"
        | Total -> "Apply[*]"
    end

    | Write w -> begin match w with
        | AsChar -> "Write[Char]"
    end

    | PopEnv -> "Pop[]" (* Shouldn't usually be seen *)

and string_of_exprs ls = fold_left (fun acc expr -> acc ^ string_of_expr expr) "" ls

and string_of_env ls = fold_left (fun acc (name, value) -> acc ^ "Name[" ^ name ^ ":" ^ (string_of_expr (Value value.data)) ^ "]") "" ls

and string_of_prog prog = (string_of_env (Env.bindings prog.env)) ^ (string_of_exprs (dechunk prog.code))

and string_of_pepe frog = "memes: " ^ frog
