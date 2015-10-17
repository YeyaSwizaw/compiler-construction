(* Compiler Construction - syntax.ml *)
(* Samuel Sleight *)

(* AST Types *)
module Env = Map.Make(String)

type apply_spec =
    | Partial of int
    | Full
    | Total

type op_item =
    | Minus
    | Divide
    | Plus
    | Star
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

and program_t = {
    env: value_item Env.t;
    code: expr list;
}

(* Pretty printing functions*)
let rec print_args = function
    | [] -> ()
    | a :: args -> print_string a; print_string " -> "; print_args args;;

let rec print_expr = function
    | Value v -> begin match v with
        | Int i -> print_int i
        | Char c -> print_char c
        | Ident s -> print_string s
        | String s -> print_string ("\"" ^ s ^ "\"")
        | Function (args, body) -> begin
            print_args args;
            print_string "{";
            print_newline ();
            print_prog body;
            print_string "}";
        end
    end
        
    | Op o -> begin match o with
        | Minus -> print_string "-"
        | Divide -> print_string "/"
        | Plus -> print_string "+"
        | Star -> print_string "*"
        | Lt -> print_string "<"
        | Gt -> print_string ">"
        | Eq -> print_string "="
        | IfThenElse -> print_string "?"
    end

    | Apply s -> begin match s with
        | Partial n -> print_string "("; print_int n; print_string ")"
        | Full -> print_string "()"
        | Total -> print_string "(*)"
    end

and print_exprs = function
    | [] -> ()
    | e :: es -> print_expr e; print_newline (); print_exprs es

and print_env code = 
    Env.iter (fun name value -> (
        print_string (name ^ ": "); 
        print_expr (Value value); 
        print_newline ())
    ) code

and print_prog prog = (
    print_env prog.env;
    print_exprs prog.code 
)
