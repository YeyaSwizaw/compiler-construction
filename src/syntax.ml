(* Compiler Construction - syntax.ml *)
(* Samuel Sleight *)

(* AST Types *)
type apply_spec =
    | Partial of int
    | Full
    | Total

type op_item =
    | Minus
    | Divide
    | Plus
    | Star

type value_item =
    | Int of int
    | Ident of string
    | Function of string list * expr list

and expr =
    | Value of value_item
    | Op of op_item
    | Apply of apply_spec
    | Assignment of string * value_item

(* Printing *)
let rec print_args = function
    | [] -> ()
    | a :: args -> print_string a; print_string " -> "; print_args args;;

let rec print_expr = function
    | Value v -> begin match v with
        | Int i -> print_int i
        | Ident s -> print_string s
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
    end

    | Apply s -> begin match s with
        | Partial n -> print_string "("; print_int n; print_string ")"
        | Full -> print_string "()"
        | Total -> print_string "(*)"
    end

    | Assignment (name, value) -> begin
        print_string name;
        print_string ": ";
        print_expr (Value value);
    end

and print_prog = function
    | [] -> ()
    | e :: prog -> print_expr e; print_newline (); print_prog prog;;
