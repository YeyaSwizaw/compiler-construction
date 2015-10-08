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

type expr =
    | Value of value_item
    | Op of op_item
    | Apply of apply_spec

(* Printing *)
let print_expr = function
    | Value v -> begin match v with
        | Int i -> print_int i
        | Ident s -> print_string s
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

let rec print_prog = function
    | [] -> ()
    | e :: prog -> print_expr e; print_newline (); print_prog prog;;
