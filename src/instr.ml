(* Compiler Construction - instr.ml *)
(* Samuel Sleight *)

(* Instruction types *)
type value_t =
    | Int of int
    | Char of char
    | String of string

type binop_t =
    | Add
    | Sub
    | Mul
    | Div
    | Eq
    | Lt
    | Gt

type triop_t =
    | Ite

type fn_t = 
    | BinOp of binop_t
    | TriOp of triop_t
    | Named of string * int

type apply_t =
    | Full

type instruction =
    | PushConst of value_t
    | PushFn of fn_t
    | PushArg of int
    | PushSelf
    | Apply of apply_t

module Fns = Map.Make(String)

type 'a env = {
    env: 'a list;
    args: string list;
    parent: ('a env) option;
    name: string;
}

type fn = {
    code: instruction list;
    args: int;
}


(* Stringifying *)
let string_of_instr = function
    | PushConst (Int i) -> "Push[Int[" ^ (string_of_int i) ^"]]"
    | PushConst (Char c) -> "Push[Char[" ^ (String.make 1 c) ^"]]"
    | PushConst (String s) -> "Push[Str[" ^ s ^"]]"

    | PushFn (BinOp Add) -> "Push[Fn[+]]"
    | PushFn (BinOp Sub) -> "Push[Fn[-]]"
    | PushFn (BinOp Mul) -> "Push[Fn[*]]"
    | PushFn (BinOp Div) -> "Push[Fn[/]]"
    | PushFn (BinOp Eq) -> "Push[Fn[=]]"
    | PushFn (BinOp Lt) -> "Push[Fn[<]]"
    | PushFn (BinOp Gt) -> "Push[Fn[>]]"

    | PushFn (TriOp Ite) -> "Push[Fn[?]]"

    | PushFn (Named (s, _)) -> "Push[Fn[" ^ s ^ "]]"

    | PushArg i -> "Push[Arg[" ^ (string_of_int i) ^ "]]"

    | PushSelf -> "Push[Self]"

    | Apply Full -> "Apply[]"

let rec string_of_args = function
    | [] -> ""
    | [arg] -> arg
    | arg :: args -> arg ^ ", " ^ (string_of_args args)

let string_of_fns fns = 
    Fns.fold 
        (fun name fn acc -> 
            let fn_str = List.fold_left (fun acc instr -> (string_of_instr instr) ^ acc) "" fn.code in
            acc ^ name ^ ":[" ^ (string_of_int fn.args) ^ "]:" ^ fn_str ^ "\n"
        ) fns ""

(* Convert parse tree to instructions *)
let generate_instructions opt_flags code =
    let next_id = let prev = ref 0 in (fun () -> prev := !prev + 1; !prev) in

    let make_env ?(parent=None) ?(name="") ?(args=[]) env = {
        env = Syntax.Env.fold (fun name value acc -> match value.Syntax.data with
            | Syntax.Int i -> `Int (name, i) :: acc
            | Syntax.Char c -> `Char (name, c) :: acc
            | Syntax.Ident i -> `Ident (name, i) :: acc
            | Syntax.String s -> `String (name, s) :: acc
            | Syntax.Function (a, p) -> `Fn (name, a, p) :: acc
        ) env [];
        parent = parent;
        args = args;
        name = name;
    } 
    in

    let result = ref Fns.empty in

    let rec idx item = function
        | [] -> None 
        | hd::tl -> if item = hd then (Some 0) else begin match idx item tl with
            | Some n -> Some (n + 1)
            | None -> None
        end
    in

    let rec env_lookup ?(step=1) ?(total_args=0) name env = 
        let rec list_lookup = function
            | [] -> None
            | `Int (n, i) :: tl -> if name = n then Some (PushConst (Int i)) else list_lookup tl
            | `Char (n, c) :: tl -> if name = n then Some (PushConst (Char c)) else list_lookup tl
            | `String (n, s) :: tl -> if name = n then Some (PushConst (String s)) else list_lookup tl
            | `Ident (n, i) :: tl -> if name = n then Some (env_lookup ~step:step ~total_args:total_args i env) else list_lookup tl
            | `Fn (n, a, p) :: tl -> if name = n then begin
                list_lookup tl
            end else
                list_lookup tl
        in

        match list_lookup env.env with
            | Some thing -> thing
            | None -> begin
                match idx name env.args with
                    | Some n -> PushArg ((n + (2 * step - 1) + total_args + 1) * 8)
                    | None -> 
                        let (Some parent) = env.parent in 
                        env_lookup ~step:(step + 1) ~total_args:(total_args + (List.length env.args)) name parent
            end

    in

    let rec generate_function instrs env code = match code with
        | [] -> instrs
        | exp_block :: tl -> begin match exp_block.Syntax.data with
            (* Push simple values *)
            | Syntax.Value (Syntax.Int i) -> generate_function (PushConst (Int i) :: instrs) env tl
            | Syntax.Value (Syntax.Char c) -> generate_function (PushConst (Char c) :: instrs) env tl
            | Syntax.Value (Syntax.String s) -> generate_function (PushConst (String s) :: instrs) env tl

            (* Push anon function *)
            | Syntax.Value (Syntax.Function (fn_args, fn_prog)) -> begin
                let fn_id = next_id () in
                let fn_name = env.name ^ "_anon_" ^ (string_of_int fn_id) in
                let fn_env = make_env ~parent:(Some env) ~name:fn_name ~args:fn_args fn_prog.Syntax.env in
                let fn = generate_function [] fn_env fn_prog.Syntax.code in
                result := Fns.add fn_name { code=fn; args=(List.length fn_args); } !result;
                generate_function (PushFn (Named (fn_name, List.length fn_args)) :: instrs) env tl
            end

            | Syntax.Value (Syntax.Ident i) -> generate_function (env_lookup i env :: instrs) env tl 

            | Syntax.Apply Syntax.Full -> generate_function (Apply Full :: instrs) env tl
        end
    in

    let fn = generate_function [] (make_env code.Syntax.env) code.Syntax.code in
    Errors.Ok (Fns.add "" { code=fn; args=0; } !result)
