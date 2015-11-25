(* Compiler Construction - instr.ml *)
(* Samuel Sleight *)

let rec idx item = function
    | [] -> None 
    | hd::tl -> if item = hd then (Some 0) else begin match idx item tl with
        | Some n -> Some (n + 1)
        | None -> None
    end

(* Value types *)
type binop_t =
    | Add
    | Sub
    | Mul
    | Div

type fn_t = 
    | BinOp of binop_t
    | Named of string

type io_t =
    | AsChar
    | AsInt

type value_t =
    | Int of int
    | Fn of fn_t

type value_source =
    | Const of value_t
    | BinOp of binop_t * value_source * value_source
    | Read of io_t
    | Arg of int
    | Stored of int

(* Instruction types *)
type instruction_t =
    | WriteConst of io_t * value_t
    | WriteStored of io_t * int
    | Store of value_source

(* Env type *)
type 'a env = {
    env: 'a list;
    parent: ('a env) option;
    args: string list;
    arg_values: (value_source list) option;
    name: string;
}

let make_env ?(parent=None) ?(name="") ?(args=[]) env = {
    env = Syntax.Env.fold (fun name value acc -> match value.Syntax.data with
        | Syntax.Int i -> `Int (name, i) :: acc
        | Syntax.Char c -> `Int (name, int_of_char c) :: acc
        | Syntax.Ident n -> `Ident (name, n) :: acc
        | Syntax.String s -> `String (name, s) :: acc
        | Syntax.Function (a, p) -> `Fn (name, a, p) :: acc
    ) env [];
    parent = parent;
    args = args;
    arg_values = None;
    name = name;
}

let rec make_env_name env = match env.parent with
    | Some parent -> (make_env_name parent) ^ "_" ^ env.name
    | None -> env.name

let env_lookup env name values = match idx name env.args with
    | Some n -> begin match env.arg_values with
        | Some l -> (List.nth l n) :: values
        | None -> (Arg n) :: values
    end

    | None -> values

(* Result type *)
type result_fn = {
    code: instruction_t list;
    args: int
}

module Fns = Map.Make(String)

(* Stringify *)
let string_of_binop = function
    | Add -> "Add"
    | Sub -> "Sub"
    | Mul -> "Mul"
    | Div -> "Div"

let string_of_value = function
    | Int i -> string_of_int i
    | Fn (BinOp op) -> string_of_binop op
    | Fn (Named n) -> n

let rec string_of_value_source = function
    | Const v -> string_of_value v
    | BinOp (op, x, y) -> string_of_binop op ^ "[" ^ string_of_value_source x ^ ":" ^ string_of_value_source y ^ "]"
    | Read AsChar -> "Read[Char]"
    | Arg n -> "Arg[" ^ string_of_int n ^ "]"
    | Stored n -> "Stored[" ^ string_of_int n ^ "]"

let string_of_instr = function
    | WriteConst (AsChar, v) -> "[Write[Char:" ^ string_of_value v ^ "]]"
    | WriteStored (AsChar, v) -> "[Write[Char:Stored[" ^ string_of_int v ^ "]]]"
    | WriteConst (AsInt, v) -> "[Write[Int:" ^ string_of_value v ^ "]]"
    | WriteStored (AsInt, v) -> "[Write[Int:Stored[" ^ string_of_int v ^ "]]]"
    | Store v -> "[Store " ^ string_of_value_source v ^ "]"

let string_of_fn name fn = begin
    let buf = Buffer.create 20 in
    Buffer.add_string buf (name ^ ":" ^ string_of_int fn.args ^ ":");
    List.iter (fun instr -> Buffer.add_string buf (string_of_instr instr)) fn.code;
    Buffer.add_string buf "\n";
    Buffer.contents buf;
end

let string_of_fns fns = begin
    let buf = Buffer.create 50 in
    Fns.iter (fun name fn -> Buffer.add_string buf (string_of_fn name fn)) fns;
    Buffer.contents buf
end

(* Impl *)
let rec pop_args n values = if n = 0 then 
    Some ([], values)
else match values with
    | [] -> None

    | v :: rest -> begin match pop_args (n - 1) rest with
        | Some (args, more) -> Some (v :: args, more)
        | None -> None
    end

let push_string values str = begin
    let ls = ref [] in
    String.iter (fun c -> ls := Const (Int (int_of_char c)) :: !ls) str;
    (List.rev !ls) @ values
end

let apply_binop values op x y = match (op, x, y) with
    | (Add, Const (Int x), Const (Int y)) -> Const (Int (x + y)) :: values
    | (Sub, Const (Int x), Const (Int y)) -> Const (Int (x - y)) :: values
    | (Mul, Const (Int x), Const (Int y)) -> Const (Int (x * y)) :: values
    | (Div, Const (Int x), Const (Int y)) -> Const (Int (x / y)) :: values

    | _ -> BinOp (op, x, y) :: values

let generate_instrs opt_flags code =
    let next_id = let prev = ref 0 in (fun () -> prev := !prev + 1; !prev) in

    let fn_envs = ref Fns.empty in
    let result = ref Fns.empty in
    let errors = ref [] in

    let rec generate_function instrs values env stored = function
        | [] -> List.rev instrs
        | exp_block :: code -> begin match exp_block.Syntax.data with
            (* Basic Values *)
            | Syntax.Value (Syntax.Int i) -> generate_function instrs (Const (Int i) :: values) env stored code
            | Syntax.Value (Syntax.Char c) -> generate_function instrs (Const (Int (int_of_char c)) :: values) env stored code
            | Syntax.Value (Syntax.String s) -> generate_function instrs (push_string values s) env stored code

            (* Anonymous Functions *)
            | Syntax.Value (Syntax.Function (fn_args, fn_prog)) -> begin
                let fn_id = next_id () in
                let fn_name = (make_env_name env) ^ "_anon_" ^ (string_of_int fn_id) in
                let fn_env = make_env ~parent:(Some env) ~name:("anon_" ^ string_of_int fn_id) ~args:fn_args fn_prog.Syntax.env in
                fn_envs := Fns.add fn_name (fn_env, fn_prog.Syntax.code) !fn_envs;
                let fn = generate_function [] [] fn_env 0 fn_prog.Syntax.code in
                result := Fns.add fn_name { code=fn; args=(List.length fn_args); } !result;
                generate_function instrs (Const (Fn (Named fn_name)) :: values) env stored code
            end

            (* Named Value *)
            | Syntax.Value (Syntax.Ident n) -> generate_function instrs (env_lookup env n values) env stored code

            (* Binary Operators *)
            | Syntax.Op Syntax.Plus -> generate_function instrs (Const (Fn (BinOp Add)) :: values) env stored code
            | Syntax.Op Syntax.Minus -> generate_function instrs (Const (Fn (BinOp Sub)) :: values) env stored code
            | Syntax.Op Syntax.Times -> generate_function instrs (Const (Fn (BinOp Mul)) :: values) env stored code
            | Syntax.Op Syntax.Divide -> generate_function instrs (Const (Fn (BinOp Div)) :: values) env stored code

            (* Application *)
            | Syntax.Apply Syntax.Full -> begin match values with
                | Const (Fn (BinOp op)) :: rest -> begin match rest with
                    | x :: y :: rest -> generate_function instrs (apply_binop rest op x y) env stored code
                    | _ -> begin
                        errors := Errors.not_enough_args exp_block.Syntax.location :: !errors;
                        generate_function instrs values env stored code
                    end
                end

                | Const (Fn (Named n)) :: rest -> begin
                    let (fn_env, fn_code) = Fns.find n !fn_envs in
                    match pop_args (List.length fn_env.args) rest with
                        Some (args, rest) -> generate_function 
                            (List.rev_map (fun arg -> Store arg) args @ instrs)
                            rest
                            { fn_env with 
                                arg_values=(Some (List.mapi (fun n _ -> Stored (stored + n)) fn_env.args));
                                parent=(Some env);
                            }
                            (stored + (List.length fn_env.args))
                            (fn_code @ { Syntax.location=Lexing.dummy_pos; Syntax.data=Syntax.PopEnv; } :: code)
                end
            end

            (* Read *)
            | Syntax.Read Syntax.AsChar -> generate_function instrs (Read AsChar :: values) env stored code
            | Syntax.Read Syntax.AsInt -> generate_function instrs (Read AsInt :: values) env stored code

            (* Write *)
            | Syntax.Write Syntax.AsChar -> begin match values with
                | Const value :: _ -> generate_function (WriteConst (AsChar, value) :: instrs) values env stored code
                | Stored n :: _ -> generate_function (WriteStored (AsChar, n) :: instrs) values env stored code
                | value :: rest -> generate_function (WriteStored (AsChar, stored) :: Store value :: instrs) (Stored stored :: rest) env (stored + 1) code
            end

            | Syntax.Write Syntax.AsInt -> begin match values with
                | Const value :: _ -> generate_function (WriteConst (AsInt, value) :: instrs) values env stored code
                | Stored n :: _ -> generate_function (WriteStored (AsInt, n) :: instrs) values env stored code
                | value :: rest -> generate_function (WriteStored (AsInt, stored) :: Store value :: instrs) (Stored stored :: rest) env (stored + 1) code
            end

            (* Pop Environment *)
            | Syntax.PopEnv -> begin match env.parent with
                | Some parent -> generate_function instrs values parent stored code
                | None -> generate_function instrs values env stored code
            end
        end
    in

    let fn = generate_function [] [] (make_env code.Syntax.env) 0 code.Syntax.code in

    if !errors = [] then
        Errors.Ok (Fns.add "" { code=fn; args=0 } Fns.empty)
    else
        Errors.Err !errors
