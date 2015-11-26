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
    | Eq
    | Lt
    | Gt

type triop_t =
    | Ite

type fn_t = 
    | BinOp of binop_t
    | TriOp of triop_t
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
    | TriOp of triop_t * value_source * value_source * value_source
    | Read of io_t
    | Arg of int
    | Stored of int

type apply_t =
    | Named of string * (value_source list) * int
    | Value of value_source * (value_source list) * (int option)
    | Recurse of string * (value_source list)

(* Instruction types *)
type instruction_t =
    | WriteConst of io_t * value_t
    | WriteStored of io_t * int
    | Store of value_source
    | Apply of apply_t
    | Return of value_source list

(* Env type *)
type 'a env = {
    env: 'a list;
    parent: ('a env) option;
    args: string list;
    arg_values: (value_source list) option;
    name: string option;
}

let make_env ?(parent=None) ?(name=Some("")) ?(args=[]) env = {
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
    | Some parent -> begin match env.name with
        | Some name -> (make_env_name parent) ^ "_" ^ name
        | None -> make_env_name parent
    end

    | None -> begin match env.name with
        | Some name -> name
        | None -> ""
    end

(* Result type *)
type result_fn = {
    code: instruction_t list;
    args: int;
    returns: int;
}

module Fns = Map.Make(String)

(* Stringify *)
let string_of_binop = function
    | Add -> "Add"
    | Sub -> "Sub"
    | Mul -> "Mul"
    | Div -> "Div"
    | Eq -> "Eq"
    | Lt -> "Lt"
    | Gt -> "Gt"

let string_of_triop = function
    | Ite -> "Ite"

let string_of_value = function
    | Int i -> string_of_int i
    | Fn (BinOp op) -> string_of_binop op
    | Fn (TriOp op) -> string_of_triop op
    | Fn (Named n) -> n

let rec string_of_value_source = function
    | Const v -> string_of_value v
    | BinOp (op, x, y) -> string_of_binop op ^ "[" ^ string_of_value_source x ^ ":" ^ string_of_value_source y ^ "]"
    | TriOp (op, x, y, z) -> string_of_triop op ^ "[" ^ string_of_value_source x ^ ":" ^ string_of_value_source y ^ ":" ^ string_of_value_source z ^ "]"
    | Read AsChar -> "Read[Char]"
    | Arg n -> "Arg[" ^ string_of_int n ^ "]"
    | Stored n -> "Stored[" ^ string_of_int n ^ "]"

let string_of_arg_list ls = begin
    let buf = Buffer.create 20 in
    List.iteri (fun i v -> Buffer.add_string buf ((if i = 0 then "" else ":") ^ string_of_value_source v)) ls;
    Buffer.contents buf;
end

let string_of_apply = function
    | Named (n, a, _) -> n ^ "[" ^ string_of_arg_list a ^ "]"
    | Value (v, a, _) -> string_of_value_source v ^ "[" ^ string_of_arg_list a ^ "]"
    | Recurse (n, a) -> n ^ "[" ^ string_of_arg_list a ^ "]"

let string_of_instr = function
    | WriteConst (AsChar, v) -> "[Write[Char:" ^ string_of_value v ^ "]]"
    | WriteStored (AsChar, v) -> "[Write[Char:Stored[" ^ string_of_int v ^ "]]]"
    | WriteConst (AsInt, v) -> "[Write[Int:" ^ string_of_value v ^ "]]"
    | WriteStored (AsInt, v) -> "[Write[Int:Stored[" ^ string_of_int v ^ "]]]"
    | Store v -> "[Store " ^ string_of_value_source v ^ "]"
    | Apply a -> "[Apply " ^ string_of_apply a ^ "]"
    | Return v -> "[Return [" ^ string_of_arg_list v ^ "]]"

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
let push_string values str = begin
    let ls = ref [] in
    String.iter (fun c -> ls := Const (Int (int_of_char c)) :: !ls) str;
    (List.rev !ls) @ values
end

let rec pop_args n values = if n = 0 then 
    Some ([], values)
else match values with
    | [] -> None

    | v :: rest -> begin match pop_args (n - 1) rest with
        | Some (args, more) -> Some (v :: args, more)
        | None -> None
    end

let apply_binop values op x y = match (op, x, y) with
    | (Add, Const (Int x), Const (Int y)) -> Const (Int (x + y)) :: values
    | (Sub, Const (Int x), Const (Int y)) -> Const (Int (x - y)) :: values
    | (Mul, Const (Int x), Const (Int y)) -> Const (Int (x * y)) :: values
    | (Div, Const (Int x), Const (Int y)) -> Const (Int (x / y)) :: values
    | (Eq, Const (Int x), Const (Int y)) -> Const (Int (if x = y then 1 else 0)) :: values
    | (Lt, Const (Int x), Const (Int y)) -> Const (Int (if x < y then 1 else 0)) :: values
    | (Gt, Const (Int x), Const (Int y)) -> Const (Int (if x > y then 1 else 0)) :: values

    | _ -> BinOp (op, x, y) :: values

let apply_triop values op x y z = match (op, x, y, z) with
    | (Ite, Const (Int x), y, z) -> (if x = 0 then z else y) :: values

    | _ -> TriOp (op, x, y, z) :: values

let rec arg_instrs instrs = function
    | [] -> instrs
    | Const c :: args -> arg_instrs instrs args
    | arg :: args -> Store arg :: arg_instrs instrs args

let rec arg_values stored = function
    | [] -> (stored, [])

    | Const c :: args -> let (s, ls) = arg_values stored args in
        (s, Const c :: ls)

    | arg :: args -> let (s, ls) = arg_values (stored + 1) args in
        (s, Stored stored :: ls)

let rec make_ret args stored values =
    if args = 0 then
        values
    else
        make_ret (args - 1) (stored + 1) (Stored stored :: values)

let generate_instrs opt_flags code =
    let next_id = let prev = ref 0 in (fun () -> prev := !prev + 1; !prev) in

    let fn_envs = ref Fns.empty in
    let result = ref Fns.empty in
    let errors = ref [] in

    let rec fn_type = function
        | Const (Fn (Named n)) -> let fn = Fns.find n !result in (Some fn.args, Some fn.returns)
        | TriOp (_, _, f1, f2) -> begin match (fn_type f1, fn_type f2) with
            | ((a1, r1), (a2, r2)) -> ((if a1 = a2 then a1 else None), (if r1 = r2 then r1 else None))
        end

        | _ -> (None, None)
    in

    let rec env_lookup env name values = 
        let rec local_lookup = function
            | [] -> None
            | `Int (n, i) :: tl -> if name = n then Some (Const (Int i) :: values) else local_lookup tl
            | `Ident (n, i) :: tl -> if name = n then Some (env_lookup env i values) else local_lookup tl
            | `String (n, s) :: tl -> if name = n then Some (push_string values s) else local_lookup tl
            | `Fn (n, a, p) :: tl -> if name = n then begin
                let fn_name = (make_env_name env) ^ "_" ^ name in
                try
                    let (_, _) = Fns.find fn_name !fn_envs in
                    Some (Const (Fn (Named fn_name)) :: values)
                with
                    Not_found -> begin
                        let fn_env = make_env ~parent:(Some env) ~name:(Some name) ~args:a p.Syntax.env in
                        fn_envs := Fns.add fn_name (fn_env, p.Syntax.code) !fn_envs;
                        let (ret, fn) = generate_function [] [] fn_env 0 p.Syntax.code in
                        result := Fns.add fn_name { code=fn; args=(List.length a); returns=ret; } !result;
                        Some (Const (Fn (Named fn_name)) :: values)
                    end
            end else 
                local_lookup tl
        in

        (* Lookup in local name scope *)
        match local_lookup env.env with
            | Some l -> l

            (* Lookup in local arguments *)
            | None -> begin match idx name env.args with
                | Some n -> begin match env.arg_values with
                    | Some l -> (List.nth l n) :: values
                    | None -> (Arg n) :: values
                end

                (* Lookup in parent *)
                | None -> begin match env.parent with
                    | Some parent -> env_lookup parent name values
                    | None -> values (* TODO actual error *)
                end
            end

    and generate_function instrs values env stored = function
        | [] -> (List.length values, List.rev (Return values :: instrs))

        | exp_block :: code -> begin match exp_block.Syntax.data with
            (* Basic Values *)
            | Syntax.Value (Syntax.Int i) -> generate_function instrs (Const (Int i) :: values) env stored code
            | Syntax.Value (Syntax.Char c) -> generate_function instrs (Const (Int (int_of_char c)) :: values) env stored code
            | Syntax.Value (Syntax.String s) -> generate_function instrs (push_string values s) env stored code

            (* Anonymous Functions *)
            | Syntax.Value (Syntax.Function (fn_args, fn_prog)) -> begin
                let fn_id = next_id () in
                let fn_name = (make_env_name env) ^ "_anon_" ^ (string_of_int fn_id) in
                let fn_env = make_env ~parent:(Some env) ~name:(Some ("anon_" ^ string_of_int fn_id)) ~args:fn_args fn_prog.Syntax.env in
                fn_envs := Fns.add fn_name (fn_env, fn_prog.Syntax.code) !fn_envs;
                let (ret, fn) = generate_function [] [] fn_env 0 fn_prog.Syntax.code in
                result := Fns.add fn_name { code=fn; args=(List.length fn_args); returns=ret; } !result;
                generate_function instrs (Const (Fn (Named fn_name)) :: values) env stored code
            end

            (* Named Value *)
            | Syntax.Value (Syntax.Ident n) -> generate_function instrs (env_lookup env n values) env stored code

            (* Binary Operators *)
            | Syntax.Op Syntax.Plus -> generate_function instrs (Const (Fn (BinOp Add)) :: values) env stored code
            | Syntax.Op Syntax.Minus -> generate_function instrs (Const (Fn (BinOp Sub)) :: values) env stored code
            | Syntax.Op Syntax.Times -> generate_function instrs (Const (Fn (BinOp Mul)) :: values) env stored code
            | Syntax.Op Syntax.Divide -> generate_function instrs (Const (Fn (BinOp Div)) :: values) env stored code
            | Syntax.Op Syntax.Eq -> generate_function instrs (Const (Fn (BinOp Eq)) :: values) env stored code
            | Syntax.Op Syntax.Lt -> generate_function instrs (Const (Fn (BinOp Lt)) :: values) env stored code
            | Syntax.Op Syntax.Gt -> generate_function instrs (Const (Fn (BinOp Gt)) :: values) env stored code

            | Syntax.Op Syntax.IfThenElse -> generate_function instrs (Const (Fn (TriOp Ite)) :: values) env stored code

            (* Application *)
            | Syntax.Apply Syntax.Full -> begin match values with
                | Const (Fn (BinOp op)) :: rest -> begin match rest with
                    | x :: y :: rest -> generate_function instrs (apply_binop rest op x y) env stored code
                    | _ -> begin
                        errors := Errors.not_enough_args exp_block.Syntax.location :: !errors;
                        generate_function instrs values env stored code
                    end
                end

                | Const (Fn (TriOp op)) :: rest -> begin match rest with
                    | x :: y :: z :: rest -> generate_function instrs (apply_triop rest op x y z) env stored code
                    | _ -> begin
                        errors := Errors.not_enough_args exp_block.Syntax.location :: !errors;
                        generate_function instrs values env stored code
                    end
                end

                | Const (Fn (Named n)) :: rest -> if opt_flags.Flag.fe then begin
                    (* Attempt inlining of function *)
                    let (fn_env, fn_code) = Fns.find n !fn_envs in
                    match pop_args (List.length fn_env.args) rest with
                        | Some (args, rest) -> 
                            let (stored, values) = arg_values stored args in
                            generate_function 
                                (arg_instrs instrs args)
                                rest
                                { fn_env with 
                                    arg_values=Some(values);
                                    parent=(Some env);
                                    name=None;
                                }
                                stored
                                (fn_code @ { Syntax.location=Lexing.dummy_pos; Syntax.data=Syntax.PopEnv; } :: code)

                        (* Cannot inline - TODO *)
                        | None -> begin
                            errors := Errors.not_enough_args exp_block.Syntax.location :: !errors;
                            generate_function instrs values env stored code
                        end

                end else begin
                    try
                        let fn = Fns.find n !result in
                        match pop_args fn.args rest with
                            | Some (args, rest) -> 
                                generate_function 
                                    (Apply (Named (n, args, fn.returns)) :: instrs) 
                                    (make_ret fn.returns stored rest) 
                                    env 
                                    (stored + fn.returns) 
                                    code

                            (* Cannot apply - TODO *)
                            | None -> begin
                                errors := Errors.not_enough_args exp_block.Syntax.location :: !errors;
                                generate_function instrs values env stored code
                            end
                    with
                        (* Recursion *)
                        Not_found -> begin
                            let (fn_env, _) = Fns.find n !fn_envs in
                            match pop_args (List.length fn_env.args) rest with
                                | Some (args, rest) ->
                                    generate_function
                                        (Apply (Recurse (n, args)) :: instrs)
                                        rest
                                        env
                                        stored
                                        code

                                (* Cannot apply - TODO *)
                                | None -> begin
                                    errors := Errors.not_enough_args exp_block.Syntax.location :: !errors;
                                    generate_function instrs values env stored code
                                end
                        end
                end

                | v :: rest -> begin match fn_type v with
                    (* Known return *)
                    | (Some argc, Some retc) -> begin match pop_args argc rest with
                        | Some (args, rest) ->
                                generate_function
                                    (Apply (Value (v, args, Some retc)) :: instrs)
                                    (make_ret retc stored rest)
                                    env
                                    (stored + retc)
                                    code

                        | None -> begin
                            errors := Errors.not_enough_args exp_block.Syntax.location :: !errors;
                            generate_function instrs values env stored code
                        end
                    end

                    (* Unknown return *)
                    | (Some argc, None) -> begin match pop_args argc rest with
                        | Some (args, rest) ->
                            generate_function
                                (Apply (Value (v, args, None)) :: instrs)
                                rest
                                env
                                stored
                                code

                        | None -> begin
                            errors := Errors.not_enough_args exp_block.Syntax.location :: !errors;
                            generate_function instrs values env stored code
                        end
                    end
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

    let (_, fn) = generate_function [] [] (make_env code.Syntax.env) 0 code.Syntax.code in

    if !errors = [] then
        Errors.Ok (Fns.add "" { code=fn; args=0; returns=0; } !result)
    else
        Errors.Err !errors
