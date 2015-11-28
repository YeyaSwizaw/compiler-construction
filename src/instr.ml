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
    | Pop (* D: *)

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
    | Push of value_source

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
    tainted: bool;
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
    | Pop -> "Pop"

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
    | Push v -> "[Push " ^ string_of_value_source v ^ "]"

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

let rec pop_args n = function
    | [] -> if n = 0 then 
        (0, [], []) 
    else 
        let (sa, args, rest) = pop_args (n - 1) [] in
        (sa + 1, Pop :: args, rest)

    | v :: rest -> if n = 0 then
        (0, [], v :: rest)
    else
        let (sa, args, rest) = pop_args (n - 1) rest in
        (sa, v :: args, rest)

let rec const_args n = function
    | [] -> if n = 0 then
        Some ([], [])
    else
        None

    | Const v :: rest -> if n = 0 then
        Some ([Const v], rest)
    else
        begin match const_args (n - 1) rest with
            | Some (a, rest) -> Some (Const v :: a, rest)
            | None -> None
        end

    | _ -> None

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
    (*
    let next_id = let prev = ref 0 in (fun () -> prev := !prev + 1; !prev) in
    *)

    let fn_envs = ref Fns.empty in
    let result = ref Fns.empty in
    let errors = ref [] in

    let rec fn_type = function
        | Const (Fn (Named n)) -> let fn = Fns.find n !result in (Some fn.args, Some fn.returns, fn.tainted)
        | BinOp (_, _, _) -> (Some 2, Some 1, false)
        | TriOp (Ite, _, f1, f2) -> begin match (fn_type f1, fn_type f2) with
            | ((a1, r1, t1), (a2, r2, t2)) -> ((if a1 = a2 then a1 else None), (if r1 = r2 then r1 else None), t1 || t2)
        end

        | _ -> (None, None, true)
    in

    let rec taint = function
        | Const (Fn (Named n)) -> begin
            let fn = Fns.find n !result in
            let new_code = List.fold_right (fun i acc -> match i with
                | Return rets -> (List.map (fun v -> Push v) rets) @ [Return []]
                | Return [] -> [Return []]
                | instr -> instr :: acc
            ) fn.code [] in
            result := Fns.add n { fn with code=new_code; tainted=true; returns=0; } !result
        end

        | TriOp (Ite, _, f1, f2) -> begin
            taint f1;
            taint f2;
        end

        | _ -> ()
    in

    let rec env_lookup recursed tainted env name values = 
        let rec local_lookup = function
            | [] -> None
            | `Int (n, i) :: tl -> if name = n then Some (Const (Int i) :: values) else local_lookup tl
            | `Ident (n, i) :: tl -> if name = n then Some (env_lookup recursed tainted env i values) else local_lookup tl
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
                        let (ret, taint, fn) = generate_function 0 recursed tainted [] [] fn_env 0 p.Syntax.code in
                        result := Fns.add fn_name { code=fn; args=(List.length a); returns=ret; tainted=taint; } !result;
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
                    | Some parent -> env_lookup recursed tainted parent name values
                    | None -> values (* TODO actual error *)
                end
            end

    and generate_function next_id recursed tainted instrs values env stored = function
        | [] -> if recursed || tainted then begin
            let instrs = 
                List.fold_right
                    (fun v acc -> Push v :: acc)
                    values
                    instrs
            in

            (0, tainted, List.rev (Return [] :: instrs))
        end else begin
            (List.length values, tainted, List.rev (Return values :: instrs))
        end
                

        | exp_block :: code -> begin match exp_block.Syntax.data with
            (* Basic Values *)
            | Syntax.Value (Syntax.Int i) -> generate_function next_id recursed tainted instrs (Const (Int i) :: values) env stored code
            | Syntax.Value (Syntax.Char c) -> generate_function next_id recursed tainted instrs (Const (Int (int_of_char c)) :: values) env stored code
            | Syntax.Value (Syntax.String s) -> generate_function next_id recursed tainted instrs (push_string values s) env stored code

            (* Anonymous Functions *)
            | Syntax.Value (Syntax.Function (fn_args, fn_prog)) -> begin
                let fn_id = next_id in
                let fn_name = (make_env_name env) ^ "_anon_" ^ (string_of_int fn_id) in
                let fn_env = make_env ~parent:(Some env) ~name:(Some ("anon_" ^ string_of_int fn_id)) ~args:fn_args fn_prog.Syntax.env in
                fn_envs := Fns.add fn_name (fn_env, fn_prog.Syntax.code) !fn_envs;
                let (ret, taint, fn) = generate_function 0 recursed tainted [] [] fn_env 0 fn_prog.Syntax.code in
                result := Fns.add fn_name { code=fn; args=(List.length fn_args); returns=ret; tainted=taint; } !result;
                generate_function (next_id + 1) recursed tainted instrs (Const (Fn (Named fn_name)) :: values) env stored code
            end

            (* Named Value *)
            | Syntax.Value (Syntax.Ident n) -> generate_function next_id recursed tainted instrs (env_lookup recursed tainted env n values) env stored code

            (* Binary Operators *)
            | Syntax.Op Syntax.Plus -> generate_function next_id recursed tainted instrs (Const (Fn (BinOp Add)) :: values) env stored code
            | Syntax.Op Syntax.Minus -> generate_function next_id recursed tainted instrs (Const (Fn (BinOp Sub)) :: values) env stored code
            | Syntax.Op Syntax.Times -> generate_function next_id recursed tainted instrs (Const (Fn (BinOp Mul)) :: values) env stored code
            | Syntax.Op Syntax.Divide -> generate_function next_id recursed tainted instrs (Const (Fn (BinOp Div)) :: values) env stored code
            | Syntax.Op Syntax.Eq -> generate_function next_id recursed tainted instrs (Const (Fn (BinOp Eq)) :: values) env stored code
            | Syntax.Op Syntax.Lt -> generate_function next_id recursed tainted instrs (Const (Fn (BinOp Lt)) :: values) env stored code
            | Syntax.Op Syntax.Gt -> generate_function next_id recursed tainted instrs (Const (Fn (BinOp Gt)) :: values) env stored code

            | Syntax.Op Syntax.IfThenElse -> generate_function next_id recursed tainted instrs (Const (Fn (TriOp Ite)) :: values) env stored code

            (* Application *)
            | Syntax.Apply Syntax.Full -> begin match values with
                | Const (Fn (BinOp op)) :: rest -> begin 
                    let (stack_args, [x; y], rest) = pop_args 2 rest in
                    generate_function next_id recursed tainted instrs (apply_binop rest op x y) env stored code
                end

                | Const (Fn (TriOp op)) :: rest -> begin
                    let (stack_args, [x; y; z], rest) = pop_args 3 rest in
                    generate_function next_id recursed tainted instrs (apply_triop rest op x y z) env stored code
                end

                | Const (Fn (Named n)) :: rest -> if opt_flags.Flag.fe && not recursed then begin
                    (* Attempt inlining of function *)
                    try
                        let fn = Fns.find n !result in
                        let (fn_env, fn_code) = Fns.find n !fn_envs in
                        let (stack_args, args, rest) = pop_args (List.length fn_env.args) rest in
                        let (stored, values) = arg_values stored args in

                        generate_function 
                            next_id 
                            recursed 
                            tainted
                            (arg_instrs instrs args)
                            rest
                            { fn_env with 
                                arg_values=Some(values);
                                parent=(Some env);
                                name=None;
                            }
                            stored
                            (fn_code @ { Syntax.location=Lexing.dummy_pos; Syntax.data=Syntax.PopEnv; } :: code)
                    with
                        (* Attempt to inline recursion *)
                        Not_found ->
                            let (fn_env, fn_code) = Fns.find n !fn_envs in
                            begin match const_args (List.length fn_env.args) rest with
                                | Some (args, rest) -> 
                                    generate_function 
                                        next_id
                                        true
                                        tainted
                                        (arg_instrs instrs args)
                                        rest
                                        { fn_env with
                                            arg_values=Some(args);
                                            parent=(Some env);
                                            name=None;
                                        }
                                        stored
                                        (fn_code @ { Syntax.location=Lexing.dummy_pos; Syntax.data=Syntax.PopEnv; } :: code)

                                | None -> begin
                                    let (stack_args, args, rest) = pop_args (List.length fn_env.args) rest in
                                    let tainted = ((stack_args > 0) || tainted) in

                                    let instrs = if tainted then
                                        List.fold_right
                                            (fun v acc -> Push v :: acc)
                                            rest
                                            instrs
                                    else
                                        instrs
                                    in

                                    generate_function 
                                        next_id
                                        true
                                        tainted 
                                        (Apply (Recurse (n, args)) :: instrs) 
                                        (if tainted then [] else rest) 
                                        env 
                                        stored 
                                        code
                                end
                            end
                end else begin
                    try
                        let fn = Fns.find n !result in
                        let (stack_args, args, rest) = pop_args fn.args rest in
                        let tainted = (fn.tainted || (stack_args > 0) || tainted) in

                        let instrs = if tainted then
                            List.fold_right
                                (fun v acc -> Push v :: acc)
                                rest
                                instrs
                        else
                            instrs
                        in

                        generate_function 
                            next_id 
                            recursed 
                            tainted
                            (Apply (Named (n, args, fn.returns)) :: instrs) 
                            (make_ret fn.returns stored (if tainted then [] else rest)) 
                            env 
                            (stored + fn.returns) 
                            code
                    with
                        (* Recursion *)
                        Not_found -> begin
                            let (fn_env, _) = Fns.find n !fn_envs in
                            let (stack_args, args, rest) = pop_args (List.length fn_env.args) rest in
                            let tainted = ((stack_args > 0) || tainted) in

                            let instrs = if tainted then
                                List.fold_right
                                    (fun v acc -> Push v :: acc)
                                    rest
                                    instrs
                            else
                                instrs
                            in

                            generate_function 
                                next_id
                                true
                                tainted 
                                (Apply (Recurse (n, args)) :: instrs) 
                                (if tainted then [] else rest) 
                                env 
                                stored 
                                code
                        end
                end

                | v :: rest -> begin match fn_type v with
                    (* Known argument count *)
                    | (Some argc, Some retc, is_tainted) -> begin 
                        let (stack_args, args, rest) = pop_args argc rest in
                        let instrs = List.fold_right
                            (fun v acc -> Push v :: acc)
                            rest
                            instrs
                        in
                            
                        generate_function 
                            next_id 
                            recursed
                            (tainted || is_tainted)
                            (Apply (Value (v, args, Some retc)) :: instrs) 
                            (make_ret retc stored rest)
                            env
                            (stored + retc)
                            code
                    end

                    | (Some argc, None, _) -> begin 
                        let (stack_args, args, rest) = pop_args argc rest in
                        let instrs = List.fold_right
                            (fun v acc -> Push v :: acc)
                            rest
                            instrs
                        in

                        taint v;
                        generate_function 
                            next_id 
                            recursed
                            true
                            (Apply (Value (v, args, None)) :: instrs) 
                            []
                            env
                            stored
                            code
                    end
                end
            end

            (* Read *)
            | Syntax.Read Syntax.AsChar -> generate_function next_id recursed tainted instrs (Read AsChar :: values) env stored code
            | Syntax.Read Syntax.AsInt -> generate_function next_id recursed tainted instrs (Read AsInt :: values) env stored code

            (* Write *)
            | Syntax.Write Syntax.AsChar -> begin match values with
                | Const value :: _ -> generate_function next_id recursed tainted (WriteConst (AsChar, value) :: instrs) values env stored code
                | Stored n :: _ -> generate_function next_id recursed tainted (WriteStored (AsChar, n) :: instrs) values env stored code
                | value :: rest -> generate_function next_id recursed tainted (WriteStored (AsChar, stored) :: Store value :: instrs) (Stored stored :: rest) env (stored + 1) code
                | [] -> generate_function next_id recursed true (WriteStored (AsChar, stored) :: Store Pop :: instrs) [Stored stored] env (stored + 1) code
            end

            | Syntax.Write Syntax.AsInt -> begin match values with
                | Const value :: _ -> generate_function next_id recursed tainted (WriteConst (AsInt, value) :: instrs) values env stored code
                | Stored n :: _ -> generate_function next_id recursed tainted (WriteStored (AsInt, n) :: instrs) values env stored code
                | value :: rest -> generate_function next_id recursed tainted (WriteStored (AsInt, stored) :: Store value :: instrs) (Stored stored :: rest) env (stored + 1) code
                | [] -> generate_function next_id recursed true (WriteStored (AsInt, stored) :: Store Pop :: instrs) [Stored stored] env (stored + 1) code
            end

            (* Pop Environment *)
            | Syntax.PopEnv -> begin match env.parent with
                | Some parent -> generate_function next_id recursed tainted instrs values parent stored code
                | None -> generate_function next_id recursed tainted instrs values env stored code
            end
        end
    in

    let (_, tainted, fn) = generate_function 0 false false [] [] (make_env code.Syntax.env) 0 code.Syntax.code in

    if !errors = [] then
        Errors.Ok (Fns.add "" { code=fn; args=0; returns=0; tainted=tainted; } !result)
    else
        Errors.Err !errors
