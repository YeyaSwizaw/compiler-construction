(* Compiler Construction - instr.ml *)
(* Samuel Sleight *)

(* Instruction types *)
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

type value_t =
    | Int of int
    | Char of char
    | String of string
    | Fn of fn_t

type instruction =
    | PushConst of value_t
    | PushArg of int
    | Apply of apply_t

module Fns = Map.Make(String)

type 'a env = {
    env: 'a list;
    parent: ('a env) option;
    args: string list;
    arg_values: (instruction list) option;
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

    | PushConst (Fn (BinOp Add)) -> "Push[Fn[+]]"
    | PushConst (Fn (BinOp Sub)) -> "Push[Fn[-]]"
    | PushConst (Fn (BinOp Mul)) -> "Push[Fn[*]]"
    | PushConst (Fn (BinOp Div)) -> "Push[Fn[/]]"
    | PushConst (Fn (BinOp Eq)) -> "Push[Fn[=]]"
    | PushConst (Fn (BinOp Lt)) -> "Push[Fn[<]]"
    | PushConst (Fn (BinOp Gt)) -> "Push[Fn[>]]"

    | PushConst (Fn (TriOp Ite)) -> "Push[Fn[?]]"

    | PushConst (Fn (Named (s, _))) -> "Push[Fn[" ^ s ^ "]]"

    | PushArg i -> "Push[Arg[" ^ (string_of_int i) ^ "]]"

    | Apply Full -> "Apply[]"

let rec string_of_args = function
    | [] -> ""
    | [arg] -> arg
    | arg :: args -> arg ^ ", " ^ (string_of_args args)

let string_of_fns fns = 
    Fns.fold 
        (fun name fn acc -> 
            let fn_str = List.fold_left (fun acc instr -> acc ^ (string_of_instr instr)) "" fn.code in
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
        arg_values = None;
        name = name;
    } 
    in

    let fn_envs = ref Fns.empty in
    let result = ref Fns.empty in
    let errors = ref [] in

    let rec make_env_name env = match env.parent with
        | Some parent -> (make_env_name parent) ^ "_" ^ env.name
        | None -> env.name
    in

    let rec idx item = function
        | [] -> None 
        | hd::tl -> if item = hd then (Some 0) else begin match idx item tl with
            | Some n -> Some (n + 1)
            | None -> None
        end
    in

    let rec pop_args n instrs = if n = 0 then 
        Some ([], instrs)
    else match instrs with
        | ((PushConst _) as v) :: rest -> begin match pop_args (n - 1) rest with
            | Some (args, more) -> Some (v :: args, more)
            | None -> None
        end
        | other -> None
    in

    let apply_binop rest = function
        | (Add, Int i1, Int i2) -> (PushConst (Int (i1 + i2))) :: rest
        | (Mul, Int i1, Int i2) -> (PushConst (Int (i1 * i2))) :: rest
        | (Sub, Int i1, Int i2) -> (PushConst (Int (i1 - i2))) :: rest
        | (Div, Int i1, Int i2) -> (PushConst (Int (i1 / i2))) :: rest
        | (Eq, Int i1, Int i2) -> (PushConst (Int (if i1 = i2 then 1 else 0))) :: rest
        | (Gt, Int i1, Int i2) -> (PushConst (Int (if i1 > i2 then 1 else 0))) :: rest
        | (Lt, Int i1, Int i2) -> (PushConst (Int (if i1 < i2 then 1 else 0))) :: rest
        | (o, a1, a2) -> (Apply Full) :: (PushConst (Fn (BinOp o))) :: (PushConst a1) :: (PushConst a2) :: rest
    in

    let apply_triop rest = function
        | (Ite, Int i, c1, c2) -> (PushConst (if i == 0 then c2 else c1)) :: rest
        | (o, a1, a2, a3) -> (Apply Full) :: (PushConst (Fn (TriOp o))) :: (PushConst a1) :: (PushConst a2) :: (PushConst a3) :: rest
    in

    let attempt_fold = function
        | (PushConst (Fn (BinOp op))) :: (PushConst a1) :: (PushConst a2) :: rest -> apply_binop rest (op, a1, a2)
        | (PushConst (Fn (TriOp op))) :: (PushConst a1) :: (PushConst a2) :: (PushConst a3) :: rest -> apply_triop rest (op, a1, a2, a3)
        | xs -> (Apply Full) :: xs
    in

    let rec env_lookup ?(base=true) location name env = 
        (* Lookup in environment *)
        let rec list_lookup = function
            | [] -> None
            | `Int (n, i) :: tl -> if name = n then Some (PushConst (Int i)) else list_lookup tl
            | `Char (n, c) :: tl -> if name = n then Some (PushConst (Char c)) else list_lookup tl
            | `String (n, s) :: tl -> if name = n then Some (PushConst (String s)) else list_lookup tl
            | `Ident (n, i) :: tl -> if name = n then Some (env_lookup location i env) else list_lookup tl
            | `Fn (n, a, p) :: tl -> if name = n then begin
                let fn_name = (make_env_name env) ^ "_" ^ name in
                try 
                    let (_e, _) = Fns.find fn_name !fn_envs in
                    Some (PushConst (Fn (Named (fn_name, List.length a))))
                with
                    Not_found -> begin
                        let fn_env = make_env ~parent:(Some env) ~name:name ~args:a p.Syntax.env in
                        fn_envs := Fns.add fn_name (fn_env, p.Syntax.code) !fn_envs;
                        let fn = generate_function [] fn_env p.Syntax.code in
                        result := Fns.add fn_name { code=fn; args=(List.length a); } !result;
                        Some (PushConst (Fn (Named (fn_name, List.length a))))
                    end
            end else
                list_lookup tl
        in

        let parent_lookup () =
            (* Lookup in parent *)
            begin match env.parent with
                | Some parent ->
                    env_lookup 
                        ~base:false
                        location
                        name 
                        parent

                | None ->
                    errors := (Errors.undefined_name name location) :: !errors;
                    PushArg (-1)
            end
        in

        match list_lookup env.env with
            | Some thing -> thing
            | None -> begin
                (* Lookup in arguments *)
                match (base, idx name env.args) with
                    | (true, Some n) -> begin
                        if (n >= (List.length env.args)) || (n < 0) then begin
                            parent_lookup ()
                        end else begin
                            begin match env.arg_values with
                                | Some l -> List.nth l n
                                | None -> PushArg ((List.length env.args) - n + 1)
                            end
                        end
                    end

                    | _ -> parent_lookup ()
            end

    and generate_function instrs env code = match code with
        | [] -> instrs
        | exp_block :: tl -> begin match exp_block.Syntax.data with
            (* Push simple values *)
            | Syntax.Value (Syntax.Int i) -> generate_function (PushConst (Int i) :: instrs) env tl
            | Syntax.Value (Syntax.Char c) -> generate_function (PushConst (Char c) :: instrs) env tl
            | Syntax.Value (Syntax.String s) -> generate_function (PushConst (String s) :: instrs) env tl

            (* Push anon function *)
            | Syntax.Value (Syntax.Function (fn_args, fn_prog)) -> begin
                let fn_id = next_id () in
                let fn_name = (make_env_name env) ^ "_anon_" ^ (string_of_int fn_id) in
                let fn_env = make_env ~parent:(Some env) ~name:("anon_" ^ string_of_int fn_id) ~args:fn_args fn_prog.Syntax.env in
                fn_envs := Fns.add fn_name (fn_env, fn_prog.Syntax.code) !fn_envs;
                let fn = generate_function [] fn_env fn_prog.Syntax.code in
                result := Fns.add fn_name { code=fn; args=(List.length fn_args); } !result;
                generate_function (PushConst (Fn (Named (fn_name, List.length fn_args))) :: instrs) env tl
            end

            (* Push named value *)
            | Syntax.Value (Syntax.Ident i) -> generate_function ((env_lookup exp_block.Syntax.location i env) :: instrs) env tl 

            (* Push operators *)
            | Syntax.Op (Syntax.Plus) -> generate_function (PushConst (Fn (BinOp Add)) :: instrs) env tl
            | Syntax.Op (Syntax.Minus) -> generate_function (PushConst (Fn (BinOp Sub)) :: instrs) env tl
            | Syntax.Op (Syntax.Times) -> generate_function (PushConst (Fn (BinOp Mul)) :: instrs) env tl
            | Syntax.Op (Syntax.Divide) -> generate_function (PushConst (Fn (BinOp Div)) :: instrs) env tl
            | Syntax.Op (Syntax.Lt) -> generate_function (PushConst (Fn (BinOp Lt)) :: instrs) env tl
            | Syntax.Op (Syntax.Gt) -> generate_function (PushConst (Fn (BinOp Gt)) :: instrs) env tl
            | Syntax.Op (Syntax.Eq) -> generate_function (PushConst (Fn (BinOp Eq)) :: instrs) env tl
            | Syntax.Op (Syntax.IfThenElse) -> generate_function (PushConst (Fn (TriOp Ite)) :: instrs) env tl

            (* Apply function *)
            | Syntax.Apply Syntax.Full -> begin match instrs with
                | (PushConst (Fn (Named (n, a)))) :: rest -> begin
                    if opt_flags.Flag.cf then
                        let (fn_env, fn_code) = Fns.find n !fn_envs in
                        begin match pop_args a rest with
                            | Some (args, ls) -> begin
                                generate_function 
                                    ls 
                                    { fn_env with 
                                        arg_values=(Some args); 
                                        name=env.name; 
                                        parent=(Some env) 
                                    } 
                                    (fn_code @ { Syntax.location=Lexing.dummy_pos; Syntax.data=Syntax.PopEnv; } :: tl)
                            end

                            | None -> generate_function (Apply Full :: (PushConst (Fn (Named (n, a)))) :: rest) env tl
                        end
                    else
                        generate_function (Apply Full :: instrs) env tl
                end

                | other -> if opt_flags.Flag.cf then
                        generate_function (attempt_fold other) env tl
                    else
                        generate_function (Apply Full :: other) env tl
            end

            | Syntax.PopEnv -> begin match env.parent with
                | Some parent -> generate_function instrs parent tl
                | None -> generate_function instrs env tl
            end

            | _ -> begin
                errors := (Errors.not_implemented exp_block.Syntax.location) :: !errors;
                generate_function instrs env tl
            end
        end
    in

    let fn = generate_function [] (make_env code.Syntax.env) code.Syntax.code in

    let rec is_dead_from name fn =
        let called_fs = List.fold_left (fun acc item -> match item with 
            | PushConst (Fn (Named (n, _))) -> (n :: acc)
            | _ -> acc
        ) [] fn in

        List.fold_left (fun acc item -> 
            acc || if item = name then 
                true 
            else 
                (is_dead_from name (Fns.find item !result).code)
        ) false called_fs
    in

    if !errors = [] then
        Errors.Ok (Fns.add "" { code=fn; args=0; } (Fns.filter (fun key f -> is_dead_from key fn) !result))
    else
        Errors.Err !errors
