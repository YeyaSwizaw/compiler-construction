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
    | Named of string

type apply_t =
    | Full

type instruction =
    | PushConst of value_t
    | PushFn of fn_t
    | PushArg of string
    | PushSelf
    | Apply of apply_t

type block = {
    args: string list;
    code: instruction Stack.t;
}

module Fns = Map.Make(String)

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

    | PushFn (Named s) -> "Push[Fn[" ^ s ^ "]]"

    | PushArg s -> "Push[Arg[" ^ s ^ "]]"

    | PushSelf -> "Push[Self]"

    | Apply Full -> "Apply[]"

let rec string_of_args = function
    | [] -> ""
    | [arg] -> arg
    | arg :: args -> arg ^ ", " ^ (string_of_args args)

let string_of_fns fns = 
    Fns.fold 
        (fun name code acc -> 
            let buf = Buffer.create 17 in
            (Stack.iter (fun instr -> Buffer.add_string buf (string_of_instr instr)) code.code);
            acc ^ name ^ ":[" ^ (string_of_args code.args) ^ "]:" ^ Buffer.contents buf ^ "\n"
        ) fns ""

(* Convert parse tree to instructions *)
let generate_instructions opt_flags code =
    let next_id = let prev = ref 0 in (fun () -> prev := !prev + 1; !prev) in

    let result = ref Fns.empty in
    let errors = ref [] in

    let rec generate_function ?(name="") ?(unique_name=name) ?(fn_args=[]) ?(args=[]) ?(parent_names=[]) fn (env::parents) = 
        let output = Stack.create () in

        let apply_binop op v1 v2 = match (op, v1, v2) with
            | (Add, Int i1, Int i2) -> Stack.push (PushConst (Int (i1 + i2))) output
            | (Sub, Int i1, Int i2) -> Stack.push (PushConst (Int (i1 - i2))) output
            | (Mul, Int i1, Int i2) -> Stack.push (PushConst (Int (i1 * i2))) output
            | (Div, Int i1, Int i2) -> Stack.push (PushConst (Int (i1 / i2))) output
            | (Eq, Int i1, Int i2) -> Stack.push (PushConst (Int (if i1 = i2 then 1 else 0))) output
            | (Gt, Int i1, Int i2) -> Stack.push (PushConst (Int (if i1 > i2 then 1 else 0))) output
            | (Lt, Int i1, Int i2) -> Stack.push (PushConst (Int (if i1 < i2 then 1 else 0))) output

            (* TODO: endless patterns *)
        in

        let rec pop_args acc n = if n = 0 then
            acc
        else 
            try
                match Stack.pop output with
                    | PushConst v -> pop_args (v :: acc) (n - 1)
                    | other -> (Stack.push other output; acc)
            with
                Stack.Empty -> acc
        in

        let attempt_full_fold () = (
            try
                match Stack.pop output with
                    | PushFn (BinOp op) -> (
                        match pop_args [] 2 with
                            | [a2; a1] -> apply_binop op a1 a2
                            | other -> (
                                List.iter (fun thing -> Stack.push (PushConst thing) output) other;
                                Stack.push (PushFn (BinOp op)) output;
                                Stack.push (Apply Full) output;
                            )
                    )

                    | other -> (
                        Stack.push other output;
                        Stack.push (Apply Full) output;
                    )
            with
                Stack.Empty -> Stack.push (Apply Full) output;
        ) in

        let lookup_item name env =
            try
                Some (Syntax.Env.find name env)
            with
                Not_found -> None
        in

        let rec lookup_loop name = function
            | [] -> None
            | hd :: tl -> match lookup_item name hd with
                | Some thing -> Some thing
                | None -> lookup_loop name tl
        in

        let rec loop = function
            | [] -> result := (Fns.add unique_name {args=fn_args; code=output} !result)

            | expr :: tl -> (match expr.Syntax.data with
                (* Push simple values *)
                | Syntax.Value (Syntax.Int i) -> (Stack.push (PushConst (Int i)) output; loop tl)
                | Syntax.Value (Syntax.Char c) -> (Stack.push (PushConst (Char c)) output; loop tl)
                | Syntax.Value (Syntax.String s) -> (Stack.push (PushConst (String s)) output; loop tl)

                | Syntax.Value (Syntax.Function (fn_args, fn_code)) -> (
                    let fn_name = string_of_int (next_id ()) in
                    generate_function 
                        ~name:fn_name 
                        ~fn_args:fn_args
                        ~args:(args @ fn_args) 
                        ~parent_names:((name, unique_name) :: parent_names)
                        fn_code.Syntax.code
                        (fn_code.Syntax.env :: env :: parents);

                    Stack.push (PushFn (Named fn_name)) output;
                    loop tl
                )

                (* Name lookup *)
                | Syntax.Value (Syntax.Ident var_name) -> (
                    match (lookup_item var_name env) with
                        (* Push simple values *)
                        | Some(val_chunk) -> (match val_chunk.Syntax.data with 
                            | Syntax.Int i -> (Stack.push (PushConst (Int i)) output; loop tl)
                            | Syntax.Char c -> (Stack.push (PushConst (Char c)) output; loop tl)
                            | Syntax.String s -> (Stack.push (PushConst (String s)) output; loop tl)

                            | Syntax.Function (fn_args, fn_code) -> (
                                let fn_name = name ^ var_name ^ (string_of_int (next_id ())) in
                                generate_function 
                                    ~name:var_name 
                                    ~unique_name:fn_name 
                                    ~fn_args:fn_args
                                    ~args:(args @ fn_args) 
                                    ~parent_names:((name, unique_name) :: parent_names)
                                    fn_code.Syntax.code
                                    (fn_code.Syntax.env :: env :: parents);

                                Stack.push (PushFn (Named fn_name)) output;
                                loop tl
                            )

                            | Syntax.Ident n -> loop ({Syntax.data=(Syntax.Value (Syntax.Ident n)); Syntax.location=val_chunk.Syntax.location} :: tl)
                        );

                        | None -> (
                            if List.mem var_name args then
                                (Stack.push (PushArg var_name) output; loop tl)
                            else if name = var_name then
                                (Stack.push PushSelf output; loop tl)
                            else if List.mem_assoc var_name parent_names then
                                (Stack.push (PushFn (Named (List.assoc var_name parent_names))) output; loop tl)
                            else
                                match lookup_loop var_name parents with
                                    | Some(var_chunk) -> (match var_chunk.Syntax.data with 
                                        | Syntax.Int i -> (Stack.push (PushConst (Int i)) output; loop tl)
                                        | Syntax.Char c -> (Stack.push (PushConst (Char c)) output; loop tl)
                                        | Syntax.String s -> (Stack.push (PushConst (String s)) output; loop tl)

                                        | Syntax.Function (fn_args, fn_code) -> (
                                            let fn_name = name ^ var_name ^ (string_of_int (next_id ())) in
                                            generate_function 
                                                ~name:var_name 
                                                ~unique_name:fn_name 
                                                ~fn_args:fn_args
                                                ~args:(args @ fn_args) 
                                                ~parent_names:((name, unique_name) :: parent_names)
                                                fn_code.Syntax.code
                                                (fn_code.Syntax.env :: env :: parents);

                                            Stack.push (PushFn (Named fn_name)) output;
                                            loop tl
                                        )

                                        | Syntax.Ident n -> loop ({Syntax.data=(Syntax.Value (Syntax.Ident n)); Syntax.location=var_chunk.Syntax.location} :: tl)
                                    );

                                    | None -> (errors := (Errors.undefined_name var_name expr.Syntax.location) :: !errors; loop tl)
                        )
                )

                (* Push binary operators *)
                | Syntax.Op Syntax.Plus -> (Stack.push (PushFn (BinOp Add)) output; loop tl)
                | Syntax.Op Syntax.Minus -> (Stack.push (PushFn (BinOp Sub)) output; loop tl)
                | Syntax.Op Syntax.Times -> (Stack.push (PushFn (BinOp Mul)) output; loop tl)
                | Syntax.Op Syntax.Divide -> (Stack.push (PushFn (BinOp Div)) output; loop tl)
                | Syntax.Op Syntax.Eq -> (Stack.push (PushFn (BinOp Eq)) output; loop tl)
                | Syntax.Op Syntax.Gt -> (Stack.push (PushFn (BinOp Gt)) output; loop tl)
                | Syntax.Op Syntax.Lt -> (Stack.push (PushFn (BinOp Lt)) output; loop tl)

                | Syntax.Op Syntax.IfThenElse -> (Stack.push (PushFn (TriOp Ite)) output; loop tl)

                (* Application - attempt constant fold *)
                | Syntax.Apply Syntax.Full -> (
                    if opt_flags.Flags.cf then (
                        attempt_full_fold (); 
                        loop tl
                    ) else (
                        Stack.push (Apply Full) output;
                        loop tl
                    )
                )

                | _ -> loop tl;
            );
        in 

        loop fn
    in

    generate_function code.Syntax.code [code.Syntax.env];

    if !errors = [] then
        Errors.Ok(!result)
    else
        Errors.Err(List.rev !errors)
