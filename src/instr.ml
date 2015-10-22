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

type fn_t = 
    | BinOp of binop_t

type apply_t =
    | Full

type instruction =
    | PushConst of value_t
    | PushFn of fn_t
    | Apply of apply_t

module Fns = Map.Make(String)

(* Stringifying *)
let string_of_instr = function
    | PushConst (Int i) -> "Push[Int[" ^ (string_of_int i) ^"]]"
    | PushConst (Char c) -> "Push[Char[" ^ (String.make 1 c) ^"]]"
    | PushConst (String s) -> "Push[Int[" ^ s ^"]]"

    | PushFn (BinOp Add) -> "Push[Fn[+]]"
    | PushFn (BinOp Sub) -> "Push[Fn[-]]"
    | PushFn (BinOp Mul) -> "Push[Fn[*]]"
    | PushFn (BinOp Div) -> "Push[Fn[/]]"

    | Apply Full -> "Apply[]"

let string_of_fns fns = 
    Fns.fold 
        (fun name code acc -> 
            let buf = Buffer.create 17 in
            (Stack.iter (fun instr -> Buffer.add_string buf (string_of_instr instr)) code);
            name ^ ":" ^ Buffer.contents buf
        ) fns ""

(* Convert parse tree to instructions *)
let generate_instructions code =
    let rec generate_function fn = 
        let output = Stack.create () in
        let queue = Stack.create () in

        let apply_binop op v1 v2 = match (op, v1, v2) with
            | (Add, Int i1, Int i2) -> (Stack.push (PushConst (Int (i1 + i2))) output; Errors.Ok(()))
            | (Sub, Int i1, Int i2) -> (Stack.push (PushConst (Int (i1 - i2))) output; Errors.Ok(()))
            | (Mul, Int i1, Int i2) -> (Stack.push (PushConst (Int (i1 * i2))) output; Errors.Ok(()))
            | (Div, Int i1, Int i2) -> (Stack.push (PushConst (Int (i1 / i2))) output; Errors.Ok(()))
            (* TODO: endless patterns *)
        in

        let rec pop_args acc n = if n = 0 then
            Errors.Ok(acc)
        else 
            try
                match Stack.pop output with
                    | PushConst v -> pop_args (v :: acc) (n - 1)
                    | other -> (Stack.push other output; Errors.Ok(acc))
            with
                Stack.Empty -> Errors.Ok(acc) (* TODO *)
        in

        let attempt_full_fold () = (
            try
                match Stack.pop output with
                    | PushFn (BinOp op) -> (
                        match pop_args [] 2 with
                            | Errors.Ok([a2; a1]) -> apply_binop op a1 a2
                            | Errors.Ok(other) -> (List.iter (fun thing -> Stack.push (PushConst thing) output) other; Errors.Ok(()))
                            | Errors.Err(err) -> Errors.Err(err)
                    )

                    | other -> Errors.Err([]) (* TODO *)
            with
                Stack.Empty -> Errors.Ok(()) (* TODO *)
        ) in

        let rec loop = function
            | [] -> Errors.Ok(output)
            | expr :: tl -> (match expr.Syntax.data with
                (* Push simple values *)
                | Syntax.Value (Syntax.Int i) -> (Stack.push (PushConst (Int i)) output; loop tl)
                | Syntax.Value (Syntax.Char c) -> (Stack.push (PushConst (Char c)) output; loop tl)
                | Syntax.Value (Syntax.String s) -> (Stack.push (PushConst (String s)) output; loop tl)

                (* Name lookup *)
                | Syntax.Value (Syntax.Ident name) -> (
                    try 
                        match (Syntax.Env.find name fn.Syntax.env).Syntax.data with
                            (* Push simple values *)
                            | Syntax.Int i -> (Stack.push (PushConst (Int i)) output; loop tl)
                            | Syntax.Char c -> (Stack.push (PushConst (Char c)) output; loop tl)
                            | Syntax.String s -> (Stack.push (PushConst (String s)) output; loop tl)

                    with
                        Not_found -> Errors.Err([Errors.undefined_name name expr.Syntax.location]) (* TODO *)
                )

                (* Push binary operators *)
                | Syntax.Op Syntax.Plus -> (Stack.push (PushFn (BinOp Add)) output; loop tl)
                | Syntax.Op Syntax.Minus -> (Stack.push (PushFn (BinOp Sub)) output; loop tl)
                | Syntax.Op Syntax.Times -> (Stack.push (PushFn (BinOp Mul)) output; loop tl)
                | Syntax.Op Syntax.Divide -> (Stack.push (PushFn (BinOp Div)) output; loop tl)

                (* Application - attempt constant fold *)
                | Syntax.Apply Syntax.Full -> (match attempt_full_fold () with
                    | Errors.Ok(()) -> loop tl
                    | Errors.Err(errs) -> Errors.Err(errs)
                )

                | _ -> loop tl;
            )
        in 

        loop fn.Syntax.code
    in

    match generate_function code with
        | Errors.Ok(fn) -> Errors.Ok(Fns.singleton "" fn)
        | Errors.Err(errs) -> Errors.Err(errs)
