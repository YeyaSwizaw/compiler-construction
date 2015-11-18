(* Compiler Construction - codegen.ml *)
(* Samuel Sleight *)

module S = Set.Make(String)

let rec idx item = function
    | [] -> 0
    | hd::tl -> if item = hd then 0 else 1 + (idx item tl)

let generate_code opt_flags size_flags instrs = 
    let asm = Buffer.create 1024 in
    let used_fns = ref S.empty in

    let generate_code curr_name code = 
        let push_queue = Queue.create () in

        let push_fn args name =
            Queue.push (`Fn (name, args)) push_queue;
            used_fns := S.add name !used_fns
        in

        let generate_expr = function
            | Instr.PushConst (Instr.Int i) -> Queue.push (`Int i) push_queue
            | Instr.PushArg n -> Queue.push (`Arg n) push_queue
            | Instr.PushFn (Instr.BinOp Instr.Add) -> push_fn 2 "add"
            | Instr.PushFn (Instr.BinOp Instr.Sub) -> push_fn 2 "sub"
            | Instr.PushFn (Instr.BinOp Instr.Mul) -> push_fn 2 "mul"
            | Instr.PushFn (Instr.BinOp Instr.Div) -> push_fn 2 "div"
            | Instr.PushFn (Instr.BinOp Instr.Eq) -> push_fn 2 "eq_cmp"
            | Instr.PushFn (Instr.BinOp Instr.Lt) -> push_fn 2 "lt_cmp"
            | Instr.PushFn (Instr.BinOp Instr.Gt) -> push_fn 2 "gt_cmp"
            | Instr.PushFn (Instr.TriOp Instr.Ite) -> push_fn 3 "ite"
            | Instr.PushFn (Instr.Named (s, n)) -> push_fn n s

            | other -> begin
                if not (Queue.is_empty push_queue) then begin
                    Buffer.add_string asm (Asm.push_block push_queue);
                    Queue.clear push_queue;
                end;

                begin match other with
                    | Instr.Apply Instr.Full -> Buffer.add_string asm (Asm.apply_block opt_flags ())
                end;
            end
        in

        List.iter generate_expr (List.rev code.Instr.code);

        if not (Queue.is_empty push_queue) then begin
            Buffer.add_string asm (Asm.push_block push_queue);
            Queue.clear push_queue;
        end;
    in

    let generate_function name code =
        if name = "" then begin 
            Buffer.add_string asm Asm.main_begin;
            generate_code name code;
            Buffer.add_string asm Asm.debug_end;
            Buffer.add_string asm Asm.main_end;
        end else begin
            Buffer.add_string asm (Asm.function_begin name);
            generate_code name code;
            Buffer.add_string asm (Asm.function_end code.Instr.args)
        end
    in

    Buffer.add_string asm (Asm.data_segment size_flags);
    Instr.Fns.iter generate_function instrs;
    S.iter (fun op -> Buffer.add_string asm (Asm.op_block op)) !used_fns;

    Errors.Ok (Buffer.contents asm)
