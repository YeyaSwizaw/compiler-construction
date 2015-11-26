(* Compiler Construction - codegen.ml *)
(* Samuel Sleight *)

open Llvm

module Values = Map.Make(struct type t = int let compare = compare end)
module Fns = Map.Make(String)

let generate_code opt_flags size_flags fns = 
    let ctx = global_context () in
    let mdl = create_module ctx "sfl_mdl" in
    let bld = builder ctx in

    let used_fns = ref Fns.empty in

    (* Typedefs *)
    let void_t = void_type ctx in
    let int_t = integer_type ctx 64 in
    let ptr_t = pointer_type int_t in
    let ret_t = array_type int_t in

    let main_t = function_type void_t [||] in
    let func_t returns = function_type (if returns = 0 then void_t else ret_t returns) [|ptr_t|] in

    let get_fn fn_t name = try
        Fns.find name !used_fns
    with
        Not_found -> begin
            let fn = declare_function name fn_t mdl in
            used_fns := Fns.add name fn !used_fns;
            fn
        end
    in
    
    let get_fn_putchar () = get_fn (function_type void_t [|int_t|]) "putchar" in
    let get_fn_getchar () = get_fn (function_type int_t [||]) "getchar" in

    (* Codegen *)
    let generate_function name code = 
        let fn_t = if name = "main" then main_t else (func_t code.Instr.returns) in
        let code_fn = get_fn fn_t name in
        let block = append_block ctx "entry" code_fn in

        set_function_call_conv (if name = "main" then CallConv.c else CallConv.fast) code_fn;
        position_at_end block bld;

        let next_value = let next = ref 0 in (fun () -> next := !next + 1; !next - 1) in
        let stored_values = ref Values.empty in

        let rec generate_value  = function
            | Instr.Const (Instr.Int i) -> const_int int_t i

            | Instr.Const (Instr.Fn (Instr.Named n)) -> begin
                let fn = Instr.Fns.find n fns in
                let func = get_fn (func_t fn.Instr.returns) n in
                const_ptrtoint func int_t
            end

            | Instr.Stored n -> Values.find n !stored_values
            | Instr.Read Instr.AsChar -> build_call (get_fn_getchar ()) [||] "" bld

            | Instr.BinOp (op, x, y) -> begin
                let x_v = generate_value x in
                let y_v = generate_value y in
                match op with
                    | Instr.Add -> build_add x_v y_v "" bld
                    | Instr.Sub -> build_sub x_v y_v "" bld
                    | Instr.Mul -> build_mul x_v y_v "" bld
                    | Instr.Div -> build_sdiv x_v y_v "" bld

                    | Instr.Eq -> 
                        let cmp = build_icmp Icmp.Eq x_v y_v "" bld in
                        build_intcast cmp int_t "" bld

                    | Instr.Lt -> 
                        let cmp = build_icmp Icmp.Slt x_v y_v "" bld in
                        build_intcast cmp int_t "" bld

                    | Instr.Gt -> 
                        let cmp = build_icmp Icmp.Sgt x_v y_v "" bld in
                        build_intcast cmp int_t "" bld
            end

            | Instr.TriOp (op, q, x, y) -> begin
                let cmp = build_icmp Icmp.Ne (generate_value q) (const_int int_t 0) "" bld in
                let then_b = append_block ctx "true_case" code_fn in
                let else_b = append_block ctx "false_case" code_fn in
                let join_b = append_block ctx "join_if" code_fn in

                build_cond_br cmp then_b else_b bld;

                position_at_end then_b bld;
                let then_v = generate_value x in
                build_br join_b bld;

                position_at_end else_b bld;
                let else_v = generate_value y in
                build_br join_b bld;

                position_at_end join_b bld;
                build_phi [then_v, then_b; else_v, else_b] "" bld
            end

            | Instr.Arg n -> begin
                let args = param code_fn 0 in
                let ep = build_gep args [|const_int int_t n|] "" bld in
                build_load ep "" bld
            end
        in

        let generate_instr = function
            | Instr.WriteConst (Instr.AsChar, v) -> begin
                let value = generate_value (Instr.Const v) in
                ignore (build_call (get_fn_putchar ()) [|value|] "" bld)
            end

            | Instr.WriteStored (Instr.AsChar, n) -> begin
                let value = generate_value (Instr.Stored n) in
                ignore (build_call (get_fn_putchar ()) [|value|] "" bld)
            end

            | Instr.Store v -> begin
                let value = generate_value v in
                stored_values := Values.add (next_value ()) value !stored_values
            end

            | Instr.Apply (Instr.Named (n, args, ret)) -> begin
                let mem = build_array_alloca int_t (const_int int_t (List.length args)) "" bld in

                List.iteri (fun i arg ->
                    let ep = build_gep mem [|const_int int_t i|] "" bld in
                    ignore (build_store (generate_value arg) ep bld)
                ) args;

                if ret = 0 then
                    ignore (build_call (get_fn (func_t ret) n) [|mem|] "" bld)
                else
                    let retobj = build_call (get_fn (func_t ret) n) [|mem|] "" bld in

                    let rec make_ret_store retn =
                        if retn = 0 then
                            ()
                        else begin
                            stored_values := Values.add (next_value ()) (build_extractvalue retobj (retn - 1) "" bld) !stored_values;
                            make_ret_store (retn - 1)
                        end
                    in

                    make_ret_store ret
            end

            | Instr.Apply (Instr.Value (v, args, ret)) -> begin
                let call_fn_t = func_t ret in
                let fn_val = build_inttoptr (generate_value v) (pointer_type call_fn_t) "" bld in

                let mem = build_array_alloca int_t (const_int int_t (List.length args)) "" bld in

                List.iteri (fun i arg ->
                    let ep = build_gep mem [|const_int int_t i|] "" bld in
                    ignore (build_store (generate_value arg) ep bld)
                ) args;

                if ret = 0 then
                    ignore (build_call fn_val [|mem|] "" bld)
                else
                    let retobj = build_call fn_val [|mem|] "" bld in

                    let rec make_ret_store retn =
                        if retn = 0 then
                            ()
                        else begin
                            stored_values := Values.add (next_value ()) (build_extractvalue retobj (retn - 1) "" bld) !stored_values;
                            make_ret_store (retn - 1)
                        end
                    in

                    make_ret_store ret
            end
            
            | Instr.Return ret -> begin
                if not (name = "main") then begin
                    let ls = List.map generate_value ret in
                    ignore (build_aggregate_ret (Array.of_list ls) bld)
                end else
                    ()
            end
        in

        List.iter generate_instr code.Instr.code;
        if name = "main" then ignore (build_ret_void bld) else ()
    in


    Instr.Fns.iter (fun name instrs -> generate_function (if name = "" then "main" else name) instrs) fns;
    Errors.Ok (string_of_llmodule mdl)
