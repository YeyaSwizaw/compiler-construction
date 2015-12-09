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
    let get_fn_printf () = get_fn (var_arg_function_type void_t [|pointer_type (integer_type ctx 8)|]) "printf" in

    let format_str = define_global "format_str" (const_stringz ctx "%d") mdl in
    set_global_constant true format_str;

    let stack = let v = ref None in (fun () -> begin match !v with
        | None -> begin
            let stack = define_global "stack" (const_array int_t (Array.make 512 (undef int_t))) mdl in
            v := (Some stack);
            stack
        end

        | Some va -> va
    end) in

    let stack_count = let v = ref None in (fun () -> begin match !v with
        | None -> begin
            let va = define_global "stack_count" (const_int int_t 0) mdl in
            v := (Some va);
            va
        end

        | Some va -> va
    end) in

    (* Codegen *)
    let generate_function name code = 
        let fn_t = if name = "main" then main_t else (func_t code.Instr.returns) in
        let code_fn = get_fn fn_t name in

        let block = append_block ctx "entry" code_fn in
        position_at_end block bld;

        if name = "main" then begin
            ()
        end else begin
            set_function_call_conv CallConv.fast code_fn;
            set_linkage Linkage.Internal code_fn;
        end;

        (*
        let stack_pos_ptr = stack_count () in
        let stack_pos = build_load stack_pos_ptr "stack_pos" bld in
        let stack_ptr = build_in_bounds_gep (stack ()) [|const_int int_t 0; stack_pos|] "stack_ptr" bld in
        build_store (const_int int_t 15) stack_ptr bld;
        build_store (build_add stack_pos (const_int int_t 1) "new_pos" bld) stack_pos_ptr bld;
        *)

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
            | Instr.Read Instr.AsChar -> build_call (get_fn_getchar ()) [||] "input" bld

            | Instr.BinOp (op, x, y) -> begin
                let x_v = generate_value x in
                let y_v = generate_value y in
                match op with
                    | Instr.Add -> build_add x_v y_v "add_result" bld
                    | Instr.Sub -> build_sub x_v y_v "sub_result" bld
                    | Instr.Mul -> build_mul x_v y_v "mul_result" bld
                    | Instr.Div -> build_sdiv x_v y_v "div_result" bld

                    | Instr.Eq -> 
                        let cmp = build_icmp Icmp.Eq x_v y_v "eq_result" bld in
                        build_intcast cmp int_t "" bld

                    | Instr.Lt -> 
                        let cmp = build_icmp Icmp.Slt x_v y_v "lt_result" bld in
                        build_intcast cmp int_t "" bld

                    | Instr.Gt -> 
                        let cmp = build_icmp Icmp.Sgt x_v y_v "gt_result" bld in
                        build_intcast cmp int_t "" bld
            end

            | Instr.TriOp (op, q, x, y) -> begin
                let cmp = build_icmp Icmp.Ne (generate_value q) (const_int int_t 0) "ite_check" bld in
                let then_b = append_block ctx "true_case" code_fn in
                let else_b = append_block ctx "false_case" code_fn in
                let join_b = append_block ctx "join_ite" code_fn in

                ignore (build_cond_br cmp then_b else_b bld);

                position_at_end then_b bld;
                let then_v = generate_value x in
                ignore (build_br join_b bld);

                position_at_end else_b bld;
                let else_v = generate_value y in
                ignore (build_br join_b bld);

                position_at_end join_b bld;
                build_phi [then_v, then_b; else_v, else_b] "ite_result" bld
            end

            | Instr.Arg n -> begin
                let args = param code_fn 0 in
                let ep = build_gep args [|const_int int_t n|] ("arg_" ^ string_of_int n ^ "_ptr") bld in
                build_load ep ("arg_" ^ string_of_int n) bld
            end

            | Instr.Pop -> begin
                let stack_pos_ptr = stack_count () in
                let stack_pos = build_sub (build_load stack_pos_ptr "stack_pos" bld) (const_int int_t 1) "new_pos" bld in
                let stack_ptr = build_in_bounds_gep (stack ()) [|const_int int_t 0; stack_pos|] "stack_ptr" bld in
                ignore (build_store stack_pos stack_pos_ptr bld);
                build_load stack_ptr "popped_value" bld
            end
        in

        let generate_instr = function
            | Instr.WriteConst (Instr.AsChar, v) -> begin
                let value = generate_value (Instr.Const v) in
                ignore (build_call (get_fn_putchar ()) [|value|] "" bld)
            end

            | Instr.WriteConst (Instr.AsInt, v) -> begin
                let value = generate_value (Instr.Const v) in
                let sp = build_gep format_str [|const_int int_t 0|] "write_ptr" bld in
                let spp = build_bitcast sp (pointer_type (integer_type ctx 8)) "write_val" bld in
                ignore (build_call (get_fn_printf ()) [|spp; value|] "" bld)
            end

            | Instr.WriteStored (Instr.AsChar, n) -> begin
                let value = generate_value (Instr.Stored n) in
                ignore (build_call (get_fn_putchar ()) [|value|] "" bld)
            end

            | Instr.WriteStored (Instr.AsInt, n) -> begin
                let value = generate_value (Instr.Stored n) in
                let sp = build_gep format_str [|const_int int_t 0|] "write_ptr" bld in
                let spp = build_bitcast sp (pointer_type (integer_type ctx 8)) "write_val" bld in
                ignore (build_call (get_fn_printf ()) [|spp; value|] "" bld)
            end

            | Instr.Store v -> begin
                let value = generate_value v in
                stored_values := Values.add (next_value ()) value !stored_values
            end

            | Instr.Apply (Instr.Named (n, args, ret)) -> begin
                let mem = build_array_alloca int_t (const_int int_t (List.length args)) "arg_array" bld in

                List.iteri (fun i arg ->
                    let ep = build_gep mem [|const_int int_t i|] ("arg_" ^ string_of_int i ^ "_ptr") bld in
                    ignore (build_store (generate_value arg) ep bld)
                ) args;

                if ret = 0 then
                    ignore (build_call (get_fn (func_t ret) n) [|mem|] "" bld)
                else
                    let retobj = build_call (get_fn (func_t ret) n) [|mem|] "call_result" bld in

                    let rec make_ret_store retn =
                        if retn = 0 then
                            ()
                        else begin
                            stored_values := Values.add (next_value ()) (build_extractvalue retobj (retn - 1) "ret_value" bld) !stored_values;
                            make_ret_store (retn - 1)
                        end
                    in

                    make_ret_store ret
            end

            | Instr.Apply (Instr.Value (v, args, ret)) -> begin
                let call_fn_t = func_t (match ret with | None -> 0 | Some n -> n) in
                let fn_val = build_inttoptr (generate_value v) (pointer_type call_fn_t) "fn_val" bld in

                let mem = build_array_alloca int_t (const_int int_t (List.length args)) "arg_array" bld in

                List.iteri (fun i arg ->
                    let ep = build_gep mem [|const_int int_t i|] ("arg_" ^ string_of_int i ^ "_ptr") bld in
                    ignore (build_store (generate_value arg) ep bld)
                ) args;

                match ret with
                    | None | Some 0 -> ignore (build_call fn_val [|mem|] "" bld)

                    | Some n -> begin
                        let retobj = build_call fn_val [|mem|] "call_result" bld in

                        let rec make_ret_store retn =
                            if retn = 0 then
                                ()
                            else begin
                                stored_values := Values.add (next_value ()) (build_extractvalue retobj (retn - 1) "ret_value" bld) !stored_values;
                                make_ret_store (retn - 1)
                            end
                        in

                        make_ret_store n
                    end
            end

            | Instr.Apply (Instr.Recurse (n, args)) -> begin
                let mem = build_array_alloca int_t (const_int int_t (List.length args)) "arg_array" bld in

                List.iteri (fun i arg ->
                    let ep = build_gep mem [|const_int int_t i|] ("arg_" ^ string_of_int i ^ "_ptr") bld in
                    ignore (build_store (generate_value arg) ep bld)
                ) args;

                ignore (build_call (get_fn (func_t 0) n) [|mem|] "" bld)
            end

            | Instr.Push v -> begin
                let value = generate_value v in
                let stack_pos_ptr = stack_count () in
                let stack_pos = build_load stack_pos_ptr "stack_pos" bld in
                let stack_ptr = build_in_bounds_gep (stack ()) [|const_int int_t 0; stack_pos|] "stack_ptr" bld in
                build_store value stack_ptr bld;
                ignore (build_store (build_add stack_pos (const_int int_t 1) "new_pos" bld) stack_pos_ptr bld);
            end

            | Instr.Return [] ->
                if not (name = "main") then begin
                    ignore (build_ret_void bld)
                end else
                    ()
            
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
