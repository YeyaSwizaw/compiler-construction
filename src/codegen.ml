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

    let main_t = function_type void_t [||] in
    let func_t returns = function_type void_t (Array.make (if returns = 0 then 1 else 2) ptr_t) in

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

            | Instr.Apply (n, args, ret) -> begin
                let mem = build_array_alloca int_t (const_int int_t (ret + (List.length args))) "" bld in

                List.iteri (fun i arg ->
                    let ep = build_gep mem [|const_int int_t i|] "" bld in
                    ignore (build_store (generate_value arg) ep bld)
                ) args;

                if ret = 0 then
                    ignore (build_call (get_fn (func_t ret) n) [|mem|] "" bld)
                else
                    let retp = build_gep mem [|const_int int_t (List.length args)|] "" bld in
                    let rec make_ret_store retn =
                        if retn = 0 then
                            ()
                        else begin
                            let ep = build_gep retp [|const_int int_t (retn - 1)|] "" bld in
                            stored_values := Values.add (next_value ()) (build_load ep "" bld) !stored_values;
                            make_ret_store (retn - 1)
                        end
                    in

                    build_call (get_fn (func_t ret) n) [|mem; retp|] "" bld;
                    make_ret_store ret
            end
            
            | Instr.Return ret -> begin
                if not (name = "main") then begin
                    let rets = param code_fn 1 in
                    List.iteri (fun i v ->
                        let ep = build_gep rets [|const_int int_t i|] "" bld in
                        ignore (build_store (generate_value v) ep bld)
                    ) ret
                end else
                    ()
            end
        in

        List.iter generate_instr code.Instr.code;
        ignore (build_ret_void bld)
    in


    Instr.Fns.iter (fun name instrs -> generate_function (if name = "" then "main" else name) instrs) fns;
    Errors.Ok (string_of_llmodule mdl)
