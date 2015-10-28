(* Compiler Construction - codegen.ml *)
(* Samuel Sleight *)

open Llvm

let generate_code instrs = 
    let ctx = global_context () in
    let mdl = create_module ctx "sfl_mdl" in
    let bld = builder ctx in

    (* Typedefs *)
    let void_t = void_type ctx in
    let int_t = integer_type ctx 32 in

    let fn_proto name ret_t arg_ts = 
        let fn_t = function_type ret_t arg_ts in
        match lookup_function name mdl with
            | None -> declare_function name fn_t mdl
            | Some f -> f
    in

    (* External Functions *)
    let fn_print_int () = fn_proto "print_int" void_t (Array.make 1 int_t) in
    let fn_stack_push () = fn_proto "stack_push" void_t (Array.make 1 int_t) in
    let fn_stack_push_add () = fn_proto "stack_push_add" void_t [||] in
    let fn_apply_full () = fn_proto "apply_full" void_t [||] in

    let build_push i =
        let args = Array.make 1 (const_int int_t i) in
        build_call (fn_stack_push ()) args "" bld;
    in

    let generate_function name code = 
        let code_fn_proto = fn_proto name void_t (Array.make (List.length code.Instr.args) int_t) in
        let block = append_block ctx "entry" code_fn_proto in
        position_at_end block bld;

        let generate_expr = function
            | Instr.PushConst (Instr.Int i) -> ignore (build_push i)
            | Instr.PushFn (Instr.BinOp Instr.Add) -> ignore (build_call (fn_stack_push_add ()) [||] "" bld)
            | Instr.Apply (Instr.Full) -> ignore (build_call (fn_apply_full ()) [||] "" bld)
        in

        Stack.iter generate_expr code.Instr.code;
        ignore (build_ret_void bld)
    in

    Instr.Fns.iter (fun name code -> (
        let stack = Stack.create () in
        Stack.iter (fun item -> Stack.push item stack) code.Instr.code;
        generate_function (if name = "" then "sfl_entry" else name) { code with Instr.code = stack };
    )) instrs;

    Errors.Ok (string_of_llmodule mdl);
