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

    let toplevel = fn_proto "sfl_entry" void_t [||] in
    let bb = append_block ctx "entry" toplevel in
    position_at_end bb bld;

    let args = Array.make 1 (const_int int_t 7) in
    build_call (fn_stack_push ()) args "" bld;

    let args = Array.make 1 (const_int int_t 20) in
    build_call (fn_stack_push ()) args "" bld;

    build_ret_void bld;

    print_string (string_of_llmodule mdl);

    Errors.Ok(())
