(* Compiler Construction - codegen.ml *)
(* Samuel Sleight *)

open Llvm

module Fns = Map.Make(String)

let generate_code instrs = 
    let ctx = global_context () in
    let mdl = create_module ctx "sfl_mdl" in
    let bld = builder ctx in

    (* Typedefs *)
    let void_t = void_type ctx in
    let int_t = integer_type ctx 32 in
    let ptr_t = pointer_type (integer_type ctx 8) in

    let obj_t = named_struct_type ctx "sfl_object" in
    struct_set_body obj_t [| int_t; int_t; int_t; ptr_t; ptr_t |] false;

    let fn_t = function_type void_t [| pointer_type obj_t |] in

    let fn_proto name ret_t arg_ts = 
        let fn_t = function_type ret_t arg_ts in
        match lookup_function name mdl with
            | None -> declare_function name fn_t mdl
            | Some f -> f
    in

    let fn_t_proto name = match lookup_function name mdl with
        | None -> declare_function name fn_t mdl
        | Some f -> f
    in

    (* External Functions *)
    let fn_print_int () = fn_proto "print_int" void_t (Array.make 1 int_t) in
    let fn_stack_push_int () = fn_proto "stack_push_int" void_t (Array.make 1 int_t) in
    let fn_stack_push_fn () = fn_proto "stack_push_fn" void_t [| ptr_t; int_t |] in
    let fn_stack_push_nth_obj () = fn_proto "stack_push_nth_obj" void_t [| pointer_type obj_t; int_t |] in
    let fn_stack_push_add () = fn_proto "stack_push_add" void_t [||] in
    let fn_stack_push_sub () = fn_proto "stack_push_sub" void_t [||] in
    let fn_stack_push_mul () = fn_proto "stack_push_mul" void_t [||] in
    let fn_stack_push_div () = fn_proto "stack_push_div" void_t [||] in
    let fn_apply_full () = fn_proto "apply_full" void_t [||] in

    let call f args = ignore (build_call (f ()) args "" bld) in

    (* Do things *)
    let fns = ref Fns.empty in

    let rec idx item = function
        | [] -> 0
        | hd::tl -> if item = hd then 0 else 1 + (idx item tl)
    in

    let rec generate_function name code = 
        let code_fn = fn_t_proto name in
        let arg_param = param code_fn 0 in
        let block = append_block ctx "entry" code_fn in
        fns := Fns.add name block !fns;
        position_at_end block bld;

        let generate_expr = function
            | Instr.PushConst (Instr.Int i) -> call fn_stack_push_int [| const_int int_t i |]

            | Instr.PushArg s -> (
                let n = idx s code.Instr.args in
                call fn_stack_push_nth_obj [| arg_param; (const_int int_t n) |]
            )

            | Instr.PushFn (Instr.BinOp Instr.Add) -> call fn_stack_push_add [||]
            | Instr.PushFn (Instr.BinOp Instr.Sub) -> call fn_stack_push_sub [||]
            | Instr.PushFn (Instr.BinOp Instr.Mul) -> call fn_stack_push_mul [||]
            | Instr.PushFn (Instr.BinOp Instr.Div) -> call fn_stack_push_div [||]

            | Instr.PushFn (Instr.Named s) -> (
                let call_fn = fn_t_proto s in
                let argc = List.length ((Instr.Fns.find s instrs).Instr.args) in

                try
                    let fn_block = Fns.find s !fns in
                    call fn_stack_push_fn [| block_address call_fn fn_block; const_int int_t argc |]
                with
                    Not_found -> (
                        generate_function s (Instr.Fns.find s instrs);
                        position_at_end block bld;
                        let fn_block = Fns.find s !fns in
                        call fn_stack_push_fn [| const_bitcast call_fn ptr_t; const_int int_t argc |]
                    )
            )

            | Instr.Apply (Instr.Full) -> call fn_apply_full [||]
        in

        let stack = Stack.create () in
        Stack.iter (fun item -> Stack.push item stack) code.Instr.code;
        Stack.iter generate_expr stack;

        ignore (build_ret_void bld)
    in

    generate_function "sfl_entry" (Instr.Fns.find "" instrs);

    Errors.Ok (string_of_llmodule mdl);
