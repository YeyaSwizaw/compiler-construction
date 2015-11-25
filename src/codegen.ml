(* Compiler Construction - codegen.ml *)
(* Samuel Sleight *)

open Llvm

module Values = Map.Make(struct type t = int let compare = compare end)

let generate_code opt_flags size_flags fns = 
    let ctx = global_context () in
    let mdl = create_module ctx "sfl_mdl" in
    let bld = builder ctx in

    (* Typedefs *)
    let void_t = void_type ctx in
    let int_t = integer_type ctx 64 in

    let main_t = function_type void_t [||] in

    let get_fn fn_t name = declare_function name fn_t mdl in
    
    let get_fn_putchar () = get_fn (function_type void_t [|int_t|]) "putchar" in
    let get_fn_getchar () = get_fn (function_type int_t [||]) "getchar" in

    (* Codegen *)
    let generate_function name code = 
        let code_fn = get_fn main_t name in
        let block = append_block ctx "entry" code_fn in
        position_at_end block bld;

        let next_value = let next = ref 0 in (fun () -> next := !next + 1; !next - 1) in
        let stored_values = ref Values.empty in

        let rec generate_value = function
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
        in

        List.iter generate_instr code;
        ignore (build_ret_void bld)
    in


    Instr.Fns.iter (fun name instrs -> generate_function (if name = "" then "main" else name) instrs.Instr.code) fns;
    Errors.Ok (string_of_llmodule mdl)
