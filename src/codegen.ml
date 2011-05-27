open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "Andowe JIT"
let builder = builder context
let double_type = double_type context

let rec codegen_expr = function
    | Ast.Number n -> const_float double_type n
    | Ast.Binary (op, lhs, rhs) ->
            let lhs_val = codegen_expr lhs in
            let rhs_val = codegen_expr rhs in
            begin
                match op with
                | '+' -> build_add lhs_val rhs_val "addtmp" builder
                | _ -> raise (Error "Unimplemented op")
            end
    | _ -> raise (Error "Unimplemented AST node")
