open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "Andowe JIT"
let double_type = double_type context

let rec codegen_expr = function
    | Ast.Number n -> const_float double_type n
    | _ -> raise (Error "Unimplemented AST node")
