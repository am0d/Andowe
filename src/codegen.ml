open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "Andowe JIT"
let builder = builder context
let double_type = double_type context

let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let rec codegen_expr = function
    | Ast.Number n -> const_float double_type n
    | Ast.Binary (op, lhs, rhs) ->
            let lhs_val = codegen_expr lhs in
            let rhs_val = codegen_expr rhs in
            begin
                match op with
                | '+' -> build_add lhs_val rhs_val "addtmp" builder
                | '-' -> build_sub lhs_val rhs_val "subtmp" builder
                | '*' -> build_mul lhs_val rhs_val "multmp" builder
                | _ -> raise (Error "Unimplemented op")
            end
    | _ -> raise (Error "Unimplemented AST node")

let codegen_prototype = function
    | Ast.Prototype (name, args) ->
            let doubles = Array.make (Array.length args) double_type in
            let ft = function_type double_type doubles in
            let f =
                match lookup_function name the_module with
                | None -> declare_function name ft the_module
                | Some f ->
                        if Array.length (basic_blocks f) == 0 then () else
                            raise (Error "Can't redefine function with a body");
                        
                        if Array.length (params f) == Array.length args then ()
                        else raise (
                            Error "Redefinition of function with different # of args");
                        f
            in
            Array.iteri (fun i a ->
                let n = args.(i) in
                set_value_name n a;
                Hashtbl.add named_values n a
            ) (params f);
            f
