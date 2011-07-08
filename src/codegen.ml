open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "Andowe JIT"
let builder = builder context
let double_type = double_type context

let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let name_of = function
    | Ast.Prototype (name, _) ->
            name
    | _ -> ""

let rec codegen_expr = function
    | Ast.Number n -> const_float double_type n
    | Ast.Binary (op, lhs, rhs) ->
            let lhs_val = codegen_expr lhs in
            let rhs_val = codegen_expr rhs in
            begin
                match op with
                | '+' -> build_fadd lhs_val rhs_val "addtmp" builder
                | '-' -> build_fsub lhs_val rhs_val "subtmp" builder
                | '*' -> build_fmul lhs_val rhs_val "multmp" builder
                | '<' ->
                        let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
                        build_uitofp i double_type "booltmp" builder
                | _ -> raise (Error "Unimplemented op")
            end
    | Ast.Call (name, args) ->
            let callee = 
                match lookup_function name the_module with
                | Some callee -> callee
                | None -> raise (Error "Unknown function referenced")
            in
            let params = params callee in
            (* Check that the parameters match up *)
            if Array.length args == Array.length params then ()
            else raise (Error "Incorrect # of params passed");
            let args = Array.map codegen_expr args in
            build_call callee args "calltmp" builder

    | Ast.Variable name ->
            (try Hashtbl.find named_values name with 
                | Not_found -> raise (Error "Unknown variable referenced"))
    | _ -> raise (Error "Unimplemented AST node")

let codegen_prototype = function
    | Ast.Prototype (name, args) ->
            let doubles = Array.make (Array.length args) double_type in
            let ft = function_type double_type doubles in
            let f =
                match lookup_function name the_module with
                | None -> declare_function name ft the_module
                | Some f ->
                        if Array.length (basic_blocks f) == 0 then () 
                        else raise (Error "Can't redefine function with a body");
                        
                        if Array.length (params f) == Array.length args then ()
                        else raise (Error "Redefinition of function with different # of args");
                        f
            in
            Array.iteri (fun i a ->
                let n = args.(i) in
                set_value_name n a;
                Hashtbl.add named_values n a;
            ) (params f);
            f

let codegen_function = function
    | Ast.Function (proto, body) ->
            Hashtbl.clear named_values;

            (* Check if the function has already been defined so we don't delete
             * the prototype later if there was an error
             *)
            let alreadyDefined = 
                match lookup_function (name_of proto) the_module with
                | None -> false
                | _ -> true 
                in
            let the_function = codegen_prototype proto in

            (* Create a new basic block to start insertion *)
            let bb = append_block context "entry" the_function in
            position_at_end bb builder;

            try
                let ret_val = codegen_expr body in

                (* Finish off the function *)
                let _ = build_ret ret_val builder in

                (* Validate the generated code, check for consistency *)
                Llvm_analysis.assert_valid_function the_function;

                the_function
            with e ->
                if alreadyDefined then
                    delete_block bb
                else
                    delete_function the_function;
                raise e
