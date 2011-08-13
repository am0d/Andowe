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
                | _ -> raise (Error ("Unimplemented op " ^ (Char.escaped op)))
            end
    | Ast.Call (name, args) ->
            let callee = 
                match lookup_function name the_module with
                | Some callee -> callee
                | None -> raise (Error ("Unknown function " ^ name ^ " referenced"))
            in
            let params = params callee in
            (* Check that the parameters match up *)
            if Array.length args == Array.length params then ()
            else raise (Error "Incorrect # of params passed");
            let args = Array.map codegen_expr args in
            build_call callee args "calltmp" builder

    | Ast.Variable name -> (
            let v = 
            (try Hashtbl.find named_values name with 
                | Not_found -> raise (Error "Unknown variable referenced")) in
            match classify_type (type_of v) with
                | TypeKind.Pointer -> build_load v "ret" builder
                | _ -> v
            )

    | Ast.If (condition, true_block, false_block) ->
            let cond = codegen_expr condition in

            (* Compare condition to 0 to get a boolean value *)
            let zero = const_float double_type 0.0 in
            let cond_val = build_fcmp Fcmp.One cond zero "ifcond" builder in
            let start_bb = insertion_block builder in
            let my_function = block_parent start_bb in
            let then_bb = append_block context "then" my_function in
            position_at_end then_bb builder;
            let then_val = codegen_expr true_block in
            let new_then_bb = insertion_block builder in
            let else_bb = append_block context "else" my_function in
            position_at_end else_bb builder;
            let else_val = codegen_expr false_block in
            let new_else_bb = insertion_block builder in
            let merge_bb = append_block context "ifcont" my_function in
            position_at_end merge_bb builder;
            let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
            let phi = build_phi incoming "iftmp" builder in
            position_at_end start_bb builder;
            ignore (build_cond_br cond_val then_bb else_bb builder);
            position_at_end new_then_bb builder; 
            ignore (build_br merge_bb builder);
            position_at_end new_else_bb builder;
            ignore (build_br merge_bb builder);
            position_at_end merge_bb builder;
            phi
    | Ast.Assignment (variable, expression) ->
            let name = match variable with
                | n -> (
                    try
                        ignore (Hashtbl.find named_values n);
                        raise (Error ("Can't redefine variable " ^ n))
                    with
                        | Not_found -> n)
            in
            let e = codegen_expr expression in
            Hashtbl.add named_values name e;
            e

    | Ast.Sequence (e1, e2) ->
            ignore(codegen_expr e1);
            codegen_expr e2

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

let codegen_function fpm = function
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

                (* Optimise the function *)
                let _ = PassManager.run_function the_function fpm in

                the_function
            with e ->
                dump_value the_function;
                if alreadyDefined then
                    delete_block bb
                else
                    delete_function the_function;
                raise e
