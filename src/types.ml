open Ast

let rec string_of_type ty =
    match ty with
    | TInt -> "int"
    | TBool -> "bool"
    | TArrow (ty1, ty2) -> 
            (string_of_type ty1) ^ " -> " ^ (string_of_type ty2)
    | TUnknown -> "unknown"
    | TError s -> "error (" ^ s ^ ")"

let type_error s =
    raise (Message.TypeError s)

let rec check ctx ty e =
    let ty1 = type_of ctx e in
    if ty1 <> ty then
        type_error ((string_of_expr e) ^
        " has type " ^ (string_of_type ty1) ^
        " but is used as " ^
        (string_of_type ty))

and type_of ctx e =
    match e with
    | Boolean _ -> TBool
    | Number _ -> TInt
    | Variable(v, _) -> begin
        try Hashtbl.find ctx v
        with Not_found -> type_error (v ^ " is not defined")
    end
    | Binary (op, e1, e2) -> begin
            match op with
            | '>'
            | '<'
            | '=' ->
                    let ty1 = type_of ctx e1 in
                    check ctx ty1 e2;
                    TBool
            | '+'
            | '-'
            | '*' ->
                    check ctx TInt e1;
                    check ctx TInt e2;
                    TInt
            | _ -> TError(Char.escaped op)
    end
    | If (cond, e1, e2) ->
            check ctx TBool cond;
            let ty1 = type_of ctx e1 in
            check ctx ty1 e2;
            ty1
    | Sequence (e1, e2) ->
            ignore (type_of ctx e1);
            type_of ctx e2
    | Call (n, e) ->
            Array.iter (check ctx TInt) e;
            TInt
    | _ -> TError(string_of_expr e)

and build_ctx function_prototype =
    (* Build up a local context based on the names / types of the parameters for the function *)
    let local_context:(string, Ast.ty) Hashtbl.t = Hashtbl.create 2 in

    match function_prototype with 
    | Prototype (function_name, function_parameters) -> begin
            let add_to_context parameter =
                match parameter with
                | Parameter (parameter_name, parameter_type) ->
                        Hashtbl.add local_context parameter_name parameter_type
            in

            Array.iter add_to_context function_parameters;
            local_context
        end

and type_check ctx e =
    match e with
    | Function (p, e) ->
            try
                let local_context = build_ctx p in
                type_of local_context e
            with Message.TypeError te ->
                match p with
                | Prototype (name, args) ->
                        raise (Message.TypeError ("In function " ^ name ^ ":\n\t" ^ te))
