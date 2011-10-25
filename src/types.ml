open Ast

type ty = 
    | TInt
    | TBool
    | TArrow of ty * ty
    | TError

let rec string_of_type ty =
    match ty with
    | TInt -> "int"
    | TBool -> "bool"
    | TArrow (ty1, ty2) -> 
            (string_of_type ty1) ^ " -> " ^ (string_of_type ty2)
    | TError -> "error"

let type_error s =
    raise (Message.Error s)

let rec check ctx ty e =
    let ty1 = type_of ctx e in
    if ty1 <> ty then
        type_error ((string_of_expr e) ^
        " has type " ^ (string_of_type ty1) ^
        " but is used as " ^
        (string_of_type ty))

and type_of ctx e =
    match e with
    | Number _ -> TInt
    | Variable _ -> TInt
    | Binary (op, e1, e2) -> begin
            check ctx TInt e1;
            check ctx TInt e2;
            match op with
            | '>'
            | '<'
            | '=' ->
                    TBool
            | '+'
            | '-'
            | '*' ->
                    TInt
            | _ -> TError
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
    | _ -> TError

and type_check ctx e =
    match e with
    | Function (p, e) ->
            type_of ctx e
