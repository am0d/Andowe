type ty = 
    | TInt
    | TBool
    | TArrow of ty * ty
    | TUnknown
    | TError of string

type expr =
    | Boolean of bool
    | Number of int
    | Variable of string * ty
    | Binary of char * expr * expr
    | Call of string * expr array
    | If of expr * expr * expr
    | Assignment of string * expr
    | Sequence of expr * expr

type prototype = Prototype of string * string array

type func = Function of prototype * expr

let rec string_of_expr e =
    match e with
    | Boolean b -> string_of_bool b
    | Number i -> string_of_int i
    | Variable (v, _) -> v
    | Binary (c, e1, e2) ->
            (string_of_expr e1) ^ (Char.escaped c) ^ (string_of_expr e2)
    | Call (n, e1) ->
            n (*^ (string_of_expr e1)*)
    | If (cond, e1, e2) ->
            "if " ^ (string_of_expr cond)
    | Assignment (n, e) ->
            "let " ^ n ^ " = " ^ (string_of_expr e)
    | Sequence (e1, e2) ->
            (string_of_expr e1) ^ "; " ^ (string_of_expr e2)
