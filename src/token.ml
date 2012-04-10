type lexing_information = {
    line_no: int;
    char_no: int;
    token: token;
}

and token = 
    (* Types *)
    | Int of int
    | Float of float
    | Kwd of char
    | Ident of string
    | Bool of bool
    (* Keywords *)
    | Def | Extern | As
    | If |Then | Else
    | Let | In
    | Begin | End
    (* Types *)
    | LBool
    | LInt

let token_of t = t.token

let string_of_token = function
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Kwd c -> Char.escaped c
    | Ident name -> name
    | Bool b -> string_of_bool b
    | Def -> "def"
    | Extern -> "extern"
    | _ -> "n/a"
