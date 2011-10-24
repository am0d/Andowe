type token = 
    (* Types *)
    | Int of int
    | Float of float
    | Kwd of char
    | Ident of string
    (* Keywords *)
    | Def | Extern
    | If |Then | Else
    | Let | In
    | Begin | End
