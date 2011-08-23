type token = 
    | Int of int
    | Float of float
    | Kwd of char
    | Ident of string
    | Def | Extern
    | If |Then | Else
    | Let | In
    | Begin | End
