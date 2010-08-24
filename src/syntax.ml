type expr = 
    | Int of int
    | Float of float
    | String of string
    | Ident of string
    | Plus of expr * expr
