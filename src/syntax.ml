type arith = 
    | Plus
    | Minus
    | Times 
    | Divide

and expr = 
    | Int of int
    | Float of float
    | String of string
    | Ident of string
    | Arith of expr * arith * expr
