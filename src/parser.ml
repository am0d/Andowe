open Syntax

let rec toplevel = parser
    | [< >] -> []

and parse_primary = parser
    | [< 'Token.Int i >] -> Number (float_of_int i)
    | [< >] -> raise (Message.Error "Number expected")

and parse_expr = parser
    | [< lhs=parse_primary; stream >] -> lhs
