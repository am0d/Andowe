open Syntax

let rec toplevel = parser
    | [< >] -> []

and parse_primary = parser
    | [< 'Token.INT i >] -> (
        Int i
    )
    | [< >] -> (
        raise (Message.Error "Number expected")
    )

and parse_expr = parser
    | [< lhs=parse_primary; stream >] -> (
        lhs
    )
