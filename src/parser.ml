open Syntax

let rec toplevel = parser
    | [< >] -> []

(* primary
 *      ::= numberexpr
 *)
and parse_primary = parser
    (* numberexpr
     *      ::= intexpr
     *      ::= floatexpr
     *)
    | [< 'Token.Int i >] -> Number (float_of_int i)
    | [< 'Token.Float f>] -> Number f
    | [< >] -> raise (Message.Error "Number expected")

and parse_expr = parser
    | [< lhs=parse_primary; stream >] -> lhs
