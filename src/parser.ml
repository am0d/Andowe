open Syntax

let rec toplevel = parser
    | [< >] -> []

(* primary
 *      ::= numberexpr
 *      ::= parenexpr
 *)
and parse_primary = parser
    (* numberexpr
     *      ::= intexpr
     *      ::= floatexpr
     *)
    | [< 'Token.Int i >] -> Number (float_of_int i)
    | [< 'Token.Float f>] -> Number f
    (* parenexpr
     *      ::= '(' expression ')'
     *)
    | [< 'Token.Kwd '('; e=parse_primary; 'Token.Kwd ')' ?? "expected ')'" >] ->
            e
    | [< >] -> raise (Message.Error "Number expected")

and parse_expr = parser
    | [< lhs=parse_primary; stream >] -> lhs
