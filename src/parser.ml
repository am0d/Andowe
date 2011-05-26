open Syntax

(* Holds the precedence for each operator we use *)
let binop_precedence:(char, int) Hashtbl.t = Hashtbl.create 10

let set_binop_precedence () = begin
    (* Install standard binary operators.
     * 1 is the lowest precedence.
     *)
    Hashtbl.add binop_precedence '<' 10;
    Hashtbl.add binop_precedence '+' 20;
    Hashtbl.add binop_precedence '-' 20;
    Hashtbl.add binop_precedence '*' 40;
    ()
end

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
