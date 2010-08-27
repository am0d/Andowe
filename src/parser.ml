open Syntax

let rec toplevel = parser
    | [< e=expr; stream >] -> (
        let rec multiline = parser 
            | [< 'Token.NEWLINE; t=toplevel >] -> (
                print_endline "toplevel -> expr NEWLINE toplevel";
                e :: t
            )
            | [< >] -> (
                print_endline "toplevel -> expr";
                [e]
            )
        in multiline stream
    )
    | [< >] -> []

and expr = parser
    | [< v=value; stream >] -> (
        let rec arith = parser
            | [< 'Token.PLUS; e=expr >] -> (
                print_endline "arith -> expr PLUS expr";
                Arith (v, Plus, e)
            )
            | [< 'Token.MINUS; e=expr >] -> (
                print_endline "arith -> expr MINUS expr";
                Arith (v, Minus, e)
            )
            | [< >] -> (
                print_endline "expr -> value";
                v
            )
        in arith stream
    )

and value = parser
    | [< 'Token.INT i >] -> (
        print_endline "value -> INT";
        Int (i)
    )
    | [< 'Token.FLOAT f >] -> (
        print_endline "value -> FLOAT";
        Float (f)
    )
    | [< 'Token.STRING s >] -> (
        print_endline "value -> STRING";
        String (s)
    )
    | [< 'Token.IDENT i >] -> (
        print_endline "value -> IDENT";
        Ident (i)
    )
