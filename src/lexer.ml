open Token

let rec lex ?(block = false) = parser
    | [< ' (' '); stream >] -> lex stream
    | [< ' ('\n') >] -> [< >]
    | [< ' ('%'); stream >] -> lex_comment stream
    | [< 'c; stream >] -> [< 'Token.Op c; lex stream >]
    | [< >] -> raise End_of_file

and lex_comment = parser
    | [< ' ('\n'); stream = lex >] -> stream
    | [< 'c; stream = lex_comment >] -> stream
    | [< >] -> [< >]
