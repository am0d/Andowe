
let rec lex ?(block = false) = parser
    (* Whitespace *)
    | [< ' (' '); stream >] -> 
            lex stream
    (* Newlines *)
    | [< ' ('\n') >] -> 
            [< >]
    (* Comments *)
    | [< ' ('%'); stream >] -> 
            lex_comment stream
    (* Identifiers *)
    | [< ' ('a'..'z'|'A'..'Z'|'_') as c; stream >] ->
            let buffer = Buffer.create 1 in
            Buffer.add_char buffer c;
            lex_ident buffer stream
    (* Anything else *)
    | [< 'c; stream >] -> 
            [< 'Token.Op c; lex stream >]
    (* End of the stream *)
    | [< >] -> 
            raise End_of_file

and lex_ident buffer = parser
    | [< ' ('a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_') as c; stream >] ->
            Buffer.add_char buffer c;
            lex_ident buffer stream
    | [< stream = lex >] -> 
            match Buffer.contents buffer with
            | "def" -> [< 'Token.Def; stream >]
            | id -> [< 'Token.Ident id; stream >]

and lex_comment = parser
    | [< ' ('\n'); stream = lex >] -> 
            stream
    | [< 'c; stream = lex_comment >] -> 
            stream
    | [< >] -> 
            [< >]
