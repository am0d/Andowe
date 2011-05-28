open Printf
open Token

let rec lex = parser
    (* Ignore whitespace in the middle of the line *)
    | [< ' (' '|'\t'|'\r'|'\n'); stream >] -> 
            lex stream
            (* Recognise numbers - both integer and float *)
    | [< ' ('0'..'9') as c; stream >] -> begin
        let buf = Buffer.create 1 in
        Buffer.add_char buf c;
        lex_int buf stream
    end
    | [< ' ('A'..'Z'|'a'..'z') as c; stream >] -> begin
        let buf = Buffer.create 1 in
        Buffer.add_char buf c;
        lex_ident buf stream
    end
    (* Recognise standard operators *)
    | [< 'c; stream >] -> [< 'Kwd c; lex stream >]
    | [< >] -> [< >]

and lex_int buf = parser
    | [< ' ('0'..'9') as c; stream >] ->
            Buffer.add_char buf c;
            lex_int buf stream
    | [< ' ('.') as c; stream >] ->
            Buffer.add_char buf c;
            lex_float buf stream
    | [< stream=lex >] ->
            let num = int_of_string (Buffer.contents buf) in
            [< 'Int(num); stream >]

and lex_float buf = parser
    | [< ' ('0'..'9') as c; stream >] ->
            Buffer.add_char buf c;
            lex_float buf stream
    | [< stream=lex >] ->
            let num = float_of_string (Buffer.contents buf) in
            [< 'Float(num); stream >]

and lex_ident buf = parser
    | [< ' ('A'..'Z'|'a'..'z'|'0'..'9') as c; stream >] ->
            Buffer.add_char buf c;
            lex_ident buf stream
    | [< stream=lex >] ->
            match Buffer.contents buf with
            | "def" -> [< 'Def; stream >]
            | "extern" -> [< 'Extern; stream >]
            | ident -> [< 'Ident(ident); stream >]
;;

let debug = Stream.iter (function
    | Kwd c -> printf "%c" c
    | Int i -> printf "%d" i
    | Float f -> printf "%f" f
    | Ident s -> printf "%s" s
    | Def -> print_string "Def"
    | Extern -> print_string "Extern"
                        );;
