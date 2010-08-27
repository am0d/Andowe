open Printf
open Token

let rec lex = parser
    | [< ' (' '|'\t'); stream >] -> (
        (* Ignore whitespace in the middle of the line *)
        lex stream
    )
    | [< ' ('\n'); stream=lex >] -> (
        (* Newlines are a special token *)
        [< 'NEWLINE; stream >]
    )
    | [< ' ('0'..'9') as c; stream >] -> (
        (* Recognise numbers - both integer and float *)
        let buf = Buffer.create 1 in
        Buffer.add_char buf c;
        lex_int buf stream
    )
    | [< ' ('+' | '-' | '*' | '/') as c; stream=lex >] -> (
        (* Recognise standard operators *)
        match c with
        | '+' -> [< 'PLUS; stream >]
        | '-' -> [< 'MINUS; stream >]
        | _ -> [< stream >]
    )
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
            [< 'INT(num); stream >]

and lex_float buf = parser
    | [< ' ('0'..'9') as c; stream >] ->
            Buffer.add_char buf c;
            lex_float buf stream
    | [< stream=lex >] ->
            let num = float_of_string (Buffer.contents buf) in
            [< 'FLOAT(num); stream >]
;;

let debug = Stream.iter (function
    | PLUS -> printf "+"
    | MINUS -> printf "-"
    | INT i -> printf "%d" i
    | FLOAT f -> printf "%f" f
    | IDENT i -> printf " %s " i
    | STRING s -> printf "\"%s\"" s
    | NEWLINE -> printf "\n"
);;
