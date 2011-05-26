open Printf
open Token

let rec lex = parser
    (* Ignore whitespace in the middle of the line *)
    | [< ' (' '|'\t'|'\n'); stream >] -> 
            lex stream
            (* Recognise numbers - both integer and float *)
    | [< ' ('0'..'9') as c; stream >] -> begin
        let buf = Buffer.create 1 in
        Buffer.add_char buf c;
        lex_int buf stream
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
;;

let debug = Stream.iter (function
    | Kwd c -> printf "%c" c
    | Int i -> printf "%d" i
    | Float f -> printf "%f" f
                        );;
