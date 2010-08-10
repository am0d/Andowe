{
    open Parser
    exception Lexing_error of string;;
    let incr_linenum lexbuf = 
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
        }

    let s = (Stack.create():int Stack.t);;
    ignore(Stack.push 0 s);;
    let rec check x =
        (
            if Stack.is_empty s then
                (
                    print_endline "Empty"; flush stdout;
                    Stack.push x s;
                    Some (BEGIN)
                )
            else if Stack.top s < x then
                (
                    print_endline "BEGIN";
                    Stack.push x s;
                    Some (BEGIN)
                )
            else if Stack.top s > x then
                (
                    print_endline "END";
                    ignore (Stack.pop s);
                    match check x with
                    | Some (END i) -> Some (END (i+1))
                    | None -> Some (END 1)
                    | _ -> raise (Lexing_error "Indentation failure")
                )
            else 
                (
                    print_endline "NONE";
                    None
                )
        )
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '?']*

rule lex = parse
(* Numbers *)
| digit+ as itext {
    let inum = int_of_string itext in
    INT inum
}
| digit* '.' digit+ as ftext {
    let fnum = float_of_string ftext in
    FLOAT fnum
}
(* Strings *)
| '\"' [^ '\"']* '\"' as stext {
    STRING stext
}
(* Operators *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| "<>" { NEQUAL }
| '<' { LTHAN }
| '>' { GTHAN }
| '=' { EQUALS }
| ',' { COMMA }
| ':' { COLON }
(* Keywords *)
| "def" { DEF }
| "begin" { BEGIN }
| "end" { END (1) }
| "if" { IF }
| "else" { ELSE }
(* Identifiers *)
| ident as text {
    IDENT text
}
(* Whitespace *)
| [' ' '\t'] {
    lex lexbuf
}
| '\n' {
    incr_linenum lexbuf;
    newline lexbuf
}
| _ {
    lex lexbuf
}
| eof { raise End_of_file }

and newline = parse
| [' ']* as spaces {
    match check (String.length spaces) with
    | Some t -> t
    | None -> NEWLINE
}

{
(* Implement the cache so that we can return multiple END tokens *)
let cached_tokens = ref None;;
let lex_cache lexbuf =
    match !cached_tokens with
    | Some (END i) when i > 1 -> (
        cached_tokens := Some (END (i-1));
        END (1)
    )
    | Some (END 1) -> (
        cached_tokens := None;
        END (1)
    )
    | Some token -> (
        token
    )
    | None -> (
        match lex lexbuf with
        | END i when i > 1 -> (
            cached_tokens := Some (END (i - 1));
            END (1)
        )
        | token -> token
    )
}
