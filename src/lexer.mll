{
    open Parser
    exception Lexing_error of string;;

    (* Set the line number correctly *)
    let incr_linenum lexbuf = 
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
        }

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
| "fun" { FUN }
| "return" { RETURN }
| "\nbegin\n" { BEGIN }
| "\nend\n" { END (1) }
| "if" { IF }
| "else" { ELSE }
(* Identifiers *)
| ident as text {
    IDENT text
}
(* Whitespace *)
| [' ' '\t']+ '\n' {
    (* Ignore blank lines *)
    incr_linenum lexbuf;
    lex lexbuf
}
| [' ' '\t'] {
    lex lexbuf
}
(* Newlines - increment the line number and check the indent *)
| ['\n']+ {
    incr_linenum lexbuf;
    (*newline lexbuf*)
    NEWLINE
}
(* Anything else *)
| _ {
    lex lexbuf
}
| eof { EOF }

(* Check the indent at the start of a new line *)
and newline = parse
| [' ']* {
    NEWLINE
}

{
(* Implement the cache so that we can return multiple END tokens *)
let cached_tokens = ref None;;
let lex_cache lexbuf =
    match !cached_tokens with
    | Some (END i) when i > 1 -> (
        (* handle multiple de-dents *)
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
