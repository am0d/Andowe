{
    open Parser
    let incr_linenum lexbuf = 
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
        }

    let s = (Stack.create():int Stack.t)
    let rec check x =
        (
            if Stack.is_empty s then
                Stack.push x s
            else if Stack.top s < x then
                (
                    Stack.push x s;
                    print_endline "INDENT"
                )
            else if Stack.top s > x then
                (
                    print_endline "DEDENT";
                    ignore (Stack.pop s);
                    check x
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
| "end" { END }
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
    lex lexbuf
}
| _ {
    lex lexbuf
}
| eof { raise End_of_file }

and newline = parse
| [' ']* as spaces {
    check (String.length spaces);
    lex lexbuf
}
