{
    let string_of_char c =
        String.make 1 c
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '?']*

rule lex = parse
(* Numbers *)
| digit+ as itext {
    let inum = int_of_string itext in
    print_endline ("Int: " ^ (string_of_int inum));
    lex lexbuf
}
| digit* '.' digit+ as ftext {
    let fnum = float_of_string ftext in
    print_endline ("Float: " ^ (string_of_float fnum));
    lex lexbuf
}
(* Strings *)
| '\"' [^ '\"']* '\"' as stext {
    print_endline ("String: " ^ stext);
    lex lexbuf
}
(* Operators *)
| '+' {print_endline "+"; lex lexbuf}
| '-' {print_endline "-"; lex lexbuf}
| '*' {print_endline "*"; lex lexbuf}
| '/' {print_endline "/"; lex lexbuf}
(* Keywords *)
| "def" {print_endline "Def"; lex lexbuf}
(* Identifiers *)
| ident as text {
    print_endline ("Ident: " ^ text);
    lex lexbuf
}
(* Whitespace *)
| [' ' '\t' '\n'] {
    lex lexbuf
}
| _ as c {
    print_endline ("Char: " ^ (string_of_char c));
    lex lexbuf
}
| eof {
}
