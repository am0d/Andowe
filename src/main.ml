let parse_error s lexbuf =
    print_endline ("Error");
    print_endline s

let main () =
    print_endline "Andowe 0.0.0";
    let lexstream = Stream.of_channel stdin in
    try
        (*Lexer.debug (Lexer.lex lexstream);*)
        ignore(Parser.toplevel (Lexer.lex lexstream));
        print_endline "Program is syntactically correct"
    with Message.Error s ->
        parse_error s lexstream
        | End_of_file ->
                print_endline ""

let _ = Printexc.print main ()
