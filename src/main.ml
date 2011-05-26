let parse_error s =
    print_string ("Error: ");
    print_endline s

let main () =
    Parser.set_binop_precedence ();
    print_endline "Andowe 0.0.0";
    try
        while true do
            try
                print_string "> ";
                let input = read_line () in
                let lexstream = Stream.of_string input in
                (*Lexer.debug (Lexer.lex lexstream);*)
                ignore(Parser.parse_primary (Lexer.lex lexstream));
                print_endline "Program is syntactically correct"
            with Message.Error s ->
                parse_error s
            | Stream.Error s ->
                parse_error s
        done
    with End_of_file ->
        print_endline ""

let _ = Printexc.print main ()
