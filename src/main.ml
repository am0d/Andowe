let main () =
    print_endline "Andowe 0.0.0";
    let in_channel = Lexing.from_channel stdin in
    try
        print_string "> "; 
        flush stdout;
        ignore(Lexer.lex in_channel)
    with End_of_file ->
        print_endline ""

let _ = Printexc.print main ()
