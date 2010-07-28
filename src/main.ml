let rec eval str =
    try
        Stream.next str;
        eval str
    with Stream.Failure ->
        ()

let main () =
    print_endline "Andowe 0.0.0";
    let in_channel = Stream.of_channel stdin in
    try
        while true do
            print_string "> "; flush stdout;
            eval (Lexer.lex in_channel)
        done
    with End_of_file ->
        print_endline ""

let _ = Printexc.print main ()
