open Llvm

let parse_error s =
    print_string ("Error: ");
    print_endline s

let rec main_loop stream =
    match Stream.peek stream with
    | None -> ()
    | Some Token.Kwd '.' ->
            Stream.junk stream;
            print_string "> "; flush stdout;
            main_loop stream
    | Some token -> begin
        try match token with
            | _ ->
                let e = Parser.parse_expression stream in
                print_endline "Parsed a toplevel expression";
                dump_value (Codegen.codegen_expr e);
            with Message.Error s ->
                Stream.junk stream;
                parse_error s;
            | Stream.Error s ->
                Stream.junk stream;
                parse_error s;
    end;
    print_string "> "; flush stdout;
    main_loop stream

let main () =
    Parser.set_binop_precedence ();
    print_endline "Andowe 0.0.0";
    print_string "> "; flush stdout;
    let lexstream = Lexer.lex (Stream.of_channel stdin) in
    main_loop lexstream;
    print_endline ""

let _ = Printexc.print main ()
