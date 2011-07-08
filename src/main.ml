open Llvm

let parse_error s =
    print_string ("Error: ");
    print_endline s

let rec main_loop stream =
    match Stream.peek stream with
    | None -> ()
    | Some Token.Kwd ';' ->
            Stream.junk stream;
            main_loop stream
    | Some token -> begin
        try match token with
            | Token.Def ->
                let e = Parser.parse_definition stream in
                print_endline "Parsed a function definition";
                dump_value (Codegen.codegen_function e);
            | Token.Extern ->
                let e = Parser.parse_extern stream in
                print_endline "Parsed an extern declaration";
                dump_value (Codegen.codegen_prototype e);
            | _ ->
                let e = Parser.parse_toplevel stream in
                print_endline "Parsed a toplevel expression";
                dump_value (Codegen.codegen_function e);
        with Message.Error s | Stream.Error s | Codegen.Error s->
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
    dump_module Codegen.the_module

let _ = Printexc.print main ()
