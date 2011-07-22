open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

let parse_error s =
    print_string ("Error: ");
    print_endline s

let rec main_loop fpm execution_engine stream =
    match Stream.peek stream with
    | None -> ()
    | Some Token.Kwd ';' ->
            Stream.junk stream;
            main_loop fpm execution_engine stream
    | Some token -> begin
        try match token with
            | Token.Def ->
                let expr = Parser.parse_definition stream in
(*                 print_endline "Parsed a function definition"; *)
                dump_value (Codegen.codegen_function fpm expr);
            | Token.Extern ->
                let expr = Parser.parse_extern stream in
(*                 print_endline "Parsed an extern declaration"; *)
                dump_value (Codegen.codegen_prototype expr);
            | _ ->
                let expr = Parser.parse_toplevel stream in
(*                 print_endline "Parsed a toplevel expression"; *)
                let func = Codegen.codegen_function fpm expr in
                dump_value func;
                let result = ExecutionEngine.run_function func [||] execution_engine in
                print_float (GenericValue.as_float Codegen.double_type result);
                print_newline ();
        with Message.Error s | Stream.Error s | Codegen.Error s->
            Stream.junk stream;
            parse_error s;
    end;
    print_string "> "; flush stdout;
    main_loop fpm execution_engine stream

let main () =
    Parser.set_binop_precedence ();
    print_endline "Andowe 0.0.0";

    (* Setup the execution engine *)
    let execution_engine = ExecutionEngine.create Codegen.the_module in
    
    (* Setup the pass manager *)
    let fpm = PassManager.create_function Codegen.the_module in

    (* Setup the optimizer *)
    TargetData.add (ExecutionEngine.target_data execution_engine) fpm;

    add_instruction_combination fpm;
    add_reassociation fpm;
    add_gvn fpm;
    add_cfg_simplification fpm;

    ignore (PassManager.initialize fpm);

    print_string "> "; flush stdout;
    let lexstream = Lexer.lex (Stream.of_channel stdin) in
    main_loop fpm execution_engine lexstream;
    dump_module Codegen.the_module

let _ = Printexc.print main ()
