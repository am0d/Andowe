open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

let parse_error ?(stream = None) s =
    print_string ("Error: ");
    print_endline s;
    match stream with
    | Some s -> Lexer.debug(s)
    | _ -> ()

(* Global variables, program state *)
(* Dump the LLVM value of each parse? *)
let argDumpValue = ref false

let rec main_loop interactive fpm execution_engine stream =
    match Stream.peek stream with
    | None -> ()
    | Some Token.Kwd ';'
    | Some Token.Kwd '\n' ->
            Stream.junk stream;
            main_loop interactive fpm execution_engine stream
    | Some token -> begin
        try match token with
            | Token.Def ->
                let expr = Parser.parse_definition stream in
(*                 print_endline "Parsed a function definition"; *)
                let ret = Codegen.codegen_function fpm expr in
                if !argDumpValue then dump_value ret
                else ();
            | Token.Extern ->
                let expr = Parser.parse_extern stream in
(*                 print_endline "Parsed an extern declaration"; *)
                let ret = Codegen.codegen_prototype expr in
                if !argDumpValue then dump_value ret
                else ();
            | _ ->
                let expr = Parser.parse_toplevel stream in
(*                 print_endline "Parsed a toplevel expression"; *)
                let func = Codegen.codegen_function fpm expr in
                if !argDumpValue then dump_value func;
                let result = ExecutionEngine.run_function func [||] execution_engine in
                print_float (GenericValue.as_float Codegen.double_type result);
                print_newline ();
        with Message.Error s | Stream.Error s | Codegen.Error s->
            parse_error ~stream:(Some stream) s;
            Stream.junk stream;
    end;
    if interactive then (print_string "> "; flush stdout);
    main_loop interactive fpm execution_engine stream

let shell fpm execution_engine =
    print_string "> "; flush stdout;
    let lexstream = Lexer.lex (Stream.of_channel stdin) in
    main_loop true fpm execution_engine lexstream

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

    shell fpm execution_engine;
    dump_module Codegen.the_module

let _ = Printexc.print main ()
