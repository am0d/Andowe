open Ast

(* Holds the precedence for each operator we use *)
let binop_precedence:(char, int) Hashtbl.t = Hashtbl.create 10

(* Find the precedence of op *)
let precedence op = try Hashtbl.find binop_precedence op with Not_found -> -1

let set_binop_precedence () = begin
    (* Install standard binary operators.
     * 1 is the lowest precedence.
     *)
    Hashtbl.add binop_precedence '<' 10;
    Hashtbl.add binop_precedence '+' 20;
    Hashtbl.add binop_precedence '-' 20;
    Hashtbl.add binop_precedence '*' 40;
    ()
end

let rec toplevel = parser
    | [< >] -> []

(* primary
 *      ::= numberexpr
 *      ::= parenexpr
 *)
and parse_primary = parser
    (* numberexpr
     *      ::= intexpr
     *      ::= floatexpr
     *)
    | [< 'Token.Int i >] -> Number (float_of_int i)
    | [< 'Token.Float f>] -> Number f
    
    (* parenexpr
     *      ::= '(' expression ')'
     *)
    | [< 'Token.Kwd '('; e=parse_expression; 'Token.Kwd ')' ?? "expected ')'" >] ->
            e

    (* identexpr
     *      ::= Ident
     *      ::= Ident '(' args ')
     *
     * args
     *      ::=
     *      ::= ',' expression
     *      ::= expression args
     *)
    | [< 'Token.Ident id; stream >] -> 
            let rec parse_args accumulator stream = 
                if Stream.peek stream = Some (Token.Kwd ')') then accumulator
                else
                begin parser
                    | [< e=parse_expression; stream >] ->
                            begin parser
                                | [< 'Token.Kwd ','; e=parse_args (e :: accumulator) >] -> e
                                | [< >] -> e :: accumulator
                            end stream
                    | [< >] -> accumulator
                end stream
            in
            let rec parse_ident id = parser
                | [< 'Token.Kwd '('; 
                      args=parse_args [];
                      'Token.Kwd ')' ?? "expected ')'" >] ->
                          Call (id, Array.of_list(List.rev args))
                | [< >] -> Variable id
            in
            parse_ident id stream

    (* ifexpr
     *      ::= 'if' expression 'then' expression elseexpr
     *
     * elseexpr
     *      ::=
     *      ::= 'else' expression
     *)
    | [< 'Token.If; condition=parse_expression;
         true_block=parse_block;
         'Token.Else ?? "expected 'else'"; false_block=parse_block; >] ->
             If (condition, true_block, false_block)
    | [< >] -> raise (Message.Error "Unknown token when expecting an expression")

(* block
 *      ::= 'begin' expression_list 'end'
 *
 * expression_list
 *      ::= expression
 *      ::= expression ';' expression_list
 *)
and parse_block = 
    let rec parse_exp_list = parser
        | [< e=parse_expression; stream >] ->
                if Stream.peek stream = Some (Token.Kwd ';') then
                    Stream.junk stream;
                if Stream.peek stream = Some Token.End then
                    e
                else if Stream.peek stream = Some Token.Else then
                    e
                else
                    Sequence(e, parse_exp_list stream)
    in
    parser            
    | [< 'Token.Kwd ':';
          e=parse_exp_list;
          stream >] ->
              if Stream.peek stream = Some Token.End then
                  e
              else if Stream.peek stream = Some Token.Else then
                  e
             else raise (Message.Error "expected 'end'")
    | [< >] -> raise (Message.Error "Blocks must begin with ':'")

(* expression
 *      ::= primary binoprhs
 *      ::= 'let' ident '=' expression 'in' expression
 *)
and parse_expression = parser
    | [< 'Token.Let; 'Token.Ident id;
            'Token.Kwd '='; value=parse_expression;
            'Token.In; val2 = parse_expression;
            >] ->
            Sequence (Assignment (id, value), val2)
    | [< lhs=parse_primary; stream >] -> parse_bin_rhs 0 lhs stream

(* binoprhs
 *      ::= ('+' primary)
 *)
and parse_bin_rhs expr_prec lhs stream =
    match Stream.peek stream with
    (* If this is a binop, find its precedence *)
    | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c ->
        let token_prec = precedence c in

        (* If the new operator's precedence is lower than what we currently
         * have, return *)
        if token_prec < expr_prec then
            lhs
        else begin
            (* Eat the binop *)
            Stream.junk stream;

            (* Parse the primary expression after the binary operator *)
            let rhs = parse_primary stream in

            (* Okay, we know this is a binop *)
            let rhs = 
                match Stream.peek stream with
                | Some (Token.Kwd op2) ->
                        (* If this binary operator binds less tightly with
                         * rhs than the operator after rhs, let the pending
                         * operator take rhs as _its_ lhs
                         *)
                        let next_prec = precedence op2 in
                        if token_prec < next_prec then
                            parse_bin_rhs (token_prec + 1) rhs stream
                        else rhs
                | _ -> rhs
            in

            (* Merge lhs and rhs *)
            let lhs = Binary (c, lhs, rhs) in
            parse_bin_rhs expr_prec lhs stream
        end
    | _ -> lhs

(* prototype
 *      ::= id '(' id* ')'
 *)
let parse_prototype = 
    let rec parse_args accumulator = parser
        | [< 'Token.Ident id; e=parse_args(id::accumulator) >] -> e
        | [< >] -> accumulator
    in

    parser
        | [< 'Token.Ident id;
        'Token.Kwd '(' ?? "Expected '(' after function name";
        args=parse_args [];
        'Token.Kwd ')' ?? "Expected ')' at end of function prototype" >] ->
            Prototype (id, Array.of_list (List.rev args))
        | [< >] ->
                raise (Message.Error "Expected function name in prototype")

(* definition
 *      ::= 'def' prototype expression
 *)
let parse_definition = parser
    | [< 'Token.Def; p=parse_prototype; e=parse_block >] ->
            Function (p, e)

(* extern
 *      ::= 'extern' prototype
 *)
let parse_extern = parser
    | [< 'Token.Extern; p=parse_prototype >] -> p

(* toplevel
 *      ::= expression
 *)
let parse_toplevel = parser
    | [< e=parse_expression >] ->
            Function (Prototype ("", [||]), e)
