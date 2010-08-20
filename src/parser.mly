%{
    let toplevel_error s = 
        raise (Message.Error s)
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token PLUS MINUS TIMES DIVIDE
%token EQUALS LTHAN GTHAN LEQUAL GEQUAL NEQUAL
%token COMMA COLON
%token NEWLINE BEGIN
%token <int> END
%token DEF FUN RETURN
%token IF ELIF ELSE
%token EOF

%left EQUALS LTHAN GTHAN LEQUAL GEQUAL NEQUAL
%left PLUS MINUS
%left TIMES DIVIDE

%start toplevel
%type <unit> toplevel

%%
toplevel:
    | EOF {}
    | exprtop {}
    | deftop {}

exprtop: 
    | expr EOF {}
    | expr NEWLINE {}
    | expr NEWLINE toplevel {}

deftop:
    | def NEWLINE toplevel {}

def: 
    | DEF IDENT paramlist COLON NEWLINE block {}

block: 
    | END toplevel {}
    | exprtop END toplevel {}

expr:
    | arith {}
    | value {}

arith:
    | expr PLUS expr {}
    | expr MINUS expr {}
    | expr TIMES expr {}
    | expr DIVIDE expr {}

paramlist:
    | {}
    | IDENT defaultvalue {}
    | IDENT defaultvalue COMMA paramlist {}

defaultvalue:
    | {}
    | EQUALS value {}

value: 
    | INT {}
    | FLOAT {}
    | IDENT {}
    | STRING {}
