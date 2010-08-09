%{
    let parse_error s = 
        raise (Message.Error s)
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token PLUS MINUS TIMES DIVIDE
%token EQUALS LTHAN GTHAN LEQUAL GEQUAL NEQUAL
%token COMMA COLON
%token DEF FUN RET
%token IF ELIF ELSE
%token EOF

%left EQUALS LTHAN GTHAN LEQUAL GEQUAL NEQUAL
%left PLUS MINUS
%left TIMES DIVIDE

%start parse
%type <unit> parse

%%
parse: {}
    | expression {}
    | defn COLON block {}

expression: {}
    | INT {}
    | expression PLUS expression {}

defn: {}
    | DEF IDENT parameters {}

block: {}

parameters: {}
    | IDENT {}
    | IDENT EQUALS value {}
    | parameters COMMA parameters {}

value: INT {}
    | FLOAT {}
    | IDENT {}
    | STRING {}
