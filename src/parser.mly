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
%token NEWLINE BEGIN
%token <int> END
%token DEF FUN RET
%token IF ELIF ELSE
%token EOF

%left EQUALS LTHAN GTHAN LEQUAL GEQUAL NEQUAL
%left PLUS MINUS
%left TIMES DIVIDE

%start parse
%type <unit> parse

%%
parse:
    | expr {}
    | defn COLON block {}

expr: {}
    | INT {}
    | expr PLUS expr {}
    | expr MINUS expr {}
    | expr TIMES expr {}
    | expr DIVIDE expr {}

expressions:
    | expr {}
    | block {}
    | expr NEWLINE expressions {}

defn: {}
    | DEF IDENT paramlist {}

block: 
    | BEGIN expressions END{}

paramlist: {}
    | IDENT defaultvalue {}
    | IDENT defaultvalue COMMA paramlist {}

defaultvalue: {}
    | EQUALS value {}

value: INT {}
    | FLOAT {}
    | IDENT {}
    | STRING {}
