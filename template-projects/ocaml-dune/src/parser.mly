%{
    open Ast
%}

%token HELLO
%token <string> WORD
%token EXCLAM
%token COMMA
%token EOF

%start expr
%type <expr> expr

%%

expr: HELLO; COMMA; w = WORD; EXCLAM; EOF { EHello w }
