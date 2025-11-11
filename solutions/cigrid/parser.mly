%token Break
%token Char
%token Delete
%token Else
%token Extern
%token For
%token If
%token Int
%token New
%token Return
%token Struct 
%token Void

%token <string> Ident
%token Eof
%token <int> IntConst
%token <string> CharConst
%token <string> StringConst

%token Exclamation "!"
%token Tylde "~"
%token Minus "-"
%token Star "*"
%token FSlash "/"
%token Percent "%"
%token Plus "+"
%token SmallerThan "<"
%token BiggerThan ">"
%token Equal "="
%token And "&"
%token Bar "|"
%token RParent ")"
%token LParent "("
%token SemiColon ";"
%token RCurly "}"
%token LCurly "{"

%token ShiftLeft "<<"
%token ShiftRight ">>"
%token SmallerEqual "<="
%token BiggerEqual ">="
%token DoubleEqual "=="
%token NotEqual "!="
%token AndAnd "&&"
%token Or "||"


%left "||"
%left "&&"
%left "|"
%left "&"
%left "==" "!="
%left "<" ">" "<=" ">="
%left "<<" ">>"
%left "+" "-"
%left "*" "/" "%"
%right "!" "~" UMINUS

%%

| "-" e = expr %prec UMINUS