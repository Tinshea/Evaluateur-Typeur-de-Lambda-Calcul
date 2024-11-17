%{
open Lambda
%}

%token <string> IDENT
%token <int> INT
%token LAMBDA LET IN FIX
%token REF DEREF ASSIGN
%token LPAREN RPAREN LBRACK RBRACK
%token ARROW DOT EQUALS COMMA
%token PLUS MINUS STAR
%token EOF

%start <Lambda.pterm> prog

%right ARROW
%left PLUS MINUS
%left STAR

%%

prog:
  | e = expr EOF { e }
;

expr:
  | LAMBDA x = IDENT DOT e = expr { Abs(x, e) }
  | LET x = IDENT EQUALS e1 = expr IN e2 = expr { Let(x, e1, e2) }
  | app_expr { $1 }
;

app_expr:
  | e = simple_expr { e }
  | e1 = app_expr e2 = simple_expr { App(e1, e2) }
  | REF e = simple_expr { Ref(e) }
  | DEREF e = simple_expr { Deref(e) }
  | e1 = simple_expr ASSIGN e2 = simple_expr { Assign(e1, e2) }
  | e1 = app_expr PLUS e2 = app_expr { Add(e1, e2) }
  | e1 = app_expr MINUS e2 = app_expr { Sub(e1, e2) }
;

simple_expr:
  | LPAREN e = expr RPAREN { e }
  | x = IDENT { Var(x) }
  | n = INT { Int(n) }
  | LBRACK es = separated_list(COMMA, expr) RBRACK { List(es) }
;