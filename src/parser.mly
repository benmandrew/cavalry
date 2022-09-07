
%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token PLUS MUL EQ
%token LET IN IF THEN ELSE
%token LPAREN RPAREN
%token EOF
%left EQ
%left PLUS
%left MUL
%start main
%type <Ast.ut_expr> main
%type <Ast.ut_expr> control_expr
%type <Ast.ut_expr> expr
%%
main:
  control_expr EOF
    { $1 }
;
control_expr:
  | LET VAR EQ control_expr IN control_expr
      { ULet ($2, $4, $6) }
  | IF control_expr THEN control_expr ELSE control_expr
      { UIf ($2, $4, $6) }
  | expr
      { $1 }
;
expr:
  | INT
      { UInt ($1) }
  | BOOL
      { UBool ($1) }
  | VAR
      { UVar ($1) }
  | expr PLUS expr
      { UPlus ($1, $3) }
  | expr MUL expr
      { UMul ($1, $3) }
  | expr EQ expr
      { UEq ($1, $3) }
  | paren_expr
      { $1 }
;
paren_expr:
  | LPAREN control_expr RPAREN
      { ( $2 ) }
;
