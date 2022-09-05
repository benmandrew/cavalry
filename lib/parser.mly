
%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token PLUS MUL EQ
%token LET IN IF THEN ELSE
%token EOF
%left PLUS
%left MUL
%start main
%type <Ast.ut_expr> main
// %type <Ast.ut_expr> expr
%%
main:
  expr EOF
    { $1 }
;
expr:
  | INT
      { UInt ($1) }
  | BOOL
      { UBool ($1) }
  | expr PLUS expr
      { UPlus ($1, $3) }
  | expr MUL expr
      { UMul ($1, $3) }
  | LET VAR EQ expr IN expr
      { ULet ($2, $4, $6) }
  | IF expr THEN expr ELSE expr
      { UIf ($2, $4, $6) }
  | VAR
      { UVar ($1) }
;
