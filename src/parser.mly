
%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token SEMICOLON
%token PLUS MUL EQ ASSGN
%token IF THEN ELSE
%token LPAREN RPAREN
%token EOF
%left SEMICOLON
%left EQ
%left PLUS
%left MUL
%start main
%type <Ast.ut_expr> main
%type <Ast.ut_expr> seq_expr
%type <Ast.ut_expr> control_expr
%type <Ast.ut_expr> expr
%type <Ast.ut_expr> paren_expr
%%
main:
  seq_expr EOF
    { $1 }
;
seq_expr:
  | seq_expr SEMICOLON seq_expr
      { USeq ($1, $3) }
  | control_expr
      { $1 }
;
control_expr:
  | VAR ASSGN control_expr
      { UAssgn ($1, $3) }
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
  | LPAREN seq_expr RPAREN
      { ( $2 ) }
;
