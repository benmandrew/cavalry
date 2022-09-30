
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
%type <Ast.ut_expr> command
%type <Ast.ut_expr> expr
%type <Ast.ut_expr> paren_expr
%%
main:
  seq_command EOF
    { $1 }
;
seq_command:
  | seq_command SEMICOLON seq_command
      { USeq ($1, $3) }
  | command
      {$1}
;
command:
  | VAR ASSGN expr
      { UAssgn ($1, $3) }
  | IF expr THEN command ELSE command
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
  | LPAREN expr RPAREN
      { ( $2 ) }
;
