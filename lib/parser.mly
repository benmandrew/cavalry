
%token <int> INT
%token <string> VAR
%token PLUS MUL
%token EQ
%token LET IN
%token EOF
%left PLUS
%left MUL
%start main
%type <Ast.ast> main
%%
main:
  expr EOF
    { $1 }
;
expr:
  | INT
      { Int ($1) }
  | expr PLUS expr
      { Plus ($1, $3) }
  | expr MUL expr
      { Mul ($1, $3) }
  | LET VAR EQ expr IN expr
      { Let ($2, $4, $6) }
  | VAR
      { Var ($1) }
;
