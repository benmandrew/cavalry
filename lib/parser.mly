// %{
// open Ast
// %}

%token <int> INT
%token PLUS MUL
%token EOL
%left PLUS
%left MUL
%start main
%type <Ast.ast> main
%%
main:
  expr EOL
    { $1 }
;
expr:
  | INT
      { Int ($1) }
  | expr PLUS expr
      { Plus ($1, $3) }
  | expr MUL expr
      { Mul ($1, $3) }
;
