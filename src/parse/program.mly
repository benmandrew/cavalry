%start top
%type <Ast.Triple.ut_t list> top
%type <Ast.Triple.ut_t> main
%type <Ast.Triple.ut_t> procedure
%type <string list> variable_list
%type <Ast.Program.ut_expr> command
%type <Ast.Program.ut_expr> expr
%type <Ast.Program.ut_expr list> expression_list

%%

top:
  | f = procedure t = top
      { f :: t }
  | m = main EOF
      { [ m ] }
;
procedure:
  | PROCEDURE f = VAR LPAREN ps = variable_list RPAREN EQ REQUIRES LBRACE p = logic_expr RBRACE ENSURES LBRACE q = logic_expr RBRACE WRITES LBRACE ws = variable_list RBRACE u = command END
      { { Ast.Triple.p; q; ws; f; ps; u } }
;
variable_list:
  | p = VAR COMMA ps = variable_list
      { p :: ps }
  | p = VAR
      { [ p ] }
  |
      { [] }
;
main:
  | LBRACE p = logic_expr RBRACE u = command LBRACE q = logic_expr RBRACE
      { { Ast.Triple.p; q; ws = []; f="main"; ps=[]; u } }
;
command:
  | v = VAR ASSGN e = expr
      { UAssgn (v, e) }
  | f = VAR LPAREN ps = expression_list RPAREN
      { UProc (f, ps) }
  | c0 = command SEMICOLON c1 = command
      { USeq (c0, c1) }
  | IF e = expr THEN c0 = command ELSE c1 = command END
      { UIf (e, c0, c1) }
  | WHILE e = expr DO LBRACE inv = logic_expr RBRACE c = command END
      { UWhile (inv, e, c) }
  | e = expr
      { e }
;
expr:
  | i = INT
      { UInt (i) }
  | b = BOOL
      { UBool (b) }
  | v = VAR
      { UVar (v) }
  | e0 = expr PLUS e1 = expr
      { UPlus (e0, e1) }
  | e0 = expr SUB e1 = expr
      { USub (e0, e1) }
  | e0 = expr MUL e1 = expr
      { UMul (e0, e1) }
  | e0 = expr EQ e1 = expr
      { UEq (e0, e1) }
  | e0 = expr NEQ e1 = expr
      { UNeq (e0, e1) }
  | e0 = expr LT e1 = expr
      { ULt (e0, e1) }
  | e0 = expr LEQ e1 = expr
      { ULeq (e0, e1) }
  | e0 = expr GT e1 = expr
      { UGt (e0, e1) }
  | e0 = expr GEQ e1 = expr
      { UGeq (e0, e1) }
  | LPAREN e = expr RPAREN
      { ( e ) }
;
expression_list:
  | p = expr COMMA ps = expression_list
      { p :: ps }
  | p = expr
      { [ p ] }
  |
      { [] }
;