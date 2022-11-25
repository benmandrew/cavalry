
%type <Ast.Logic.expr> logic_expr
%type <Ast.Logic.expr> arith_expr

%%

%public logic_expr:
  | NOT e = logic_expr
      { Not (e) }
  | e0 = logic_expr AND e1 = logic_expr
      { And (e0, e1) }
  | e0 = logic_expr OR e1 = logic_expr
      { Or (e0, e1) }
  | e0 = logic_expr IMPL e1 = logic_expr
      { Impl (e0, e1) }
  | e = arith_expr
      { e }
;
arith_expr:
  | i = INT
      { Int (i) }
  | b = BOOL
      { Bool (b) }
  | v = VAR
      { Var (v) }
  | e0 = arith_expr PLUS e1 = arith_expr
      { Plus (e0, e1) }
  | e0 = arith_expr MUL e1 = arith_expr
      { Mul (e0, e1) }
  | e0 = arith_expr EQ e1 = arith_expr
      { Eq (e0, e1) }
  | e0 = arith_expr LT e1 = arith_expr
      { Lt (e0, e1) }
  | LPAREN e = arith_expr RPAREN
      { ( e ) }
;
