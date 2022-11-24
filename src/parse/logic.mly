
%type <Ast.Logic.ut_expr> logic_expr
%type <Ast.Logic.ut_expr> arith_expr

%%

%public logic_expr:
  | NOT e = logic_expr
      { UNot (e) }
  | e0 = logic_expr AND e1 = logic_expr
      { UAnd (e0, e1) }
  | e0 = logic_expr OR e1 = logic_expr
      { UOr (e0, e1) }
  | e0 = logic_expr IMPL e1 = logic_expr
      { UImpl (e0, e1) }
  | e = arith_expr
      { e }
;
arith_expr:
  | i = INT
      { UInt (i) }
  | b = BOOL
      { UBool (b) }
  | v = VAR
      { UVar (v) }
  | e0 = arith_expr PLUS e1 = arith_expr
      { UPlus (e0, e1) }
  | e0 = arith_expr MUL e1 = arith_expr
      { UMul (e0, e1) }
  | e0 = arith_expr EQ e1 = arith_expr
      { UEq (e0, e1) }
  | e0 = arith_expr LT e1 = arith_expr
      { ULt (e0, e1) }
  | LPAREN e = arith_expr RPAREN
      { ( e ) }
;
