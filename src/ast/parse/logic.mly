
%type <Ast.Logic.logic_expr> logic_expr
%type <Ast.Logic.arith_expr> arith_expr

%%

%public logic_expr:
  | b = BOOL
      { Bool (b) }
  | v = VAR
      { Ast.Logic.BoolVar (v) }
  | a = VAR LBRACKET i = arith_expr RBRACKET
      { Ast.Logic.BGet (a, i) }
  | NOT e = logic_expr
      { Not (e) }
  | e0 = logic_expr AND e1 = logic_expr
      { And (e0, e1) }
  | e0 = logic_expr OR e1 = logic_expr
      { Or (e0, e1) }
  | e0 = logic_expr IMPL e1 = logic_expr
      { Impl (e0, e1) }
  | FORALL x = VAR DOT e = logic_expr
      { Forall (x, e) }
  | EXISTS x = VAR DOT e = logic_expr
      { Exists (x, e) }
  | e0 = arith_expr EQ e1 = arith_expr
      { Eq (e0, e1) }
  | e0 = arith_expr NEQ e1 = arith_expr
      { Neq (e0, e1) }
  | e0 = arith_expr LT e1 = arith_expr
      { Lt (e0, e1) }
  | e0 = arith_expr LEQ e1 = arith_expr
      { Leq (e0, e1) }
  | e0 = arith_expr GT e1 = arith_expr
      { Gt (e0, e1) }
  | e0 = arith_expr GEQ e1 = arith_expr
      { Geq (e0, e1) }
;
%public arith_expr:
  | i = INT
      { Int (i) }
  | v = VAR
      { Var (v) }
  | a = VAR LBRACKET i = arith_expr RBRACKET
      { Get (a, i) }
  | LEN LPAREN a = VAR RPAREN
      { Len (a) }
  | e0 = arith_expr PLUS e1 = arith_expr
      { Plus (e0, e1) }
  | e0 = arith_expr SUB e1 = arith_expr
      { Sub (e0, e1) }
  | e0 = arith_expr MUL e1 = arith_expr
      { Mul (e0, e1) }
  | e0 = arith_expr DIV e1 = arith_expr
      { Div (e0, e1) }
  | e0 = arith_expr MOD e1 = arith_expr
      { Mod (e0, e1) }
  | LPAREN e = arith_expr RPAREN
      { ( e ) }
;
