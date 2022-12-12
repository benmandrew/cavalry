
%start main
%type <Ast.Triple.ut_t> main
%type <Ast.Program.ut_expr> command
%type <Ast.Program.ut_expr> expr

%%

main:
  LBRACE p = logic_expr RBRACE u = command LBRACE q = logic_expr RBRACE EOF
    { Ast.Triple.{p; u; q } }
;
command:
  | v = VAR ASSGN e = expr
      { UAssgn (v, e) }
  | c0 = command SEMICOLON c1 = command
      { USeq (c0, c1) }
  | IF e = expr THEN c0 = command ELSE c1 = command END
      { UIf (e, c0, c1) }
  | WHILE e = expr DO c = command END
      { UWhile (e, c) }
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
