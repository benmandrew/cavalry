
%start main
%type <Ast.Program.ut_expr> main
%type <Ast.Program.ut_expr> command
%type <Ast.Program.ut_expr> expr

%%

main:
  LBRACE le0 = logic_expr RBRACE c = seq_command LBRACE le1 = logic_expr RBRACE EOF
    { ignore le0; ignore le1; c }
;
seq_command:
  | c0 = seq_command SEMICOLON c1 = seq_command
      { USeq (c0, c1) }
  | c = command
      { c }
;
command:
  | v = VAR ASSGN e = expr
      { UAssgn (v, e) }
  | IF e = expr THEN c0 = command ELSE c1 = command
      { UIf (e, c0, c1) }
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
  | LPAREN e = expr RPAREN
      { ( e ) }
;