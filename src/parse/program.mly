
%start main
%type <Ast.Triple.ut_t> main
%type <Ast.Program.ut_expr> command
%type <Ast.Program.ut_expr> expr

%%

main:
  LBRACE p = logic_expr RBRACE u = seq_command LBRACE q = logic_expr RBRACE EOF
    { Ast.Triple.{p; u; q } }
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