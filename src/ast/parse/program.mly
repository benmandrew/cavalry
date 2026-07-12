%start top
%type <Ast.Triple.ut_t list> top
%type <Ast.Triple.ut_t> main
%type <Ast.Triple.ut_t> procedure
%type <Ast.Logic.arith_expr option> variant_opt
%type <string list> variable_list
%type <Ast.Program.ut_expr> command
%type <Ast.Program.ut_expr> command_desc
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
  | PROCEDURE f = VAR LPAREN ps = variable_list RPAREN EQ REQUIRES LBRACE p = logic_expr RBRACE ENSURES LBRACE q = logic_expr RBRACE variant = variant_opt WRITES LBRACE ws = variable_list RBRACE u = command END
      { { Ast.Triple.p; q; variant; ws; f; ps; u } }
;
variant_opt:
  |
      { None }
  | VARIANT LBRACE m = arith_expr RBRACE
      { Some m }
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
      { { Ast.Triple.p; q; variant = None; ws = []; f="main"; ps=[]; u } }
;
command:
  | c = command_desc
      { ULoc (Ast.Loc.of_span $loc, c) }
;
command_desc:
  | v = VAR ASSGN ARRAY LPAREN n = expr RPAREN
      { UArrMake (v, n) }
  | a = VAR LBRACKET i = expr RBRACKET ASSGN e = expr
      { UArrAssgn (a, i, e) }
  | v = VAR ASSGN e = expr
      { UAssgn (v, e) }
  | f = VAR LPAREN ps = expression_list RPAREN
      { UProc (f, ps) }
  | c0 = command SEMICOLON c1 = command
      { USeq (c0, c1) }
  | IF e = expr THEN c0 = command ELSE c1 = command END
      { UIf (e, c0, c1) }
  (* One-armed [if]: an absent [else] is a no-op. Desugar it to an [else]
     branch that evaluates the trivial expression [0] and discards it -- a bare
     expression is already a valid (effect-free) command, so this needs no new
     AST node and its WLP collapses to the identity. *)
  | IF e = expr THEN c0 = command END
      { UIf (e, c0, UInt 0) }
  | WHILE e = expr DO INVARIANT LBRACE inv = logic_expr RBRACE c = command END
      { UWhile (inv, None, e, c) }
  | WHILE e = expr DO INVARIANT LBRACE inv = logic_expr RBRACE VARIANT LBRACE m = arith_expr RBRACE c = command END
      { UWhile (inv, Some m, e, c) }
  | PRINT e = expr
      { UPrint e }
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
  | a = VAR LBRACKET i = expr RBRACKET
      { UGet (a, i) }
  | LEN LPAREN a = VAR RPAREN
      { ULen (a) }
  | e0 = expr PLUS e1 = expr
      { UPlus (e0, e1) }
  | e0 = expr SUB e1 = expr
      { USub (e0, e1) }
  | e0 = expr MUL e1 = expr
      { UMul (e0, e1) }
  | e0 = expr DIV e1 = expr
      { UDiv (e0, e1) }
  | e0 = expr MOD e1 = expr
      { UMod (e0, e1) }
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
  | e0 = expr AND e1 = expr
      { UAnd (e0, e1) }
  | e0 = expr OR e1 = expr
      { UOr (e0, e1) }
  | NOT e = expr
      { UNot (e) }
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