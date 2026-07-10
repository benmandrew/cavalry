%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token LBRACE RBRACE
%token PROCEDURE COMMA REQUIRES ENSURES INVARIANT WRITES
%token SEMICOLON
%token ASSGN PRINT
%token IF THEN ELSE
%token WHILE DO END
%token EOF
%token PLUS SUB MUL DIV MOD
%token EQ NEQ LT LEQ GT GEQ
%token AND OR IMPL NOT
%token LPAREN RPAREN
%token LBRACKET RBRACKET DOT
%token FORALL EXISTS LEN ARRAY

%left SEMICOLON

%nonassoc FORALL EXISTS DOT
%left IMPL
%left OR
%left AND
%nonassoc NOT
%left EQ NEQ LT LEQ GT GEQ
%left PLUS
%left SUB
%left MUL DIV MOD

%%
