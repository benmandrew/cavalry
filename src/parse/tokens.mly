%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token LBRACE RBRACE
%token FUN COMMA REQUIRES ENSURES
%token SEMICOLON
%token ASSGN
%token IF THEN ELSE
%token WHILE DO END
%token EOF
%token PLUS SUB MUL
%token EQ NEQ LT LEQ GT GEQ
%token AND OR IMPL NOT
%token LPAREN RPAREN

%left SEMICOLON

%left IMPL
%left OR
%left AND
%nonassoc NOT
%left EQ NEQ LT LEQ GT GEQ
%left PLUS
%left SUB
%left MUL

%%
