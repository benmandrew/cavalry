%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token LBRACE RBRACE
%token SEMICOLON
%token ASSGN
%token IF THEN ELSE
%token EOF
%token PLUS MUL
// %token EQ NEQ LT LEQ GT GEQ
%token EQ LT
%token AND OR IMPL NOT
%token LPAREN RPAREN

%left SEMICOLON

%left IMPL
%left OR
%left AND
%nonassoc NOT
%left EQ
%left LT
%left PLUS
%left MUL

%%
