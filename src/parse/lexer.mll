{
open Tokens
}

rule main = parse
  | [' ' '\t' '\n']
      { main lexbuf }
  | ['0'-'9']+ as i
      { INT (int_of_string i) }
  | "true"
      { BOOL true }
  | "false"
      { BOOL false }
  | '+'
      { PLUS }
  | '*'
      { MUL }
  | '='
      { EQ }
  | '<'
      { LT }
  | "<="
      { LEQ }
  | ">"
      { GT }
  | ">="
      { GEQ }
  | "!="
      { NEQ }
  | '!'
      { NOT }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "->"
      { IMPL }
  | ';'
      { SEMICOLON }
  | "<-"
      { ASSGN }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '{'
      { LBRACE }
  | '}'
      { RBRACE }
  | "if"
      { IF }
  | "then"
      { THEN }
  | "else"
      { ELSE }
  | (['a'-'z']) (['a'-'z']['A'-'Z'])*
      { VAR (Lexing.lexeme lexbuf) }
  | eof
      { EOF }
