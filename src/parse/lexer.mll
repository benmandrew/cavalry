{
open Tokens

type error = Illegal_character of char

exception Error of error
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
  | '-'
      { SUB }
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
  | "while"
      { WHILE }
  | "do"
      { DO }
  | "end"
      { END }
  | "procedure"
      { PROCEDURE }
  | "requires"
      { REQUIRES }
  | "ensures"
      { ENSURES }
  | (['a'-'z']|['A'-'Z']|'_')+
      { VAR (Lexing.lexeme lexbuf) }
  | eof
      { EOF }
  | (_ as illegal_char)
    { raise
        (Error
          (Illegal_character illegal_char)) }
