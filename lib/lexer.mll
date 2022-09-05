{
open Parser
}

rule main = parse
  | [' ' '\t' '\n']
      { main lexbuf }
  | ['0'-'9']+ as i
      { INT (int_of_string i) }
  | '+'
      { PLUS }
  | '*'
      { MUL }
  | "let"
      { LET }
  | '='
      { EQ }
  | "in"
      { IN }
  | (['a'-'z']) (['a'-'z']['A'-'Z'])*
      { VAR (Lexing.lexeme lexbuf) }
  | eof
      { EOF }
