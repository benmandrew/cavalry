{
open Parser
}

rule main = parse
  | [' ' '\t']
      { main lexbuf }
  | '\n'
      { EOL }
  | ['0'-'9']+ as i
      { INT (int_of_string i) }
  | '+'
      { PLUS }
  | '*'
      { MUL }
