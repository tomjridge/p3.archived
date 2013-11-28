rule lex = parse
  | [' ' '\n' '\t'] { lex lexbuf }
  | ['0'-'9']+ as s { `INT(int_of_string s) }
  | '+'             { `PLUS }
  | '-'             { `MINUS }
  | '*'             { `TIMES }
  | '('             { `OPEN }
  | ')'             { `CLOSE }
  | eof             { `EOF }
