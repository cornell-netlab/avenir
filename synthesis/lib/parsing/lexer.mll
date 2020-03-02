{
  open Parser

  exception ParseError of string


}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule tokens = parse
| [' ' '\t' '\n'] { tokens lexbuf }
| '-'?['0'-'9']+ as i { INT i }
| "partial"       { PARTIAL }
| "ordered"       { ORDERED }
| "assert"        { ASSERT }
| "assume"        { ASSUME }
| "abort"         { ABORT }
| "apply"         { APPLY }
| "true"          { TRUE }
| "false"         { FALSE }
| "while"         { WHILE }
| "total"         { TOTAL }
| "skip"          { SKIP }
| "==>"           { IMPLIES }
| ":="            { ASSIGN }
| "->"            { CASE }
| "if"            { IF }
| "[]"            { BRACKETS }
| "fi"            { FI }
| "||"            { OR }
| "&&"            { AND }
| "<>"            { NEQ }
| "!="            { NEQ }
| "<="            { LEQ }
| ">="            { GEQ }
| "~"             { NOT }
| "="             { EQ }
| "("             { LPAREN }
| ")"             { RPAREN }
| "{"             { LBRACE }
| "}"             { RBRACE }
| ";"             { SEMICOLON }
| ","             { COMMA }
| "|"             { BAR }
| "?"             { QUESTION }
| "#"             { POUND }
| ">"             { GREATER }
| "<"             { LESS }
| "\\"            { FUNC }
| eof             { EOF }
| id as x         { ID x }
| _ { raise (ParseError (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }