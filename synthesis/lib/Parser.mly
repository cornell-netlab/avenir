%token <string> INT
%token <string> ID
%token QUESTION COMMA BAR POUND
%token TRUE
%token FALSE
%token OR AND NOT IMPLIES
%token EQ LESS GREATER LEQ GEQ NEQ
%token PLUS TIMES MINUS LAND
%token SKIP SEMICOLON ASSIGN
%token ASSUME APPLY
%token IF TOTAL PARTIAL ORDERED CASE BRACKETS FI
%token LPAREN RPAREN LBRACE RBRACE
%token EOF
%token FUNC
%left IMPLIES
%left OR
%left AND
%left PLUS
%left TIMES
%left SEMICOLON
%nonassoc NOT


%start <Cmd.t> main
%%

main :
| command EOF  { $1 }

command :
| SKIP
  { Cmd.Skip }
| c = command; SEMICOLON; cs = command
  { Cmd.seq c cs }
| f = ID; ASSIGN; e = expr
  { Cmd.assign f e }
| ASSUME; LPAREN; t = test; RPAREN
  { Cmd.assume (t) }
| IF; TOTAL; s = select; FI
  { Cmd.(total s) }
| IF; PARTIAL; s = select; FI
  { Cmd.(partial s) }
| IF; ORDERED; s = select; FI
  { Cmd.(ordered s) }
| APPLY; LPAREN; s = ID; COMMA; LPAREN; ks = keys; RPAREN; COMMA; LPAREN; a = actions; RPAREN; COMMA; LBRACE; d = command; RBRACE; RPAREN
  { Cmd.(apply (s,ks,a,d)) }

keys :
  | { [] }
  | k = ID; POUND; size = INT; COMMA; ks = keys { ((k, int_of_string size)::ks) }

params :
  | { [] }
  | p = ID; POUND; size = INT { [p,int_of_string size] }
  | p = ID; POUND; size = INT; COMMA; ps = params { ((p, int_of_string size)::ps) }

actions :
  | n = ID; LBRACE; c = command; RBRACE;  { [(n, [],c)] }
  | n = ID; LBRACE; FUNC; LPAREN; ps = params; RPAREN; CASE; c = command; RBRACE;
    { [(n, ps,c)] }
  | n = ID; LBRACE; c = command; RBRACE; BAR; acts = actions
    { (n, [],c)::acts }
  | n = ID; LBRACE; FUNC; LPAREN; ps = params; RPAREN; CASE; c = command; RBRACE; BAR; acts = actions
    { (n, ps,c)::acts }
                        
select :
| t = test; CASE; c = command; BRACKETS { [ t, c ] }
| t = test; CASE; c = command;          { [ t, c ] }
| t = test; CASE; c = command; BRACKETS; s = select
  { (t, c) :: s }

expr :
| i = INT; POUND; size = INT { Expr.Value (Value.big_make (Bigint.of_string i, int_of_string size)) }
| x = ID; POUND; size = INT  { Expr.Var (x, int_of_string size) }
| e = expr; PLUS; e1 = expr { Expr.(plus e e1) }
| e = expr; MINUS; e1 = expr { Expr.(minus e e1) }
| e = expr; TIMES; e1 = expr { Expr.(times e e1) }
| e = expr; LAND; e1 = expr { Expr.(mask e e1) }
| LPAREN; e = expr; RPAREN { e }
| QUESTION; x = ID; POUND; size = INT { Expr.Hole (x, int_of_string size) }

test :
| TRUE
  { Test.True }
| FALSE
  { Test.False }
| t = test; OR; tt = test
  { Test.or_ t tt }
| t = test; AND; tt = test
  { Test.and_ t tt }
| NOT; t = test
  { Test.neg t }
| e = expr; EQ; ee = expr
  { Test.eq e ee }
| e = expr; NEQ; ee = expr
  { Test.(e %<>% ee) }
| e = expr; LESS; ee = expr
  { Test.(e %<% ee) }
| e = expr; GREATER; ee = expr
  { Test.(e %>% ee) }
| e = expr; GEQ; ee = expr
  { Test.(e %>=% ee) }
| e = expr; LEQ; ee = expr
  { Test.(e %<=% ee) }
| LPAREN; t = test; RPAREN
  { t }
| t = test; IMPLIES; tt = test
  { Test.(t %=>% tt) }
