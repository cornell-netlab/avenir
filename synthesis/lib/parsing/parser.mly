%token <string> INT
%token <string> ID
%token QUESTION COMMA BAR POUND
%token TRUE
%token FALSE
%token OR AND NOT IMPLIES
%token EQ LESS GREATER LEQ GEQ NEQ
%token PLUS TIMES MINUS
%token WHILE SKIP SEMICOLON ASSIGN
%token ASSERT ASSUME ABORT APPLY
%token IF TOTAL PARTIAL ORDERED CASE BRACKETS FI
%token LPAREN RPAREN LBRACE RBRACE
%token EOF
%token FUNC
%left IMPLIES
%left OR
%left AND
%left SEMICOLON
%nonassoc NOT


%start <Ast.cmd> main
%%

main :
| command EOF  { $1 }

command :
| SKIP
  { Ast.Skip }
| c = command; SEMICOLON; cs = command
  { Ast.Seq (c, cs) }
| WHILE; LPAREN; t = test; RPAREN; LBRACE; c = command; RBRACE
  { Ast.mkWhile t c }
| f = ID; ASSIGN; e = expr
  { Ast.Assign (f, e) }
| ASSERT; LPAREN; t = test ; RPAREN
  { Ast.Assert (t) }
| ABORT
  { Ast.Assert (Ast.False) }
| ASSUME; LPAREN; t = test; RPAREN
  { Ast.Assume (t) }
| IF; TOTAL; s = select; FI
  { Ast.(Select (Total, s)) }
| IF; PARTIAL; s = select; FI
  { Ast.(Select (Partial, s)) }
| IF; ORDERED; s = select; FI
  { Ast.(Select (Ordered, s)) }
| APPLY; LPAREN; s = ID; COMMA; LPAREN; ks = keys; RPAREN; COMMA; LPAREN; a = actions; RPAREN; COMMA; LBRACE; d = command; RBRACE; RPAREN
  { Ast.(Apply(s,ks,a,d)) }

keys :
  | { [] }
  | k = ID; POUND; size = INT; COMMA; ks = keys { ((k, int_of_string size)::ks) }

params :
  | { [] }
  | k = ID; POUND; size = INT { [k,int_of_string size] }
  | k = ID; POUND; size = INT; COMMA; ks = keys { ((k, int_of_string size)::ks) }

actions :
  | LBRACE; c = command; RBRACE;  { [([],c)] }
  | LBRACE; FUNC; LPAREN; ps = params; RPAREN; CASE; c = command; RBRACE;
    { [(ps,c)] }
  | LBRACE; FUNC; LPAREN; ps = params; RPAREN; CASE; c = command; RBRACE; BAR; a = actions
    { (ps,c)::a }
                        
select :
| t = test; CASE; c = command; BRACKETS { [ t, c ] }
| t = test; CASE; c = command;          { [ t, c ] }
| t = test; CASE; c = command; BRACKETS; s = select
  { (t, c) :: s }

expr :
| i = INT; POUND; size = INT { Ast.(Value (Int (Bigint.of_string i, int_of_string size))) }
| x = ID; POUND; size = INT  { Ast.Var (x, int_of_string size) }
| e = expr; PLUS; e1 = expr { Ast.(mkPlus e e1) }
| e = expr; MINUS; e1 = expr { Ast.(mkPlus e e1) }
| e = expr; TIMES; e1 = expr { Ast.(mkTimes e e1) }
| LPAREN; e = expr; RPAREN { e }
| QUESTION; x = ID; POUND; size = INT { Ast.Hole (x, int_of_string size) }

test :
| TRUE
  { Ast.True }
| FALSE
  { Ast.False }
| t = test; OR; tt = test
  { Ast.Or (t, tt) }
| t = test; AND; tt = test
  { Ast.And (t, tt) }
| NOT; t = test
  { Ast.Neg t }
| e = expr; EQ; ee = expr
  { Ast.Eq (e, ee) }
| e = expr; NEQ; ee = expr
  { Ast.Neg(Ast.Eq (e, ee)) }
| e = expr; LESS; ee = expr
  { Ast.(e %<% ee) }
| e = expr; GREATER; ee = expr
  { Ast.(e %>% ee) }
| e = expr; GEQ; ee = expr
  { Ast.(e %>=% ee) }
| e = expr; LEQ; ee = expr
  { Ast.(e %<=% ee) }
| LPAREN; t = test; RPAREN
  { t }
| t = test; IMPLIES; tt = test
  { Ast.(t %=>% tt) }
