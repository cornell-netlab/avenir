%token <int> INT
%token <string> ID
%token QUESTION COMMA BAR
%token TRUE
%token FALSE
%token OR AND NOT IMPLIES
%token EQ LESS GREATER LEQ GEQ NEQ
%token PLUS TIMES MINUS
%token LOC
%token WHILE SKIP SEMICOLON ASSIGN
%token ASSERT ASSUME ABORT APPLY
%token IF TOTAL PARTIAL ORDERED CASE BRACKETS FI
%token LPAREN RPAREN LBRACE RBRACE
%token EOF

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
| LOC; ASSIGN; i = INT
  { Ast.SetLoc i }
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
| k = ID; COMMA; ks = keys { (k::ks) }

actions :
| LBRACE; c = command; RBRACE; { [c] }
| LBRACE; c = command; RBRACE; BAR; a = actions { c::a }
                        
select :
| t = test; CASE; c = command; BRACKETS { [ t, c ] }
| t = test; CASE; c = command;          { [ t, c ] }
| t = test; CASE; c = command; BRACKETS; s = select
  { (t, c) :: s }

tuple :
| e = expr; COMMA; e1 = expr { [e; e1] }
| e = expr; COMMA; t = tuple { e :: t  }
  
expr :
| i = INT { Ast.(Value1 (Int i)) }
| x = ID  { Ast.Var1 (x) }
| e = expr; PLUS; e1 = expr { Ast.(mkPlus e e1) }
| e = expr; MINUS; e1 = expr { Ast.(mkPlus e e1) }
| e = expr; TIMES; e1 = expr { Ast.(mkTimes e e1) }
| LPAREN; t = tuple; RPAREN { Ast.(mkTuple t) }
| LPAREN; e = expr; RPAREN { e }
| QUESTION; x = ID { Ast.Hole1 (x) }

test :
| TRUE
  { Ast.True }
| FALSE
  { Ast.False }
| LOC; EQ; i = INT;
  { Ast.LocEq i }
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
  { Ast.Lt (e, ee) }
| e = expr; GREATER; ee = expr
  { Ast.Lt (ee, e) }
| e = expr; GEQ; ee = expr
  { Ast.Or(Ast.Lt(ee, e), Ast.Eq(ee, e)) }
| e = expr; LEQ; ee = expr
  { Ast.Or(Ast.Lt(e, ee), Ast.Eq(e, ee)) }
| LPAREN; t = test; RPAREN
  { t }
| t = test; IMPLIES; tt = test
  { Ast.(Or(Neg(t), tt)) }
