%token <int> INT
%token <string> ID
%token QUESTION
%token TRUE
%token FALSE
%token OR AND NOT EQ LESS GREATER LEQ GEQ NEQ IMPLIES
%token WHILE SKIP SEMICOLON ASSIGN
%token ASSERT ASSUME ABORT
%token IF CASE BRACKETS FI
%token LPAREN RPAREN LBRACE RBRACE
%token EOF

%left IMPLIES
%left OR
%left AND
%left SEMICOLON
%nonassoc NOT


%start <Ast.expr> main
%%

main :
| expression EOF  { $1 }

expression :
| SKIP
  { Ast.Skip }
| e = expression; SEMICOLON; ee = expression
  { Ast.Seq (e, ee) }
| WHILE; LPAREN; t = test; RPAREN; LBRACE; e = expression; RBRACE
  { Ast.mkWhile t e }
| f = ID; ASSIGN; v = value
  { Ast.Assign (f, v) }
| ASSERT; LPAREN; t = test ; RPAREN
  { Ast.Assert (t) }
| ABORT
  { Ast.Assert (Ast.False) }
| ASSUME; LPAREN; t = test; RPAREN
  { Ast.Assume (t) }
| IF; s = select; FI
  { Ast.PartialSelect s }

select :
| t = test; CASE; e = expression; BRACKETS { [ t, e ] }
| t = test; CASE; e = expression { [ t, e ] }
| t = test; CASE; e = expression; BRACKETS; s = select
  { (t, e) :: s }

value :
| i = INT { Ast.Int (i) }
| x = ID  { Ast.Var (x) }
| QUESTION; x = ID { Ast.Hole (x) }

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
| v = value; EQ; vv = value
  { Ast.Eq (v, vv) }
| v = value; NEQ; vv = value
  { Ast.Neg(Ast.Eq (v, vv)) }
| v = value; LESS; vv = value
  { Ast.Lt (v, vv) }
| v = value; GREATER; vv = value
  { Ast.Lt (vv, v) }
| v = value; GEQ; vv = value
  { Ast.Or(Ast.Lt(vv, v), Ast.Eq(vv, v)) }
| v = value; LEQ; vv = value
  { Ast.Or(Ast.Lt(v, vv), Ast.Eq(v, vv)) }
| LPAREN; t = test; RPAREN
  { t }
| t = test; IMPLIES; tt = test
  { Ast.(Or(Neg(t), tt)) }