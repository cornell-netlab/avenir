%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token OR AND NOT EQ
%token WHILE SKIP SEMICOLON ASSIGN
%token ASSERT
%token IF CASE FI
%token LPAREN RPAREN LBRACE RBRACE
%token EOF

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
| WHILE; LPAREN; t = test; RPAREN; LBRACE; e = expression RBRACE
  { Ast.mkWhile t e }
| f = ID; ASSIGN; v = value
  { Ast.Assign (f, v) }
| IF; s = select; FI
  { Ast.SelectFrom s }

select :
| t = test; CASE; e = expression
  { [ t, e ] }
| t = test; CASE; e = expression; SEMICOLON; s = select
  { (t, e) :: s }

value :
| i = INT { Ast.Int (i) }
| x = ID  { Ast.Var (x) }

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
| LPAREN; t = test; RPAREN
  { t }