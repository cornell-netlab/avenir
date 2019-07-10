open Ast
    
val unroll : int -> expr -> expr
val wp : expr -> test -> test
val dnf : test -> test list
val nnf : test -> test
  
