type value =
  | Var of string
  | Hole of string
  | Int of int
         

val string_of_value :  value -> string

type test =
  | True
  | False
  | Eq of (value * value)
	| Lt of (value * value) 
  | And of (test * test)
  | Or of (test * test)
  | Neg of test

val string_of_test : test -> string
val free_vars_of_test : test -> string list

val mkImplies : test -> test -> test
val (%=>%) : test -> test -> test
val mkIff : test -> test -> test
val (%<=>%) : test -> test -> test
val mkEq : value -> value -> test
val (%=%) : value -> value -> test
val (%<>%) : value -> value -> test
val mkLt : value -> value -> test
val (%<%) : value -> value -> test
val mkOr : test -> test -> test
val (%+%) : test -> test -> test
val mkAnd : test -> test -> test
val (%&%) : test -> test -> test
val mkNeg : test -> test
val (!%) : test -> test
    
type expr =
  | Skip
  | Assign of (string * value)
  | Assert of test
  | Seq of (expr * expr)
  | While of (test * expr)
  | SelectFrom of (test * expr) list

val mkIf : test -> expr -> expr
val (%?%) : test -> expr -> expr
val mkSeq : expr -> expr -> expr
val (%:%) : expr -> expr -> expr
val mkAssn : string -> value -> expr
val (%<-%) : string -> value -> expr
val mkWhile : test -> expr -> expr


val combineSelects : expr -> expr -> expr (*PRE : both input exprs are [SelectFrom]s*)
val (%%) : expr -> expr -> expr

val string_of_expr : ?depth:int -> expr -> string
val free_vars_of_expr : expr -> string list
                                         
