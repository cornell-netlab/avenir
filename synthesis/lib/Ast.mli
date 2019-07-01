type value =
  | Var of string
  | Int of int

val string_of_value :  value -> string

type test =
  | True | False
  | Eq of (string * value)
  | And of (test * test)
  | Or of (test * test)
  | Neg of test

val string_of_test : test -> string

type expr =
  | While of (test * expr)
  | Seq of (expr * expr)
  | Assign of (string * value)
  | Test of test
  | SelectFrom of expr list

val mkIf : test -> expr -> expr -> expr

val string_of_expr : ?depth:int -> expr -> string
