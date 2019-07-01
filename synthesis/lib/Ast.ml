open Core

type value =
  | Var of string
  | Int of int

let string_of_value v =
  match v with
  | Var s -> s
  | Int i -> string_of_int i

type test =
  | True | False
  | Eq of (string * value)
  | And of (test * test)
  | Or of (test * test)
  | Neg of test

let rec string_of_test t =
  match t with
  | True -> "true"
  | False -> "false"
  | Eq (field, value) -> field ^ " = " ^ string_of_value value
  | Or (left, right) -> "(" ^ string_of_test left ^ ") || (" ^ string_of_test right ^ ")"
  | And (left, right) -> string_of_test left ^ "&" ^ string_of_test right
  | Neg t -> "~(" ^ string_of_test t ^ ")"

type expr =
  | While of (test * expr)
  | Seq of (expr * expr)
  | Assign of (string * value)
  | Test of test
  | SelectFrom of expr list

let mkIf cond tru fls = SelectFrom [
    Seq (Test cond, tru);
    Seq (Test (Neg cond), fls)
  ]

let rec repeat c n =  if n = 0 then "" else c ^ repeat c (n-1)

let rec string_of_expr ?depth:(depth=0) (e : expr) : string =
  match e with 
  | While (cond, body) ->
    repeat "\t" depth ^
    "while(" ^ string_of_test cond ^ ") {\n"
      ^ repeat "\t" (depth+1)
      ^ string_of_expr ~depth:(depth+1) body ^ "\n"
      ^ repeat "\t" depth ^ "}\n"
  | Seq (firstdo, thendo) ->
    string_of_expr ~depth firstdo ^ ";"
    ^ string_of_expr ~depth thendo
  | Assign (field, value) ->
    repeat "\t" depth ^
    field ^ " := " ^ string_of_value value
  | Test test ->
    repeat "\t" depth ^ string_of_test test
  | SelectFrom es ->
    let needJoin = ref false in
    List.fold_left es ~init:"" ~f:(fun str e' ->
        str ^
        (if !needJoin then "  []  " else (needJoin := true; "")) 
        ^ string_of_expr e'
      )

    
   
    
