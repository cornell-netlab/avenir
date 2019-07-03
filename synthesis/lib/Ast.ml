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
  | Eq of (value * value)
  | And of (test * test)
  | Or of (test * test)
  | Neg of test

let mkImplies assum conseq = Or(Neg assum, conseq)

let rec string_of_test t =
  match t with
  | True -> "true"
  | False -> "false"
  | Eq (left, right) -> string_of_value left ^ " = " ^ string_of_value right
  | Or (left, right) -> "(" ^ string_of_test left ^ ") || (" ^ string_of_test right ^ ")"
  | And (left, right) -> string_of_test left ^ "&" ^ string_of_test right
  | Neg t -> "~(" ^ string_of_test t ^ ")"

type expr =
  | Skip
  | While of (test * expr)
  | Seq of (expr * expr)
  | Assign of (string * value)
  | SelectFrom of (test * expr) list

let mkIf cond tru = SelectFrom [
    (cond, tru)
  ]

let combineSelects e e' =
  match e, e' with
  | SelectFrom xs, SelectFrom ys -> SelectFrom (xs @ ys)
  | _ -> failwith "Can only combine selects statements "

let rec repeat c n =  if n = 0 then "" else c ^ repeat c (n-1)

let rec string_of_expr ?depth:(depth=0) (e : expr) : string =
  match e with
  | Skip -> "skip"
  | While (cond, body) ->
    "\n" ^ repeat "\t" depth ^
    "while(" ^ string_of_test cond ^ ") {\n"
      (* ^ repeat "\t" (depth+1) *)
      ^ string_of_expr ~depth:(depth+1) body ^ "\n"
      ^ repeat "\t" depth ^ "}\n"
  | Seq (firstdo, thendo) ->
    string_of_expr ~depth firstdo ^ "; "
    ^ string_of_expr ~depth thendo
  | Assign (field, value) ->
    repeat "\t" depth ^
    field ^ " := " ^ string_of_value value
  (* | Test test ->
   *   repeat "\t" depth ^ string_of_test test *)
  | SelectFrom es ->
    "\n" ^ repeat "\t" depth ^ "if" ^
    List.fold_left es ~init:"" ~f:(fun str (cond, act)->
        str ^ "\n"  ^
        repeat "\t" (depth + 1)
        ^ string_of_test cond  ^ " -> " ^ string_of_expr act
      )
    ^ "\n" ^ repeat "\t" depth ^ "fi"

    
   
    
