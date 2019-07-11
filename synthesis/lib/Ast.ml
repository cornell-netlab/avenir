open Core

type value =
  | Var of string
  | Hole of string
  | Int of int

let string_of_value v =
  match v with
  | Var s | Hole s -> s
  | Int i -> string_of_int i

type test =
  | True | False
  | Eq of (value * value)
  | Lt of (value * value)
  | And of (test * test)
  | Or of (test * test)
  | Neg of test         

let mkEq v v' =
  if v = v' then True else
    match v, v' with
    | Int _, Int _ -> False
    | _, _ -> Eq(v, v')

let mkLt v v' =
  if v < v' then True else
    match v, v' with
    | Int _, Int _ -> False
    | _, _ -> Lt(v, v')

let (%=%) = mkEq
let (%<>%) v v' = Neg(v %=% v') 

let (%<%) = mkLt

let mkOr t t' =
  match t, t' with
  | False, x  | x, False -> x
  | _ -> Or (t, t')

let (%+%) = mkOr

let mkAnd t t' =
  match t, t' with
  | True, x | x, True -> x
  | _ -> And (t, t')

let (%&%) = mkAnd
       
let mkNeg t =
  match t with
  | True -> False
  | False -> True
  | _ -> Neg t

let (!%) = mkNeg      
  
let mkImplies assum conseq = mkOr (Neg assum) conseq
let (%=>%) = mkImplies

let mkIff lhs rhs = (lhs %=>% rhs) %&% (rhs %=>% lhs)
let (%<=>%) = mkIff

let rec string_of_test t =
  match t with
  | True -> "true"
  | False -> "false"
  | Eq (left, right) -> string_of_value left ^ " = " ^ string_of_value right
  | Lt (left, right) -> string_of_value left ^ " < " ^ string_of_value right
  | Or (left, right) -> "(" ^ string_of_test left ^ " || " ^ string_of_test right ^ ")"
  | And (left, right) -> "(" ^ string_of_test left ^ "&&" ^ string_of_test right ^ ")"
  | Neg t -> "~(" ^ string_of_test t ^ ")"


let rec free_vars_of_test test =
  begin match test with
  | True | False ->
     []
  | Or (l,r) | And (l, r) ->
     free_vars_of_test l @ free_vars_of_test r
  | Neg t ->
     free_vars_of_test t
  | Eq (Var v, Var v') -> [v; v']
  | Eq (Var v, _) | Eq (_, Var v) -> [v]
  | Eq (_, _) -> []
  | Lt (Var v, Var v') -> [v; v']
  | Lt (Var v, _) | Lt (_, Var v) -> [v]
  | Lt (_, _) -> [] 
  end
  |> List.dedup_and_sort ~compare
           
type expr =
  | Skip
  | Assign of (string * value)
  | Assert of test
  | Assume of test
  | Seq of (expr * expr)
  | While of (test * expr)
  | SelectFrom of (test * expr) list

let mkIf cond tru = SelectFrom [(cond, tru)]
let (%?%) = mkIf

let mkSeq first scnd =
  match first, scnd with
  | Skip, x | x, Skip -> x
  | _,_ -> Seq(first, scnd)
         
let (%:%) = mkSeq

let mkAssn f v = Assign (f, v)
let (%<-%)= mkAssn
         

let combineSelects e e' =
  match e, e' with
  | SelectFrom xs, SelectFrom ys -> SelectFrom (xs @ ys)
  | _ -> failwith "Can only combine selects statements "

let (%%) = combineSelects

let mkWhile t e = While(t,e)

       
let rec repeat c n =  if n = 0 then "" else c ^ repeat c (n-1)

let rec string_of_expr ?depth:(depth=0) (e : expr) : string =
  match e with
  | Skip -> "skip"
  | While (cond, body) ->
    "\n" ^ repeat "\t" depth ^
    "while(" ^ string_of_test cond ^ ") {\n"
      ^ repeat "\t" (depth+1)
      ^ string_of_expr ~depth:(depth+1) body
      ^ "\n" ^ repeat "\t" depth
      ^ "}\n" ^ repeat "\t" depth
  | Seq (firstdo, thendo) ->
    string_of_expr ~depth firstdo ^ "; "
    ^ string_of_expr ~depth thendo
  | Assert t ->
     repeat "\t" depth ^ "assert ("
     ^ string_of_test t ^ ")"
  | Assume t ->
     repeat "\t" depth ^ "assume ("
     ^ string_of_test t ^ ")"
  | Assign (field, value) ->
    field ^ " := " ^ string_of_value value
  | SelectFrom es ->
     (* "\n" ^ repeat "\t" depth ^*) "if" ^
      List.fold_left es ~init:"" ~f:(fun str (cond, act)->
            str ^ "\n" ^
              repeat "\t" (depth + 1)
              ^ string_of_test cond  ^ " -> " ^ string_of_expr ~depth:(depth+2) act ^ " []"
        )
      ^ "\n" ^ repeat "\t" depth ^ "fi"


let rec free_vars_of_expr (e:expr) : string list =
  match e with
  | Skip -> []
  | Assign (f, v) ->
     f :: (match v with
           | Int _ | Hole _ -> []
           | Var x -> [x])
  | Seq (p, q) ->
     free_vars_of_expr p @ free_vars_of_expr q
  | While (cond, body) ->
     free_vars_of_test cond
     @ free_vars_of_expr body
  | Assert t | Assume t -> free_vars_of_test t
  | SelectFrom ss ->
     List.fold ss ~init:[] ~f:(fun fvs (test, action) ->
         free_vars_of_test test
         @ free_vars_of_expr action
         @ fvs
       )
           
      
                     
