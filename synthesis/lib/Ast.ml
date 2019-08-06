open Core
open Util

type value =
  | Var of string
  | Hole of string
  | Int of int

let string_of_value v =
  match v with
  | Var s -> s
  | Hole s -> "?" ^ s
  | Int i -> string_of_int i

let sexp_string_of_value v =
  match v with
  | Var s -> "(Var " ^ s ^ ")"
  | Hole s -> "(Hole " ^ s ^ ")"  
  | Int i -> "(Int " ^ string_of_int i ^ ")"

type test =
  | True | False
  | LocEq of int
  | Eq of (value * value)
  | Lt of (value * value)
  | And of (test * test)
  | Or of (test * test)
  | Neg of test       

let mkEq v v' =
  if v = v' then True else
    match v, v' with 
    | Int _, Int _ -> False
    | Hole x, Hole x'
    | Var x, Var x' ->
      if x < x' then Eq (v, v') else Eq (v', v)
    (* Var < Int < Hole *)
    | Hole _, Int _
    | Hole _, Var _
    | Int  _, Var _
      ->  Eq(v', v)
    | Int _, Hole _
    | Var _, Hole _
    | Var _, Int _
      -> Eq (v, v')

let mkLt v v' = match v, v' with
    | Int first, Int second -> 
      if first < second then True else False
    | _, _ -> Lt(v, v')

let (%=%) = mkEq
let (%<>%) v v' = Neg(v %=% v') 

let (%<%) = mkLt

let rec mkOr t t' =
  match t, t' with
  | False, x  | x, False -> x
  | True, _ | _, True -> True
  | _, Or (t'', t''') -> (* left-associative *)
     mkOr (mkOr t t'') t'''
  | _ -> Or (t, t')

let (%+%) = mkOr

let rec mkAnd t t' =
  match t, t' with
  | True, x | x, True -> x
  | False, _ | _, False -> False
  | _, And ( t'', t''') -> (* left-associative *)
     mkAnd (mkAnd t t'') t'''
  | _ -> And (t, t')

let (%&%) = mkAnd
       
let mkNeg t =
  match t with
  | True -> False
  | False -> True
  | Neg t -> t
  | _ -> Neg t

let (!%) = mkNeg      
  
let mkImplies assum conseq = mkOr (mkNeg assum) conseq
let (%=>%) = mkImplies

let mkIff lhs rhs = (lhs %=>% rhs) %&% (rhs %=>% lhs)
let (%<=>%) = mkIff

let rec string_of_test t =
  match t with
  | True -> "true"
  | False -> "false"
  | LocEq i -> "loc = " ^ string_of_int i
  | Eq (left, right) -> string_of_value left ^ " = " ^ string_of_value right
  | Lt (left, right) -> string_of_value left ^ " < " ^ string_of_value right
  | Or (Neg(assum), conseq) -> "(" ^ string_of_test assum ^ " ==> " ^ string_of_test conseq ^ ")"
  | Or (left, right) -> "(" ^ string_of_test left ^ " || " ^ string_of_test right ^ ")"
  | And (left, right) -> "(" ^ string_of_test left ^ "&&" ^ string_of_test right ^ ")"
  | Neg t -> "~(" ^ string_of_test t ^ ")"

let rec sexp_string_of_test t =
  let binop opname left right recfun : string=
    opname ^ "(" ^ recfun left ^ "," ^ recfun right ^ ")" in
  match t with
  | True -> "True"
  | False -> "False"
  | LocEq l -> "LocEq(" ^ string_of_int l ^ ")"
  | Eq  (left, right) -> binop "Eq" left right sexp_string_of_value
  | Lt  (left, right) -> binop "Lt" left right sexp_string_of_value
  | Or  (left, right) -> binop "Or" left right sexp_string_of_test
  | And (left, right) -> binop "And" left right sexp_string_of_test
  | Neg t -> "Neg(" ^ sexp_string_of_test t ^ ")"
           
let rec remove_dups y xs =
  match xs with
  | [] -> []
  | x::xs ->
     if y = x then
       remove_dups y xs
     else
       x :: remove_dups y xs
    
let rec dedup xs =
  match xs with
  | [] -> []
  | x::xs ->
     let xs' = remove_dups x xs in
     x :: dedup xs'
  
let rec free_of_test typ test =
  begin match test with
  | True | False | LocEq _ ->
    []
  | Or (l,r) | And (l, r) ->
     free_of_test typ l @ free_of_test typ r
  | Neg t ->
    free_of_test typ t
  | Eq (v, v') | Lt (v, v') ->
    match typ, v, v'  with
    | `Var, Var x, Var x' ->
      [x; x']
    | `Var, _, Var x
    | `Var, Var x, _ ->
      [x]
    | `Hole, Hole x, Hole x' ->
      [x; x']
    | `Hole, Hole x, _
    | `Hole, _, Hole x ->
      [x]
    | _ ->
      []
  end
  |> dedup

let free_vars_of_test = free_of_test `Var
let holes_of_test = free_of_test `Hole

let rec multi_ints_of_test test =
  begin match test with
    | True | False | LocEq _ ->
      []
    | Or (l, r) | And (l, r) ->
      multi_ints_of_test l
      @ multi_ints_of_test r
    | Neg t ->
      multi_ints_of_test t
    | Eq (v, v') | Lt (v, v') ->
      match v, v' with
      | Int x, Int y ->
        [x; y]
      | Int x, _
      | _, Int x ->
        [x]
      | _ -> []
  end
           
type expr =
  | Skip
  | SetLoc of int
  | Assign of (string * value)
  | Assert of test
  | Assume of test
  | Seq of (expr * expr)
  | While of (test * expr)
  | PartialSelect of (test * expr) list
  | TotalSelect of (test * expr) list 

let mkSelects =
  concatMap ~init:(Some []) ~c:(@)
    ~f:(fun (cond, act) ->
        if cond = False then
          []
        else
          [cond, act])
let mkPartial ss =
  let selects = mkSelects ss in
  if List.length selects = 0 then
    Skip
  else
    PartialSelect selects

let mkTotal ss =
  let selects = mkSelects ss in
  if List.length selects = 0 then
    Assert False
  else
    TotalSelect selects


let mkIf cond tru = PartialSelect [(cond, tru)]
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
  | PartialSelect xs, PartialSelect ys -> PartialSelect (xs @ ys)
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
    (* repeat "\t" depth ^ *)
    "assert (" ^ string_of_test t ^ ")"
  | Assume t ->
    (* repeat "\t" depth ^ *)
    "assume (" ^ string_of_test t ^ ")"
  | SetLoc i -> "loc := " ^ string_of_int i
  | Assign (field, value) ->
    field ^ " := " ^ string_of_value value
  | PartialSelect es ->
     string_of_select "if" "fi" depth es
  | TotalSelect es -> 
     string_of_select "if total" "fi" depth es

and string_of_select openstr closestr depth es =
  openstr ^
    List.fold_left es ~init:"" ~f:(fun str (cond, act)->
        str ^ "\n" ^
          repeat "\t" (depth + 1)
          ^ string_of_test cond  ^ " -> " ^ string_of_expr ~depth:(depth+2) act ^ " []"
      )
    ^ "\n" ^ repeat "\t" depth ^ closestr
  
let rec sexp_string_of_expr e : string =
  let string_select = concatMap 
                        ~f:(fun (cond,act) -> "(" ^ sexp_string_of_test cond ^ "," ^ sexp_string_of_expr act ^ ")")
                        ~c:(fun acc d -> acc ^ ";" ^ d) in
  match e with
  | Skip -> "Skip"
  | While (cond, body) -> "While("^ sexp_string_of_test cond ^ "," ^ sexp_string_of_expr body ^")"
  | Seq (p, q) -> "Seq(" ^ sexp_string_of_expr p ^ "," ^ sexp_string_of_expr q ^ ")"
  | Assert t -> "Assert(" ^ sexp_string_of_test t ^ ")"
  | Assume t -> "Assume(" ^ sexp_string_of_test t ^ ")"
  | SetLoc l ->  "SetLoc(" ^ string_of_int l ^ ")"
  | Assign (f,v) -> "Assign(" ^ f ^ "," ^ string_of_value v ^")"
  | PartialSelect [] -> "PartialSelect([])"
  | PartialSelect exprs -> "PartialSelect([" ^ string_select exprs ^ "])"
  | TotalSelect [] -> "TotalSelect([])"
  | TotalSelect exprs -> "PartialSelect([" ^ string_select exprs ^ "])"

  
let rec free_of_expr typ (e:expr) : string list =
  begin match e with
  | Skip | SetLoc _ -> []
  | Assign (f, v) ->
     f :: (match v,typ with
        | Hole x,`Hole -> [x]
        | Var x, `Var -> [x]
        | _, _ -> [] )
  | Seq (p, q) ->
     free_of_expr typ p @ free_of_expr typ q
  | While (cond, body) ->
     free_of_test typ cond
     @ free_of_expr typ body
  | Assert t | Assume t -> free_of_test typ t
  | PartialSelect ss 
  | TotalSelect ss ->
    List.fold ss ~init:[] ~f:(fun fvs (test, action) ->
        free_of_test typ test
         @ free_of_expr typ action
         @ fvs
      )
  end
  |> dedup


let free_vars_of_expr = free_of_expr `Var
let holes_of_expr = free_of_expr `Hole
      
let rec multi_ints_of_expr e =
  match e with
  | Skip
  | SetLoc _ ->
    []
  (* | Assign (_, Int i) ->
   *   [i] *)
  (* Only collect _tested_ inputs*)
  | Assign _ ->
    []
  | Seq (p, q) ->
    multi_ints_of_expr p
    @ multi_ints_of_expr q
  | While (cond, body) ->
     multi_ints_of_test cond
     @ multi_ints_of_expr body
  | Assert t
  | Assume t ->
    multi_ints_of_test t
  | PartialSelect ss 
  | TotalSelect ss ->
    concatMap ss ~init:(Some []) ~c:(@)
      ~f:(fun (test, action) ->
        multi_ints_of_test test
        @ multi_ints_of_expr action
        )
 
  
