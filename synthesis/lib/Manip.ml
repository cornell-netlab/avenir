open Core
open Ast

module StringMap = Map.Make (String)


(* computes the product of two lists of disjuncitons *)
let multiply orlist orlist' =
  let foil outer inner =
    List.fold outer ~init:[] ~f:(fun acc x ->
        List.map inner ~f:(mkAnd x)
        @ acc
      )
  in
  foil orlist orlist'
  @ foil orlist' orlist
    
  
(* Computes the Negation Normal Form of a test*)               
let rec nnf t : test =
  match t with
  | Eq(_, _)
	  | Lt(_, _) 
    | True
    | False
    | Neg(Eq(_, _))
		| Neg(Lt(_, _))
    | Neg(True)
    | Neg(False) -> t
  | Neg (Neg t) -> nnf t
  | And (a, b) -> mkAnd (nnf a) (nnf b)
  | Or (a, b) -> mkOr (nnf a) (nnf b)
  | Neg(And(a, b)) -> mkOr (Neg a) (Neg b) |> nnf
  | Neg(Or(a, b)) -> mkAnd (Neg a) (Neg b) |> nnf


(* Computes the Disjunctive Normal form of a test *)
let rec dnf t : test list =
  match nnf t with
  | And(a, b) -> multiply (dnf a) (dnf b)
  | Or (a, b) -> dnf a @ dnf b
  | Eq _
	  | Lt _ 
    | Neg _ (* will not be And/Or because NNF*)
    | True
    | False  ->  [t]

               
                 
(* Unrolls all loops in the program p n times *)
let rec unroll n p =
  match p, n with
  | While (_, _), 0 -> Skip
  | While (cond, body), _ -> 
     cond %?% (body %:% unroll (n-1) p)
  | Seq (firstdo, thendo), _ ->
    Seq (unroll n firstdo, unroll n thendo)
  | SelectFrom exprs, _ ->
    List.map exprs ~f:(fun (cond, action) -> (cond, unroll n action))
    |> SelectFrom
  | _ -> p (* Assign, Test, Assert cannot be unrolled *)

let get_val subsMap str default =
  StringMap.find subsMap str |> Option.value ~default

(* computes ex[xs -> vs] *)
let rec substitute ex subsMap =
  let subst = get_val subsMap in 
  match ex with
  | True  -> True
  | False -> False
  (* Homomorphic Rules*)               
  | Neg e       -> !%(substitute e subsMap)
  | Or  (e, e') -> substitute e subsMap %+% substitute e' subsMap
  | And (e, e') -> substitute e subsMap %&% substitute e' subsMap
  (* Do the work *)
  | Eq (v,v') -> 
    (match v, v' with
    | Var field, Var field' -> subst field v %=% subst field' v'
    | Var field, _          -> subst field v %=% v'             
    | _        , Var field' -> v %=% subst field' v'
    | _        , _          -> v %=% v')
  | Lt (v,v') -> 
    (match v, v' with
    | Var field, Var field' -> subst field v %<% subst field' v'
    | Var field, _          -> subst field v %<% v'             
    | _        , Var field' -> v %<% subst field' v'
    | _        , _          -> v %<% v')


(* computes weakest pre-condition of condition phi w.r.t command c *)
let rec wp c phi = match c with
  | Skip -> phi
  | Seq (firstdo, thendo) ->
    wp firstdo (wp thendo phi)
  | Assign (field, value) ->
     substitute phi (StringMap.singleton field value)
  | Assert t -> t %&% phi
  | Assume t -> t %=>% phi
  | SelectFrom exprs -> (* requires at least one guard to be true *)
    And(List.fold exprs ~init:False ~f:(fun acc (cond, _  ) -> acc %+% cond),
        List.fold exprs ~init:True ~f:(fun acc (cond, act) -> acc %&% (cond %=>% wp act phi))
       )                                     
  | While _ ->
    Printf.printf "Warning: skipping While loop, because loops must be unrolled\n%!";
    phi
