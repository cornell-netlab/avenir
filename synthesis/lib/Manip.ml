open Core
open Ast
open Util

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
  | LocEq _
  | Eq(_, _)
  | Lt(_, _)
  | Member(_,_)
  | True
  | False
  | Neg(Eq(_, _))
  | Neg(LocEq _)
  | Neg(Lt(_, _))
  | Neg(Member(_,_))
  | Neg(True)
  | Neg(False) -> t
  | Neg (Neg t) -> nnf t
  | And (a, b) -> mkAnd (nnf a) (nnf b)
  | Or (a, b) -> mkOr (nnf a) (nnf b)
  | Neg(And(a, b)) -> mkOr (Neg a) (Neg b) |> nnf
  | Neg(Or(a, b)) -> mkAnd (Neg a) (Neg b) |> nnf


(* Computes the Disjunctive Normal form of a test *)
let rec dnf t : test list =
  let t' = nnf t in
  match t' with
  | And(a, b) -> multiply (dnf a) (dnf b)
  | Or (a, b) -> dnf a @ dnf b
  | Member (_,_)
  | LocEq _
  | Eq _
  | Lt _ 
  | Neg _ (* will not be And/Or because NNF*)
  | True
  | False  ->  [t']
               
                 
(* Unrolls all loops in the program p n times *)
let rec unroll n p =
  match p, n with
  | While (_, _), 0 -> Skip
  | While (cond, body), _ -> 
    mkPartial [cond , (body %:% unroll (n-1) p)]
    (* %:% Assert (!% cond) *)
  | Seq (firstdo, thendo), _ ->
    Seq (unroll n firstdo, unroll n thendo)
  | Select (styp, cmds), _ ->
    List.map cmds ~f:(fun (cond, action) -> (cond, unroll n action))
    |> mkSelect styp
  | _ -> p (* Assign, Test, Assert cannot be unrolled *)

let get_val subsMap str default =
  StringMap.find subsMap str |> Option.value ~default

(** computes ex[xs -> vs], replacing Vars only *)
let rec substitute ex subsMap =
  let subst = get_val subsMap in
  let rec substituteE e =
    match e with
    | Var field -> subst field e
    | Int _ | Hole _ -> e
    | Plus (e, e') -> Plus (substituteE e, substituteE e')
    | Times (e, e') -> Times (substituteE e, substituteE e')
    | Minus (e, e') -> Minus (substituteE e, substituteE e')
  in
  match ex with
  | True | False | LocEq _ -> ex
  (* Homomorphic Rules*)               
  | Neg e       -> !%(substitute e subsMap)
  | Or  (e, e') -> substitute e subsMap %+% substitute e' subsMap
  | And (e, e') -> substitute e subsMap %&% substitute e' subsMap
  (* Do the work *)
  | Eq (e,e') ->  substituteE e %=% substituteE e'
  | Lt (e,e') ->  substituteE e %<% substituteE e'
  | Member(e,set) -> Member(substituteE e, set)

              
(* computes weakest pre-condition of condition phi w.r.t command c *)
let rec wp c phi =
  let guarded_wp (cond, act) = cond %=>% wp act phi in
  let rec subst_location l phi =
    match phi with
    (* Do the Work *)
    | LocEq l' -> if l' = l then True else False
    (* Do nothing *)
    | True | False | Eq _ | Lt _ | Member _ -> phi
    (* Homorphically recurse *)
    | And (p, q) -> subst_location l p %&% subst_location l q
    | Or (p, q) -> subst_location l p %+% subst_location l q
    | Neg p -> !%(subst_location l p)
  in
  match c with
  | Skip -> phi
  | Seq (firstdo, thendo) ->
    wp firstdo (wp thendo phi)
  | SetLoc l -> subst_location l phi
  | Assign (field, value) ->
     substitute phi (StringMap.singleton field value)
  | Assert t -> t %&% phi
  | Assume t -> t %=>% phi
              
  (* requires at least one guard to be true *)
  | Select (Total, []) -> False
  | Select (Total, cmds) ->
    concatMap cmds ~c:(%+%) ~f:fst 
    %&% concatMap cmds ~c:(%&%) ~f:guarded_wp
    
  (* doesn't require at any guard to be true *)
  | Select (Partial, []) -> True
  | Select (Partial, cmds) ->
    concatMap cmds ~c:(%&%) ~f:guarded_wp

  (* negates the previous conditions *)
  | Select (Ordered, cmds) ->
    List.fold cmds ~init:(True, False) ~f:(fun (wp_so_far, prev_conds) (cond, act) ->
        guarded_wp (cond %&% !%prev_conds, act) %&% wp_so_far
      , prev_conds %+% cond
      )
    |> fst

  | Apply (_, _, acts, dflt)
    -> concatMap acts  ~f:(fun a -> wp a phi) ~c:(mkAnd) ~init:(Some True)
      %&% wp dflt phi
  | While _ ->
    Printf.printf "[WARNING] skipping While loop, because loops must be unrolled\n%!";
    phi



(** [fill_holes(|_value|_test]) replace the applies the substitution
   [subst] to the supplied cmd|value|test. It only replaces HOLES, and
   has no effect n vars *)
let rec fill_holes_value v subst =
  let binop op e e' = op (fill_holes_value e subst) (fill_holes_value e' subst) in
  match v with
  | Int _ | Var _ -> v
  | Hole h ->
     begin match StringMap.find subst h with
     | None -> v
     | Some v' -> v'
     end
  | Plus (e, e') -> binop mkPlus e e'
  | Minus (e, e') -> binop mkMinus e e'
  | Times (e, e') -> binop mkTimes e e'


(* Fills in first-order holes according to subst  *)                  
let rec fill_holes_test t subst =
  let binop cnstr rcall left right = cnstr (rcall left subst) (rcall right subst) in
  match t with
  | True | False | LocEq _ -> t
  | Neg a -> mkNeg (fill_holes_test a subst)
  | And (a, b) -> binop mkAnd fill_holes_test  a b
  | Or (a, b)  -> binop mkOr  fill_holes_test  a b
  | Lt (a, b)  -> binop mkLt  fill_holes_value a b
  | Eq (a, b)  -> binop mkEq  fill_holes_value a b
  | Member (a, s) -> Member(fill_holes_value a subst, s)

let rec fill_holes (c : cmd) subst =
  let rec_select = concatMap ~c:(@)
                     ~f:(fun (cond, act) ->
                       [(fill_holes_test cond subst, fill_holes act subst)]) in
  match c with
  | Assign (f, Hole h) ->
     begin match StringMap.find subst h with
     | None -> c
     | Some v -> Assign (f, v)
     end
  | SetLoc _ | Assign (_, _) -> c
  | Seq (firstdo, thendo) ->
     fill_holes firstdo subst %:% fill_holes thendo subst
  | Assert t ->
     fill_holes_test t subst |> Assert
  | Assume t ->
     fill_holes_test t subst |> Assume
  | Select (_,[]) | Skip ->
     c
  | Select (styp, cmds) ->
     rec_select cmds |> mkSelect styp
  | While (cond, body) -> While (fill_holes_test cond subst, fill_holes body subst)
  | Apply (n,keys, acts, dflt)
    -> Apply(n, keys
             , List.map acts ~f:(fun act -> fill_holes act subst)
             , fill_holes dflt subst)
     
