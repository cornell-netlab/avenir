open Core
open Ast

module StringMap = Map.Make (String)


(* Unrolls all loops in the program p n times *)
let rec unroll n p =
  if n = 0 then p else
  match p with
  | While (cond, body) ->
    mkIf cond (Seq(body, unroll (n-1) p))
  | Seq (firstdo, thendo) ->
    Seq (unroll n firstdo, unroll n thendo)
  | SelectFrom exprs ->
    List.map exprs ~f:(fun (cond, action) -> (cond, unroll n action))
    |> SelectFrom
  | _ -> p (* Assign, Test cannot be unrolled *)

let get_val subsMap str default =
  StringMap.find subsMap str |> Option.value ~default

(* computes ex[xs -> vs] *)
let rec substitute ex subsMap =
  let subst = get_val subsMap in 
  match ex with
  | True  -> True
  | False -> False
  (* Homomorphic Rules*)               
  | Neg e       -> Neg (substitute e subsMap)
  | Or  (e, e') -> Or  (substitute e subsMap, substitute e' subsMap)
  | And (e, e') -> And (substitute e subsMap, substitute e' subsMap)
  (* Do the work *)
  | Eq (v,v') -> 
    match v, v' with
    | Var field, Var field' -> Eq (subst field v, subst field' v')
    | Var field, _          -> Eq (subst field v, v'             )
    | _        , Var field' -> Eq (v            , subst field' v')
    | _        , _          -> Eq (v            , v'             )

(* computes weakest pre-condition of condition phi w.r.t command c *)
let rec wp c phi = match c with
  | Skip -> phi
  | Seq (firstdo, thendo) ->
    wp firstdo (wp thendo phi)
  | Assign (field, value) ->
    substitute phi (StringMap.singleton field value)
  | SelectFrom exprs ->
    And(List.fold exprs ~init:False ~f:(fun acc (cond, _  ) -> Or  (acc, cond)),
        List.fold exprs ~init:True ~f:(fun acc (cond, act) -> And (acc, mkImplies cond (wp act phi)))
       )                                     
  | While _ ->
    Printf.printf "Warning: skipping While loop, because loops must be unrolled\n%!";
    phi
