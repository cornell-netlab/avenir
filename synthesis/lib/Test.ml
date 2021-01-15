open Util
open Core

let enable_smart_constructors = true

type t =
  | True
  | False
  | Eq of (Expr.t * Expr.t)
  | Le of (Expr.t * Expr.t)
  | And of (t * t)
  | Or of (t * t)
  | Impl of (t * t)
  | Iff of (t * t)
  | Neg of t

let rec equals a b =
  match (a, b) with
  | True, True -> true
  | False, False -> true
  | Eq (ea1, ea2), Eq (eb1, eb2) | Le (ea1, ea2), Le (eb1, eb2) ->
      Expr.equals ea1 eb1 && Expr.equals ea2 eb2
  | And (a1, a2), And (b1, b2)
   |Or (a1, a2), Or (b1, b2)
   |Impl (a1, a2), Impl (b1, b2)
   |Iff (a1, a2), Iff (b1, b2) ->
      equals a1 b1 && equals a2 b2
  | Neg a, Neg b -> equals a b
  | _, _ -> false

let rec to_string t =
  match t with
  | True -> "true"
  | False -> "false"
  | Eq (left, right) -> Expr.to_string left ^ " = " ^ Expr.to_string right
  | Le (left, right) -> Expr.to_string left ^ " <= " ^ Expr.to_string right
  | Impl (assum, conseq) ->
      "(" ^ to_string assum ^ " ==> " ^ to_string conseq ^ ")\n"
  | Iff (left, right) ->
      "(" ^ to_string left ^ " <==> " ^ to_string right ^ ")\n"
  | Or (left, right) -> "(" ^ to_string left ^ " || " ^ to_string right ^ ")"
  | And (left, right) ->
      "(" ^ to_string left ^ " && " ^ to_string right ^ ")"
  | Neg (Le (left, right)) ->
      Printf.sprintf "(%s < %s)" (Expr.to_string right) (Expr.to_string left)
  | Neg (Eq (left, right)) ->
      Printf.sprintf "(%s <> %s)" (Expr.to_string left)
        (Expr.to_string right)
  | Neg t -> "~(" ^ to_string t ^ ")"

let rec to_sexp_string t =
  let binop opname left right recfun : string =
    opname ^ "(" ^ recfun left ^ "," ^ recfun right ^ ")"
  in
  match t with
  | True -> "True"
  | False -> "False"
  | Eq (left, right) -> binop "Eq" left right Expr.to_sexp_string
  | Le (left, right) -> binop "Le" left right Expr.to_sexp_string
  | Impl (assum, conseq) -> binop "Impl" assum conseq to_sexp_string
  | Iff (left, right) -> binop "Iff" left right to_sexp_string
  | Or (left, right) -> binop "Or" left right to_sexp_string
  | And (left, right) -> binop "And" left right to_sexp_string
  | Neg t -> "Neg(" ^ to_sexp_string t ^ ")"

let rec or_ a b =
  if enable_smart_constructors then
    match (a, b) with
    | False, x | x, False -> x
    | True, _ | _, True -> True
    | _, Or (b1, b2) ->
        (* left-associative *)
        or_ (or_ a b1) b2
    | _ -> Or (a, b)
  else Or (a, b)

let bigor = List.fold ~init:False ~f:or_

let ( %+% ) = or_

let rec and_ a b =
  if enable_smart_constructors then
    if equals a b then a
    else
      match (a, b) with
      | True, x | x, True -> x
      | False, _ | _, False -> False
      | Eq (Var x, u), Neg (Eq (Var y, v)) when Stdlib.(x = y) ->
          if Expr.equals u v then False else Eq (Var x, u)
      | _, And (b1, b2) ->
          (* left-associative *)
          and_ (and_ a b1) b2
      | _ -> And (a, b)
  else And (a, b)

let bigand = List.fold ~init:True ~f:and_

let ( %&% ) = and_

let neg t =
  if enable_smart_constructors then
    match t with True -> False | False -> True | Neg t -> t | _ -> Neg t
  else Neg t

let ( !% ) = neg

let eq (e : Expr.t) (e' : Expr.t) =
  if not enable_smart_constructors then Eq (e, e')
  else if Expr.equals e e' then
    (*Printf.printf "[=] %s and %s are equal, so True\n" (Expr.to_string e)
      (Expr.to_string e');*)
    True
  else
    let norm =
      match (e, e') with
      | Value _, Value _ ->
          (*Printf.printf "[=] %s and %s are unequal values so False\n"
            (Expr.to_string e) (Expr.to_string e');*)
          False
      | Hole x, Hole x' | Var x, Var x' ->
          if String.(fst x < fst x') then Eq (e, e') else Eq (e', e)
      | _, _ -> if Expr.ord e < Expr.ord e' then Eq (e, e') else Eq (e', e)
    in
    match norm with
    | Eq (Value _, Mask (Var (_, _), Value m))
      when Value.big_eq m Bigint.zero ->
        True
    | _ -> norm

let leq (e : Expr.t) (e' : Expr.t) : t =
  if not enable_smart_constructors then Le (e, e')
  else
    match (e, e') with
    | Value first, Value second ->
        if Value.leq first second then True else False
    | _, _ -> Le (e, e')

let ( %=% ) = eq

let ( %<>% ) v v' = neg (v %=% v')

let ( %<=% ) = leq

let ( %>=% ) e e' = e' %<=% e

let ( %<% ) e e' = !%(e %>=% e')

let ( %>% ) e e' = !%(e %<=% e')

let impl assum conseq =
  if not enable_smart_constructors then Impl (assum, conseq)
  else if equals assum conseq then True
  else
    match (assum, conseq) with
    | True, _ -> conseq
    | _, True | False, _ -> True
    | _, False -> !%assum
    | _, _ -> Impl (assum, conseq)

let ( %=>% ) = impl

let iff lhs rhs =
  if enable_smart_constructors then
    if equals lhs rhs then True else Iff (lhs, rhs)
  else Iff (lhs, rhs)

let ( %<=>% ) = iff

let rec num_nodes t =
  match t with
  | True | False -> 1
  | Eq (left, right) -> Expr.num_nodes left + Expr.num_nodes right + 1
  | Le (left, right) -> Expr.num_nodes left + Expr.num_nodes right + 1
  | Iff (left, right)
   |Or (left, right)
   |And (left, right)
   |Impl (left, right) ->
      num_nodes left + 1 + num_nodes right
  | Neg t -> num_nodes t + 1

let rec frees typ test : (string * int) list =
  ( match test with
  | True | False -> []
  | Or (l, r) | And (l, r) | Impl (l, r) | Iff (l, r) ->
      frees typ l @ frees typ r
  | Neg t -> frees typ t
  | Eq (e, e') | Le (e, e') -> Expr.frees typ e @ Expr.frees typ e' )
  |> dedup

let vars = frees `Var

let holes = frees `Hole

let rec has_hole = function
  | True | False -> false
  | Neg b -> has_hole b
  | And (a, b) | Or (a, b) | Impl (a, b) | Iff (a, b) ->
      has_hole a || has_hole b
  | Eq (e1, e2) | Le (e1, e2) -> Expr.has_hole e1 || Expr.has_hole e2

let rec multi_vals test : Value.t list =
  match test with
  | True | False -> []
  | Or (l, r) | And (l, r) | Impl (l, r) | Iff (l, r) ->
      multi_vals l @ multi_vals r
  | Neg t -> multi_vals t
  | Eq (e, e') | Le (e, e') -> Expr.multi_vals e @ Expr.multi_vals e'

let rec holify ~f holes b : t =
  match b with
  | True | False -> b
  | Eq (e, e') -> Expr.holify ~f holes e %=% Expr.holify ~f holes e'
  | Le (e, e') -> Expr.holify ~f holes e %<=% Expr.holify ~f holes e'
  | And (b, b') -> holify ~f holes b %&% holify ~f holes b'
  | Or (b, b') -> holify ~f holes b %+% holify ~f holes b'
  | Impl (b, b') -> holify ~f holes b %=>% holify ~f holes b'
  | Iff (b, b') -> holify ~f holes b %=>% holify ~f holes b'
  | Neg b -> !%(holify ~f holes b)
