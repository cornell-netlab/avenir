open Core

type t =
  | Value of Value.t
  | Var of (string * int)
  | Hole of (string * int)
  | Plus of (t * t)
  | SatPlus of (t * t)
  | SatMinus of (t * t)
  | Times of (t * t)
  | Minus of (t * t)
  | Mask of (t * t)
  | Xor of (t * t)
  | BOr of (t * t)
  | Shl of (t * t)
  | Concat of (t * t)
  | Cast of (int * t)
  | Slice of {hi: int; lo: int; bits: t}
  [@@deriving yojson]

let rec random () : t =
  let random_value () =
    let size = (Random.int 29) + 1 in
    Value.random size in
  match Random.int 15 with
  | 0 -> Value (random_value ())
  | 1 -> Var ("var_name", Random.int 32)
  | 2 -> Hole ("hole_name", Random.int 32)
  | 3 -> Plus (random (), random ())
  | 4 -> SatPlus (random (), random ())
  | 5 -> SatMinus (random (), random ())
  | 6 -> Times (random (), random ())
  | 7 -> Minus (random (), random ())
  | 8 -> Mask (random (), random ())
  | 9 -> Xor (random (), random ())
  | 10 -> BOr (random (), random ())
  | 11 -> Shl (random (), random ())
  | 12 -> Concat (random (), random ())
  | 13 -> Cast (Random.int 32, random ())
  | _ -> Slice { hi = Random.int 32; lo = Random.int 32; bits = random () }

let rec to_string (e : t) : string =
  let string_binop e1 op e2 =
    Printf.sprintf "(%s %s %s)" (to_string e1) op (to_string e2)
  in
  match e with
  | Value v -> Value.to_string v
  | Var (x, s) -> x ^ "#" ^ string_of_int s
  | Hole (x, s) -> "?" ^ x ^ "#" ^ string_of_int s
  | Cast (i, e) -> Printf.sprintf "((<%d>) %s)" i (to_string e)
  | Plus (e, e') -> string_binop e "+" e'
  | SatPlus (e, e') -> string_binop e "|+|" e'
  | SatMinus (e, e') -> string_binop e "|-|" e'
  | Times (e, e') -> string_binop e "*" e'
  | Minus (e, e') -> string_binop e "-" e'
  | Mask (e, e') -> string_binop e "&" e'
  | Xor (e, e') -> string_binop e "^" e'
  | BOr (e, e') -> string_binop e "|" e'
  | Shl (e, e') -> string_binop e "<<" e'
  | Concat (e, e') -> string_binop e "APPEND" e'
  | Slice {hi; lo; bits} -> Printf.sprintf "%s[%d:%d]" (to_string bits) hi lo

let rec to_sexp_string (e : t) =
  let string_binop ctor (e1, e2) =
    Printf.sprintf "%s(%s, %s)" ctor (to_sexp_string e1) (to_sexp_string e2)
  in
  match e with
  | Value v -> "Value(" ^ Value.to_sexp_string v ^ ")"
  | Var (x, s) -> "Var(\"" ^ x ^ "\"," ^ string_of_int s ^ ")"
  | Hole (x, s) -> "Hole(\"" ^ x ^ "\"," ^ string_of_int s ^ ")"
  | Cast (i, e) -> Printf.sprintf "Cast(%d, %s)" i (to_string e)
  | Plus es -> string_binop "Plus" es
  | SatPlus es -> string_binop "SatPlus" es
  | Times es -> string_binop "Times" es
  | Minus es -> string_binop "Minus" es
  | SatMinus es -> string_binop "SatMinus" es
  | Mask es -> string_binop "Mask" es
  | Xor es -> string_binop "Xor" es
  | BOr es -> string_binop "BOr" es
  | Shl es -> string_binop "Shl" es
  | Concat es -> string_binop "Concat" es
  | Slice {hi; lo; bits} ->
      Printf.sprintf "Slice {hi=%d;lo=%d;bits=%s}" hi lo
        (to_sexp_string bits)

let rec equals e1 e2 =
  match (e1, e2) with
  | Value v1, Value v2 -> Value.equals v1 v2
  | Var (x1, sz1), Var (x2, sz2) | Hole (x1, sz1), Hole (x2, sz2) ->
      String.(x1 = x2) && sz1 = sz2
  | Plus (x1, x2), Plus (y1, y2)
   |SatPlus (x1, x2), SatPlus (y1, y2)
   |SatMinus (x1, x2), SatMinus (y1, y2)
   |Minus (x1, x2), Minus (y1, y2)
   |Mask (x1, x2), Mask (y1, y2)
   |Xor (x1, x2), Xor (y1, y2)
   |BOr (x1, x2), BOr (y1, y2)
   |Shl (x1, x2), Shl (y1, y2)
   |Concat (x1, x2), Concat (y1, y2) ->
      equals x1 y1 && equals x2 y2
  | Cast (sz1, e1), Cast (sz2, e2) -> sz1 = sz2 && equals e1 e2
  | Slice s1, Slice s2 ->
      s1.hi = s2.hi && s1.lo = s2.lo && equals s1.bits s2.bits
  | _, _ -> false

let value i = Value (Value.make i)

let cast i e = Cast (i, e)

let plus e e' = Plus (e, e')

let minus e e' = Minus (e, e')

let times e e' = Times (e, e')

let sat_plus e e' = SatPlus (e, e')

let sat_minus e e' = SatMinus (e, e')

let mask e e' = Mask (e, e')

let xor e e' = Xor (e, e')

let or_ e e' = BOr (e, e')

let shl e e' = Shl (e, e')

let concat e e' = Concat (e, e')

let slice hi lo bits = Slice {hi; lo; bits}

let bin_ctor = function
  | Hole _ | Value _ | Var _ | Cast _ | Slice _ ->
      failwith
        "[bin_ctor_for_expr] received hole, value, var, cast, or slice"
  | Plus _ -> plus
  | Times _ -> times
  | Minus _ -> minus
  | SatPlus _ -> sat_plus
  | SatMinus _ -> sat_minus
  | Mask _ -> mask
  | Xor _ -> xor
  | BOr _ -> or_
  | Shl _ -> shl
  | Concat _ -> concat

let un_ctor = function
  | Cast (s, _) -> cast s
  | Slice {hi; lo; _} -> slice hi lo
  | e ->
      Printf.sprintf "Expected slice or cast, recieved %s" (to_string e)
      |> failwith

let rec size (e : t) : int =
  match e with
  | Value v -> Value.size v
  | Var (_, s) -> s
  | Hole (_, s) -> s
  | Cast (i, _) -> i
  | Concat (e, e') -> size e + size e'
  | Plus (e, e')
   |Minus (e, e')
   |Times (e, e')
   |Mask (e, e')
   |Xor (e, e')
   |BOr (e, e')
   |Shl (e, e')
   |SatPlus (e, e')
   |SatMinus (e, e') ->
      let s = size e in
      let s' = size e' in
      if s = s' then s
      else if s = -1 || s' = -1 then -1
      else
        failwith
          (Printf.sprintf
             "size of expressions: %s, and %s differs (%d and %d)"
             (to_string e) (to_string e') s s' )
  | Slice {hi; lo; _} ->
      let sz = hi - lo in
      if sz < 0 then -1 else sz

let rec num_nodes e =
  match e with
  | Value _ | Var _ | Hole _ -> 1
  | Cast (_, e) | Slice {bits= e; _} -> 1 + num_nodes e
  | Plus (e, e')
   |Minus (e, e')
   |Times (e, e')
   |Mask (e, e')
   |Xor (e, e')
   |BOr (e, e')
   |Shl (e, e')
   |Concat (e, e')
   |SatPlus (e, e')
   |SatMinus (e, e') ->
      num_nodes e + num_nodes e' + 1

let bin_eval = function
  | Hole _ | Value _ | Var _ | Cast _ | Slice _ ->
      failwith
        "[bin_ctor_for_expr] received hole, value, var, cast, or slice"
  | Plus _ -> Value.add
  | Times _ -> Value.multiply
  | Minus _ -> Value.subtract
  | SatPlus _ -> Value.sat_add
  | SatMinus _ -> Value.sat_subtract
  | Mask _ -> Value.mask
  | Xor _ -> Value.xor
  | BOr _ -> Value.or_
  | Shl _ -> Value.shl
  | Concat _ -> Value.concat

let un_eval = function
  | Cast (s, _) -> Value.cast s
  | Slice {hi; lo; _} -> Value.slice hi lo
  | e ->
      Printf.sprintf "Expected slice or cast, recieved %s" (to_string e)
      |> failwith

let ord = function
  | Var _ -> 0
  | Hole _ -> 1
  | Cast _ -> 2
  | Plus _ -> 3
  | SatPlus _ -> 4
  | Minus _ -> 5
  | SatMinus _ -> 6
  | Times _ -> 7
  | Value _ -> 8
  | Mask _ -> 9
  | Xor _ -> 10
  | BOr _ -> 11
  | Shl _ -> 12
  | Slice _ -> 13
  | Concat _ -> 14

let rec frees typ e : (string * int) list =
  match (e, typ) with
  | Value _, _ -> []
  | Var (x, sz), `Var | Hole (x, sz), `Hole -> [(x, sz)]
  | Var _, `Hole -> []
  | Hole _, `Var -> []
  | Cast (_, e), _ | Slice {bits= e; _}, _ -> frees typ e
  | Plus (e, e'), _
   |Times (e, e'), _
   |Minus (e, e'), _
   |Mask (e, e'), _
   |Xor (e, e'), _
   |BOr (e, e'), _
   |Shl (e, e'), _
   |Concat (e, e'), _
   |SatPlus (e, e'), _
   |SatMinus (e, e'), _ ->
      frees typ e @ frees typ e'

let vars = frees `Var

let holes = frees `Hole

let rec has_hole = function
  | Value _ | Var _ -> false
  | Hole _ -> true
  | Cast (_, e) | Slice {bits= e; _} -> has_hole e
  | Plus (e1, e2)
   |Minus (e1, e2)
   |Times (e1, e2)
   |Mask (e1, e2)
   |Xor (e1, e2)
   |BOr (e1, e2)
   |Shl (e1, e2)
   |Concat (e1, e2)
   |SatPlus (e1, e2)
   |SatMinus (e1, e2) ->
      has_hole e1 || has_hole e2

let rec multi_vals e : Value.t list =
  match e with
  | Value v -> [v]
  | Var _ | Hole _ -> []
  | Cast (_, e) | Slice {bits= e; _} -> multi_vals e
  | Plus (e, e')
   |Times (e, e')
   |Minus (e, e')
   |Mask (e, e')
   |Xor (e, e')
   |BOr (e, e')
   |Shl (e, e')
   |Concat (e, e')
   |SatPlus (e, e')
   |SatMinus (e, e') ->
      multi_vals e @ multi_vals e'

let rec holify ~f holes (e : t) : t =
  let binop ctor (e, e') = ctor (holify ~f holes e) (holify ~f holes e') in
  match e with
  | Hole _ | Value _ -> e
  | Var (x, sz) -> (
    match List.find holes ~f:(String.( = ) x) with
    | None -> e
    | Some _ -> Hole (f (x, sz)) )
  | Cast (i, e) -> cast i @@ holify ~f holes e
  | Slice {hi; lo; bits} -> slice hi lo @@ holify ~f holes bits
  | Plus es
   |SatPlus es
   |Times es
   |Minus es
   |SatMinus es
   |Mask es
   |Xor es
   |BOr es
   |Shl es
   |Concat es ->
      binop (bin_ctor e) es
