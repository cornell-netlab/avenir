open Core
open Ast
open Util

let namegen = NameGen.make ()

let disable = true

let rec shorten_expr (bht : Bishtbl.t) (e : expr) : expr =
  if disable  then e else
    let get key = Bishtbl.get bht ~key ~default:(NameGen.get_fresh_name namegen) in
    let unop ?(mk=Fn.id) e = mk (shorten_expr bht e) in
    let binop mk (e1,e2) = mk (unop e1) (unop e2) in
    match e with
    | Value _ -> e
    | Var (x, sz) -> Var(get x, sz)
    | Hole (h, sz) -> Hole(get h, sz)
    | Cast (i,e) -> unop ~mk:(mkCast i) e
    | Slice {hi;lo;bits} -> unop ~mk:(mkSlice hi lo) bits
    | Plus es | Times es | Minus es | Mask es | Xor es | BOr es | Shl es | Concat es | SatPlus es | SatMinus es
      -> binop (ctor_for_binexpr e) es

let rec shorten (bht : Bishtbl.t) (t : test) : test =
  if disable then t else
    match t with
    | True -> True
    | False -> False
    | Eq (e1,e2) -> shorten_expr bht e1 %=% shorten_expr bht e2
    | Le (e1,e2) -> shorten_expr bht e1 %<=% shorten_expr bht e2
    | And (t1,t2) -> shorten bht t1 %&% shorten bht t2
    | Or (t1,t2) -> shorten bht t1 %+% shorten bht t2
    | Iff (t1,t2) -> shorten bht t1 %<=>% shorten bht t2
    | Impl (t1,t2) -> shorten bht t1 %=>% shorten bht t2
    | Neg t1 -> mkNeg @@ shorten bht t1

let rec unshorten_expr (bht : Bishtbl.t) (e : expr) : expr =
  if disable then e else
    let unget key = Bishtbl.get_back bht ~key in
    let unop ?(mk=Fn.id) e = mk (unshorten_expr bht e) in
    let binop mk (e1,e2) = mk (unop e1) (unop e2) in
    match e with
    | Value _ -> e
    | Var (x, sz) -> Var(unget x, sz)
    | Hole (h, sz) -> Hole(unget h, sz)
    | Cast (i,e) -> unop ~mk:(mkCast i) e
    | Slice {hi;lo;bits} -> unop ~mk:(mkSlice hi lo) bits
    | Plus es | Times es | Minus es | Mask es | Xor es | BOr es | Shl es | Concat es | SatPlus es | SatMinus es
      -> binop (ctor_for_binexpr e) es

let rec unshorten (bht : Bishtbl.t) (t : test) : test =
  if disable then t else
    match t with
    | True -> True
    | False -> False
    | Eq (e1,e2) -> unshorten_expr bht e1 %=% unshorten_expr bht e2
    | Le (e1,e2) -> unshorten_expr bht e1 %<=% unshorten_expr bht e2
    | And (t1,t2) -> unshorten bht t1 %&% unshorten bht t2
    | Or (t1,t2) -> unshorten bht t1 %+% unshorten bht t2
    | Iff (t1,t2) -> unshorten bht t1 %<=>% unshorten bht t2
    | Impl (t1,t2) -> unshorten bht t1 %=>% unshorten bht t2
    | Neg t1 -> mkNeg @@ unshorten bht t1


let unshorten_model (bht : Bishtbl.t) (m : Model.t) : Model.t =
  if disable then m else
    Model.fold m ~init:Model.empty
      ~f:(fun ~key ~data acc ->
        Model.set acc ~key:(Bishtbl.get_back bht ~key) ~data
      )

let unshorten_packet (bht : Bishtbl.t) (m : value StringMap.t) : value StringMap.t =
  if disable then m else
    StringMap.fold m ~init:StringMap.empty
      ~f:(fun ~key ~data acc ->
        StringMap.set acc ~key:(Bishtbl.get_back bht ~key) ~data
      )
