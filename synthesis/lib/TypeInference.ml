open Core
open Ast


let rec relabel (e : Expr.t) (sz : size) : Expr.t =
  let open Expr in
  let binop mk (e,e') = mk (relabel e sz) (relabel e' sz) in
  match e with
  | Value v when Value.size v >= 0 && Value.size v <> sz ->
     failwith @@ Printf.sprintf
                   "Tried to relabel %s with #%d"
                   (Value.to_string v) sz
  |Var (_,sz')
    | Hole (_,sz') when sz' >= 0 && sz <> sz' ->
     failwith @@ Printf.sprintf "Tried to relabel %s to width %d" (Expr.to_string e) sz
  | Value v -> Value(Value.resize v sz)
  | Var (x, _) -> Var(x, sz)
  | Hole(h,_) -> Hole(h, sz)
  | Cast (i,_) -> if i = sz then e else failwith "Tried to relabel an explicit cast incorrectly:("
  | Slice {hi;lo;_} -> let sz' = hi - lo in
                       if sz = sz' then e else failwith "Tried to incorrectly relabel a slice"
  | Concat _ -> e
  | Plus es | Times es | Minus es | Mask es | Xor es | BOr es | Shl es | SatPlus es | SatMinus es
    -> binop (bin_ctor e) es

let rec infer_expr (e : Expr.t) : Expr.t =
  let open Expr in
  let binop f (e1,e2) =
    let s1 = size e1 in
    let s2 = size e2 in
    assert (s1 >= -1);
    assert (s2 >= -1);
    if s1 = s2
    then if s1 = -1
         then f (infer_expr e1) (infer_expr e2)
         else e
    else
      if s1 = -1
      then f (relabel e1 s2) e2
      else
        if s2 = -1
        then f e1 (relabel e2 s1)
        else begin Printf.printf "sizes are known and differ!\n\t%s\t%s\n%!" (Expr.to_string e1) (Expr.to_string e2);
                   failwith "BitWidth type error"
             end
  in
  match e with
  | Value _
    | Var _
    | Hole _ -> e
  | Cast (i,e) -> cast i @@ infer_expr e
  | Slice {hi;lo;bits} -> slice hi lo @@ infer_expr bits
  | Concat (e,e') -> concat (infer_expr e) (infer_expr e')
  | Plus es | Times es | Minus es | Mask es | Xor es | BOr es | Shl es | SatPlus es | SatMinus es
    -> binop (bin_ctor e) es

let rec infer_test t =
  let open Test in
  let binop_e op e1 e2 =
    let e1' = infer_expr e1 in
    let e2' = infer_expr e2 in
    let sz1 = Expr.size e1' in
    let sz2 = Expr.size e2' in
    if sz1 = -1 && sz2 >= 0 then
      op (relabel e1' sz2) e2'
    else if sz1 >= 0 && sz2 = -1 then
      op e1' (relabel e2' sz1)
    else if sz1 = sz2 then
      op e1' e2'
    else
      failwith @@ Printf.sprintf "[TypeError] %s and %s have different bitwidths" (Expr.to_string e1) (Expr.to_string e2)
  in
  match t with
  | True -> True
  | False -> False
  | Eq (e1,e2) -> binop_e (%=%) e1 e2
  | Le (e1,e2) -> binop_e (%<=%) e1 e2
  | And (b1,b2) -> infer_test b1 %&% infer_test b2
  | Or (b1,b2) -> infer_test b1 %+% infer_test b2
  | Impl (b1,b2) -> infer_test b1 %=>% infer_test b2
  | Iff (b1,b2) -> infer_test b1 %<=>% infer_test b2
  | Neg b -> !%(infer_test b)

let rec infer (p : cmd) : cmd =
  match p with
  | Skip -> Skip
  | Assign (s, e) -> s %<-% infer_expr e
  | Assume t -> Assume (infer_test t)
  | Seq (c1,c2) -> infer c1 %:% infer c2
  | Select (typ, bs) ->
     mkSelect typ @@
       List.map bs ~f:(fun (t,c) -> infer_test t, infer c)
  | Apply t ->
     let actions' = List.map t.actions ~f:(infer_action) in
     Apply {t with actions = actions'}

and infer_action (n, params,c) = (n, params,infer c)
