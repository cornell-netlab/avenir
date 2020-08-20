open Core
open Util
open Ast


let join m m' : expr StringMap.t =
  StringMap.merge m m'
    ~f:(fun ~key:_ case ->
      match case with
      | `Both (l,r) when l = r -> Some l
      | `Both (_,_) -> None
      | `Left  l -> Some l
      | `Right r -> Some r)

let rec eval_const_expr e =
  let binop op (e1, e2) =
    match eval_const_expr e1, eval_const_expr e2 with
    | Some e1', Some e2' -> Some (op e1' e2')
    | _,_ -> None
  in
  match e with
  | Value v -> Some (v)
  | Var _ | Hole _ -> None
  | Plus es -> binop add_values es
  | Times es -> binop multiply_values es
  | Minus es -> binop subtract_values es
  | Mask es -> binop mask_values es
  | Xor es -> binop xor_values es
  | BOr es -> binop or_values es
  | Shl es -> binop shl_values es
  | Concat es -> binop concat_values es
  | Cast (i, e') ->
     let open Option in
     eval_const_expr e' >>| cast_value i
  | Slice {hi;lo;bits} ->
     let open Option in
     eval_const_expr bits >>| slice_value hi lo

let rec propogate_expr (map : expr StringMap.t) (e : expr) =
  let eval_if_const e =
    let open Option in
    (eval_const_expr e >>| fun v -> Value v)
    |> value ~default:e
  in
  let binop ctor (e1,e2) =
    let e' = ctor (propogate_expr map e1) (propogate_expr map e2) in
    eval_if_const e'
  in
  let unop ctor e =
    eval_if_const @@ ctor @@ propogate_expr map e
  in
  match e with
  | Value _ | Hole _ -> e
  | Var (x,_) ->
     StringMap.find map x
     |> Option.value ~default:e
  | Plus es
  | Times es
  | Minus es
  | Mask es
  | Xor es
  | BOr es
  | Shl es
  | Concat es ->
     binop (ctor_for_binexpr e) es
  | Cast (i, e') ->
     unop (mkCast i) e'
  | Slice {hi;lo;bits} ->
     unop (mkSlice hi lo) bits

let rec propogate_test (map : expr StringMap.t) (t : test) =
  let tbinop ctor (t1,t2) =
    ctor (propogate_test map t1) (propogate_test map t2)
  in
  let ebinop ctor (e1,e2) = ctor (propogate_expr map e1) (propogate_expr map e2) in
  let unop ctor t = ctor @@ propogate_test map t in
  match t with
  | True | False -> t
  | Eq es -> ebinop mkEq es
  | Le es -> ebinop mkLe es
  | And ts -> tbinop mkAnd ts
  | Or ts -> tbinop mkOr ts
  | Impl ts -> tbinop mkImplies ts
  | Iff ts -> tbinop mkIff ts
  | Neg t -> unop mkNeg t

let rec propogate_cmd (map : expr StringMap.t) (cmd : cmd) =
  match cmd with
  | Skip -> map, Skip
  | Assign (f,e) ->
     let e' = propogate_expr map e in
     let map' = StringMap.set map ~key:f ~data:e' in
     map', f %<-% e'
  | Seq (c1,c2) ->
     let map',c1' = propogate_cmd map c1 in
     let map'', c2' = propogate_cmd map' c2 in
     map'', c1' %:% c2'
  | Assert t -> map, Assert (propogate_test map t)
  | Assume t -> map, Assume (propogate_test map t)
  | Select (typ, cases) ->
     let map', cases',_ =
       List.fold cases ~init:(StringMap.empty, [], false)
         ~f:(fun (accMap, cases, _) (b,c) ->
             let b' = propogate_test map b in
             let map',c' = propogate_cmd map c in
             join accMap map', cases @ [b', c'], (b' = True && typ = Ordered)
         )
     in
     map', mkSelect typ cases'

  | Apply _ -> failwith "unsupported"
  | While _ -> failwith "unsupported"


let propogate cmd =
  let f cmd = propogate_cmd StringMap.empty cmd |> snd in
  let rec fix cmd_last cmd =
    if cmd_last = cmd
    then cmd
    else fix cmd (f cmd)
  in
  fix Skip cmd
