open Core
open Util

(* TYPES *)
type match_data =
  | Exact of Value.t
  | Between of Value.t * Value.t
  | Mask of Value.t * Value.t

type t = {key : string; data : match_data}

let mk_match key data = {key; data}
let exact_ key v = mk_match key @@ Exact v
let between_ key lo hi =
    if Value.equals lo hi then
      exact_ key lo
    else
      mk_match key @@ Between (lo, hi)
let mask_ key v m =
  if Bigint.(Value.get_bigint m >= max_int (Value.size v))
  then exact_ key v
  else mk_match key @@ Mask(v,m)

let wildcard key sz = mask_ key (Value.zero sz) (Value.zero sz)

let equal_data d d' =
  match d, d' with
  | Exact v, Exact v' -> Value.eq v v'
  | Between (lo, hi), Between (lo',hi') -> Value.eq lo lo' && Value.eq hi hi'
  | Mask (v,msk), Mask (v',msk') -> Value.eq v v' && Value.eq msk msk'
  | _,_ -> false

let equal m m' = String.equal m.key m'.key && equal_data m.data m'.data

let get_key m = m.key

let to_string (m : t) : string =
  match m.data with
  | Exact x ->
     Printf.sprintf "%s ~ %s" m.key (Value.to_string x)
  | Between (lo,hi) ->
     Printf.sprintf "%s ~ [%s,%s]" m.key (Value.to_string lo) (Value.to_string hi)
  | Mask (v, msk) ->
     Printf.sprintf "%s ~ %s & %s" m.key (Value.to_string v) (Value.to_string msk)

let to_bmv2_string (m:t) : string =
  match m.data with
  | Exact v ->
     Value.to_bmv2_string v
  | Mask (v,m) ->
     Printf.sprintf "%s &&& %s" (Value.to_bmv2_string v) (Value.to_bmv2_string m)
  | _ ->
     Printf.sprintf "[Unimplemented] Don't know how to install range matches"
     |> failwith

let get_size (m : t) : int =
  match m.data with
  | Exact v
  | Mask (v,_)
  | Between (v,_) -> Value.size v


let to_test (m : t) : Test.t =
  let open Test in
  let sz = get_size m in
  let k = (m.key, sz) in
  match m.data with
  | Exact v ->
     Var k %=% Value v
  | Between (lo, hi) ->
     let upper_bound = Value.big_make (max_int sz,sz) in
     let lo_test =
       if Value.leq lo (Value.zero sz) then
        True
      else
        Value lo %<=% Var (m.key, sz) in
     let hi_test =
       if Value.leq upper_bound hi
       then True
       else Var k %<=% Value hi
     in
     lo_test %&% hi_test
  | Mask (v, m) ->
     Expr.((mask (Var k) (Value m)) %=% Value v)

(**Assume match is well-typed for tbl*)
let to_test_hole tbl m =
  let open Test in
  match m.data with
  | Exact v ->
     Hole (Hole.match_hole_exact tbl m.key,Value.size v) %=% Value v
  | Between (l, h) ->
     let sz = Value.size l in
     let (loh, hih) = Hole.match_holes_range tbl m.key in
     (Hole(loh, sz) %=% Value l) %&% (Hole(hih, sz) %=% Value h)
  | Mask (vint, mask) ->
     let (vh, mh) = Hole.match_holes_mask tbl m.key in
     let sz = Value.size vint in
     (Hole(vh,sz) %=% Value vint) %&% (Hole(mh,sz) %=% Value mask)

let to_model_alist tbl m =
  match m.data with
  | Exact v ->
     [Hole.match_hole_exact tbl m.key, v]
  | Between  (l, h) ->
     let (loh, hih) = Hole.match_holes_range tbl m.key in
     [loh, l; hih, h]
  | Mask (vint, mask) ->
     let (vh, mh) = Hole.match_holes_mask tbl m.key in
     [vh, vint; mh, mask]

let test_hole_of_lists tbl ms =
  let open Test in
  List.fold ms ~init:True
    ~f:(fun acc m ->
      acc %&% to_test_hole tbl m)

let list_to_model_alist tbl ms = List.bind ms ~f:(to_model_alist tbl)


let to_model ?typ:(typ=`Vals) table (m : t) =
  let encode_msk (v,msk) (hv,hmsk) =
    let model = [ (*Hole.add_row_hole_name table, mkInt(1,1);*) hmsk, msk] in
       begin match typ  with
       | `Vals ->
          (hv, v) :: model
       | `NoVals ->
          model
       end
  in
  Model.of_alist_exn @@
    match m.data with
    | Exact (v) ->
       let sz = Value.size v in
       let msk = Value.big_make (max_int sz, sz) in
       Hole.match_holes_mask table m.key
       |> encode_msk (v,msk)
    | Mask (v,msk) ->
       Hole.match_holes_mask table m.key
       |> encode_msk (v,msk)
    | Between (lo, hi) ->
       let hlo, hhi = Hole.match_holes_range table m.key in
       [hlo,lo; hhi,hi]


let list_to_model tbl ms =
  list_to_model_alist tbl ms
  |> Model.of_alist_exn


let to_valuation_test table hole_typ mtch =
  let open Test in
  let k = mtch.key in
  let sz = get_size mtch in
  match hole_typ, mtch.data with
  | `Exact, Exact v ->
     let h = Hole.match_hole_exact table k in
     (Hole(h,sz) %=% Value v)
  | `Mask, Mask(v,m) ->
     let hv,hm = Hole.match_holes_mask table k in
     bigand [
         Hole(hv, sz) %=% Value v;
         Hole(hm, sz) %=% Value m
       ]
  | `Mask, Exact(v) ->
     let hv, hm = Hole.match_holes_mask table k in
     bigand [
         Hole(hv,sz) %=% Value v;
         Hole(hm,sz) %=% Value(Value.big_make(max_int sz,sz))
       ]
  | _, Between _ ->
     failwith "[Match.valuation] Between Holes are impossible, so cannot create such a valuation"
  | `Exact, Mask _ ->
     failwith "[Match.valuation] Mask valuation is incompatible with exact hole encoding"

let is_wildcard m =
  match m.data with
  | Exact _ ->
     false
  | Mask (_, m) ->
     Bigint.(Value.get_bigint m = zero)
  | Between(lo,hi) ->
     let sz = Value.size hi in
     Bigint.(Value.get_bigint lo <= zero
             && Value.get_bigint hi >= max_int sz)

let list_to_string : t list -> string =
  let list_el_string m = if is_wildcard m then "" else (to_string m ^ " ") in
  List.fold ~init:"" ~f:(fun acc m -> Printf.sprintf "%s%s" acc (list_el_string m))

let list_to_test (matches : t list) =
  let open Test in
  List.fold matches ~init:True ~f:(fun acc m -> acc %&% to_test m)


let mk_ipv6_match key ipv6_str =
  let (addr_str, prefix_len) =
    match String.lsplit2 ipv6_str ~on:'/' with
    | None -> (ipv6_str, 128)
    | Some (addr, len) -> (addr, int_of_string len) in
  let hex_addr =
    let exp_addr_str =
      match String.substr_index addr_str ~pattern:"::" with
      | None -> addr_str
      | Some _ ->
         let rec loop addr =
           if String.count addr ~f:(Char.(=) ':') = 8 then
             String.substr_replace_all addr  ~pattern:"::" ~with_:":"
           else
             String.substr_replace_all addr ~pattern:("::") ~with_:":0000::"
             |> loop
         in
         loop addr_str
         |> String.split ~on:':'
         |> List.fold ~init:"" ~f:(fun acc str ->
                Printf.sprintf "%s:%s" acc (lfill '0' 4 str))
    in
    String.substr_replace_all exp_addr_str ~pattern:":" ~with_:""
  in
  let bv = Bigint.of_string ("0x" ^ hex_addr) in
  let data = if prefix_len = 128 then
               Exact(Value.big_make (bv,128))
             else
               let mask = Bigint.of_string ("0b" ^ String.make (prefix_len/4) 'f' ^ String.make ((128 - prefix_len) / 4) '0' ) in
               let hi = Bigint.(((bv land mask) + (Bigint.shift_left Bigint.one (Int.(128 - prefix_len))) - Bigint.one)) in
               let lo = Bigint.(bv land mask) in
               Between (Value.big_make(lo,128), Value.big_make (hi,128))
  in
  {key;data}


let cap (m : t) (m' : t) =
  if String.(m.key <> m'.key) then
    []
  else
    match m.data, m'.data with
    | Exact x, Exact y->
       if Value.eq x y then [m] else []
    | Exact x, Between (lo, hi)
      | Between (lo, hi), Exact x->
       if Value.leq lo x && Value.leq x hi then
         [m]
       else
         []
    | Between (lo, hi), Between (lo', hi') ->
       let lo'' = Stdlib.max lo lo' in
       let hi'' = Stdlib.min hi hi' in
       if Value.leq lo'' hi'' then
         [{m with data = Exact lo''}]
       else if Value.leq lo'' hi'' then
         [{m with data = Between (lo'', hi'')}]
       else
         []
    | Mask _, _ | _, Mask _ ->
     failwith "Dont know how to intersect masks"

let rec has_inter_data (d : match_data) (d' : match_data) : bool =
  match d, d' with
  | Exact x, Exact y ->
     Value.eq x y
  | Exact x, Between (lo, hi)
    | Between(lo,hi), Exact(x) ->
    Value.leq lo x && Value.leq x hi
  | Between(lo, hi), Between(lo',hi') ->
    Value.leq (Stdlib.max lo lo') (Stdlib.min hi hi')
  | Mask(v,msk), Exact(v')
    | Exact (v'), Mask(v,msk)
    -> Bigint.(Value.get_bigint v land Value.get_bigint msk = Value.get_bigint v' land Value.get_bigint msk)

  | Mask (v,msk),mm | mm, Mask (v,msk)
    -> if Bigint.(Value.get_bigint msk >= pow (of_int 2) (of_int @@ Value.size msk) - one)
       then has_inter_data (Exact v) mm
       else failwith "Cant' tell"

let has_inter (m : t) (m' : t) : bool =
  String.(m.key = m'.key)
  && (is_wildcard m
      || is_wildcard m'
      || has_inter_data m.data m'.data)

let has_inter_l (ms : t list) (ms' : t list) : bool =
  List.fold2_exn ms ms' ~init:true
    ~f:(fun acc m m' -> acc && has_inter m m')


let is_subset_data (d : match_data) (d' : match_data) : bool =
  match d, d' with
  | Exact i, Exact j -> Value.eq i j
  | Exact i, Between (lo', hi') -> Value.leq lo' i && Value.leq i hi'
  | Between (lo, hi), Exact j -> Value.leq lo j && Value.leq hi j
  | Between (lo, hi), Between (lo', hi') -> Value.leq lo hi' && Value.leq lo' hi
  | Mask _, _ | _, Mask _ ->
     failwith "Dont know how to subset masks"


let is_subset (m : t) (m': t) : bool =
  String.(m.key = m'.key) && (is_wildcard m' || is_subset_data m.data m'.data)


let get_bitmask mtch =
  match mtch.data with
  | Mask(_,m) -> m
  | Exact(v) ->
     let sz = Value.size v in
     Value.big_make (Bigint.of_string ("0b"^String.make sz '1'), sz)
  | Between(_,_) -> failwith "cannot get bitmask of a range"


let get_exact_val mtch =
  match mtch.data with
  | Exact v -> v
  | _ -> failwith "called [Match.get_exact_val], but it wasn't a value"

let get_base_value mtch =
  match mtch.data with
  | Exact base -> base
  | Mask (base,_) -> base
  | _ -> failwith "[Match.get_base_value] cannot get base value of range "


let exactify (m : t) : t =
  if is_wildcard m then
    m
  else
    {m with data = match m.data with
                   | Mask (v,_) | Exact (v) -> Exact v
                   | Between (lo,hi) ->
                      if Bigint.(Value.get_bigint lo = zero)
                      then Exact hi
                      else Exact lo }

let exactify_list = List.map ~f:exactify

let relevant_matches = List.filter ~f:(Fn.non is_wildcard)
let relevant_keys = Fn.compose (List.map ~f:get_key) relevant_matches


let hits mtch v =
  match mtch.data with
  | Exact v' ->
     Value.eq v v'
  | Mask (v',msk) ->
     Value.eq v (Value.mask v' msk)
  | Between (lo,hi) ->
     Value.leq lo v && Value.leq v hi
