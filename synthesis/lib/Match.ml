open Core
open Util
open Ast
(* open Manip *)

(* TYPES *)
type match_data =
  | Exact of value
  | Between of value * value
  | Mask of value * value

type t = {key : string; data : match_data}

let mk_match key data = {key; data}
let exact_ key v = mk_match key @@ Exact v
let between_ key lo hi =
    if lo = hi then
      exact_ key lo
    else
      mk_match key @@ Between (lo, hi)
let mask_ key v m =
  if Bigint.(get_int m >= max_int (size_of_value v))
  then exact_ key v
  else mk_match key @@ Mask(v,m)

let equal_data d d' =
  match d, d' with
  | Exact v, Exact v' -> veq v v'
  | Between (lo, hi), Between (lo',hi') -> veq lo lo' && veq hi hi'
  | Mask (v,msk), Mask (v',msk') -> veq v v' && veq msk msk'
  | _,_ -> false

let equal m m' = m.key = m'.key && equal_data m.data m'.data


let to_string (m : t) : string =
  match m.data with
  | Exact x ->
     Printf.sprintf "%s ~ %s" m.key (string_of_value x)
  | Between (lo,hi) ->
     Printf.sprintf "%s ~ [%s,%s]" m.key (string_of_value lo) (string_of_value hi)
  | Mask (v, msk) ->
     Printf.sprintf "%s ~ %s & %s" m.key (string_of_value v) (string_of_value msk)

let get_size (m : t) : size =
  match m.data with
  | Exact (Int(_,sz))
  | Mask (Int(_,sz),_)
  | Between (Int(_,sz),_) -> sz


let to_test (m : t) : test =
  let sz = get_size m in
  let k = (m.key, sz) in
  match m.data with
  | Exact v ->
     Var k %=% Value v
  | Between (lo, hi) ->
     let upper_bound = Int(max_int sz,sz) in
     let lo_test =
       if vleq lo (mkInt(0,sz)) then
        True
      else
        Value lo %<=% Var (m.key, sz) in
     let hi_test =
       if vleq upper_bound hi
       then True
       else Var k %<=% Value hi
     in
     lo_test %&% hi_test
  | Mask (v, m) ->
     (mkMask (Var k) (Value m)) %=% Value v

let to_test_hole tbl m =
  match m.data with
  | Exact (Int(_,sz) as v) ->
     Hole (Hole.match_hole_exact tbl m.key,sz) %=% Value(v)
  | Between((Int(_,lsz) as l), (Int(_,hsz) as h)) ->
     assert (lsz = hsz);
     let (loh, hih) = Hole.match_holes_range tbl m.key in
     (Hole(loh,lsz) %=% Value l) %&% (Hole(hih,hsz) %=% Value h)
  | Mask ((Int(_,vsz) as vint), (Int(_,msz) as mask)) ->
     assert (vsz = msz);
     let (vh, mh) = Hole.match_holes_mask tbl m.key in
     (Hole(vh,vsz) %=% Value vint) %&% (Hole(mh,msz) %=% Value mask)

let test_hole_of_lists tbl ms =
  List.fold ms ~init:True
    ~f:(fun acc m ->
      acc %&% to_test_hole tbl m)


let to_valuation_test table hole_typ mtch =
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
         Hole(hm,sz) %=% Value(Int(max_int sz,sz))
       ]
  | _, Between _ ->
     failwith "[Match.valuation] Between Holes are impossible, so cannot create such a valuation"
  | `Exact, Mask _ ->
     failwith "[Match.valuation] Mask valuation is incompatible with exact hole encoding"

let is_wildcard m =
  match m.data with
  | Exact _ -> false
  | Mask (_, m) -> Bigint.(get_int m = zero)
  | Between(lo,Int(hi_v,sz)) ->
     Bigint.(get_int lo = zero
             && (hi_v >= (pow (of_int 2) (of_int sz)) - one))

let list_to_string : t list -> string =
  let list_el_string m = if is_wildcard m then "" else (to_string m ^ " ") in
  List.fold ~init:"" ~f:(fun acc m -> Printf.sprintf "%s%s" acc (list_el_string m))

let list_to_test (matches : t list) =
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
           if String.count addr ~f:((=) ':') = 8 then
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
               Exact(Int(bv,128))
             else
               let mask = Bigint.of_string ("0b" ^ String.make (prefix_len/4) 'f' ^ String.make ((128 - prefix_len) / 4) '0' ) in
               let hi = Bigint.(((bv land mask) + (Bigint.shift_left Bigint.one (Int.(128 - prefix_len))) - Bigint.one)) in
               let lo = Bigint.(bv land mask) in
               Between (Int(lo,128), Int(hi,128))
  in
  {key;data}


let cap (m : t) (m' : t) =
  if String.(m.key <> m'.key) then
    []
  else
    match m.data, m'.data with
    | Exact x, Exact y->
       if veq x y then [m] else []
    | Exact x, Between (lo, hi)
      | Between (lo, hi), Exact x->
       if vleq lo x && vleq x hi then
         [m]
       else
         []
    | Between (lo, hi), Between (lo', hi') ->
       let lo'' = Stdlib.max lo lo' in
       let hi'' = Stdlib.min hi hi' in
       if veq lo'' hi'' then
         [{m with data = Exact lo''}]
       else if vleq lo'' hi'' then
         [{m with data = Between (lo'', hi'')}]
       else
         []
    | Mask _, _ | _, Mask _ ->
     failwith "Dont know how to intersect masks"


let rec has_inter_data (d : match_data) (d' : match_data) : bool =
  match d, d' with
  | Exact x, Exact y
    -> veq x y
  | Exact x, Between (lo, hi)
    | Between(lo,hi), Exact(x)
    -> vleq lo x && vleq x hi

  | Between(lo, hi), Between(lo',hi')
    -> Stdlib.max lo lo' <= Stdlib.min hi hi'

  | Mask(v,msk), Exact(v')
    | Exact (v'), Mask(v,msk)
    -> Bigint.(get_int v land get_int msk = get_int v' land get_int msk)

  | Mask (v,msk),mm | mm, Mask (v,msk)
    -> if Bigint.(get_int msk >= pow (of_int 2) (of_int @@ size_of_value msk) - one)
       then has_inter_data (Exact v) mm
       else failwith "Cant' tell"

let has_inter (m : t) (m' : t) : bool =
  m.key = m'.key
  && (is_wildcard m
      || is_wildcard m'
      || has_inter_data m.data m'.data)

let has_inter_l (ms : t list) (ms' : t list) : bool =
  List.fold2_exn ms ms' ~init:true
    ~f:(fun acc m m' -> acc && has_inter m m')


let is_subset_data (d : match_data) (d' : match_data) : bool =
  match d, d' with
  | Exact i, Exact j -> veq i j
  | Exact i, Between (lo', hi') -> vleq lo' i && vleq i hi'
  | Between (lo, hi), Exact j -> veq lo j && veq hi j
  | Between (lo, hi), Between (lo', hi') -> vleq lo hi' && vleq lo' hi
  | Mask _, _ | _, Mask _ ->
     failwith "Dont know how to subset masks"


let is_subset (m : t) (m': t) : bool =
  m.key = m'.key && (is_wildcard m' || is_subset_data m.data m'.data)


let get_bitmask mtch =
  match mtch.data with
  | Mask(_,m) -> m
  | Exact(Int(_,sz)) -> Int(Bigint.of_string ("0b"^String.make sz '1'), sz)
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
                      if Bigint.(get_int lo = zero)
                      then Exact hi
                      else Exact lo }

let exactify_list = List.map ~f:exactify


let to_model table (m:t) =
  StringMap.of_alist_exn @@
    match m.data with
    | Exact (e) ->
       [Hole.match_hole_exact table m.key, e]
    | Mask (v, msk)->
       let (hv, hmsk) = Hole.match_holes_mask table m.key in
       [hv,v; hmsk, msk]
    | Between (lo, hi) ->
       let hlo, hhi = Hole.match_holes_range table m.key in
       [hlo,lo; hhi,hi]
