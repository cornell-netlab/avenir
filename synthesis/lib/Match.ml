open Core
open Util
open Ast
(* open Manip *)

(* TYPES *)
type t =
  | Exact of value
  | Between of value * value
  | Mask of value * value

let exact_ v = Exact v
let between_ lo hi = if lo = hi then exact_ lo else Between (lo, hi)
let mask_ v m = if Bigint.(get_int m >= max_int (size_of_value v)) then exact_ v else Mask(v,m)

let equal m m' =
  match m, m' with
  | Exact v, Exact v' -> veq v v'
  | Between (lo, hi), Between (lo',hi') -> veq lo lo' && veq hi hi'
  | Mask (v,msk), Mask (v',msk') -> veq v v' && veq msk msk'
  | _,_ -> false


let to_string m =
  match m with
  | Exact x -> string_of_value x
  | Between (lo,hi) -> Printf.sprintf "[%s,%s]" (string_of_value lo) (string_of_value hi)
  | Mask ((Int(i,sz) as v), (Int(j,_) as m)) ->
     if Bigint.(i = zero && j = zero ) then
       Printf.sprintf "*#%d" sz
     else
       Printf.sprintf "%s & %s" (string_of_value v) (string_of_value m)

let to_test k m =
  match m with
  | Exact x -> Var k %=% Value(x)
  | Between (Int(lo,sz), Int(hi,sz')) when sz = sz'
    -> (if lo <= Bigint.zero then
          True
        else
          Value(Int(lo,sz)) %<=% Var k)
       %&% (if Bigint.(hi >= (pow (of_int 2) (of_int sz) - one))
            then True
            else(Var k %<=% Value(Int(hi, sz))))
  | Between (Int(lo,sz), Int(hi,sz')) ->
     Printf.sprintf "Type error, %s#%d and %s#%d are of different sizes"
       (Bigint.to_string lo) sz (Bigint.to_string hi) sz'
     |> failwith
  | Mask (v, m) ->
     (Ast.mkMask (Var k) (Value m)) %=% Value v

let to_test_hole tbl k m =
  match m with
  | Exact (Int(_,sz) as v) ->
     Hole (Hole.match_hole_exact tbl k,sz) %=% Value(v)
  | Between((Int(_,lsz) as l), (Int(_,hsz) as h)) ->
     assert (lsz = hsz);
     let (loh, hih) = Hole.match_holes_range tbl k in
     (Hole(loh,lsz) %=% Value l) %&% (Hole(hih,hsz) %=% Value h)
  | Mask ((Int(_,vsz) as vint), (Int(_,msz) as mask)) ->
     assert (vsz = msz);
     let (vh, mh) = Hole.match_holes_mask tbl k in
     (Hole(vh,vsz) %=% Value vint) %&% (Hole(mh,msz) %=% Value mask)

let test_hole_of_lists tbl ks ms =
  List.fold2_exn ks ms ~init:True
    ~f:(fun acc (k,_,v_opt) m ->
      acc %&% match v_opt with
              | None -> to_test_hole tbl k m
              | Some _ -> True)


let to_valuation_test table (k,sz) hole_typ mtch =
  match hole_typ, mtch with
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


let list_to_string : t list -> string =
  List.fold ~init:"" ~f:(fun acc m -> Printf.sprintf "%s %s" acc (to_string m))

let list_to_test (keys : (string * size * value option) list) (matches : t list) =
  List.fold2_exn (free_keys keys) matches ~init:True ~f:(fun acc k m -> acc %&% to_test k m)


let mk_ipv6_match ipv6_str =
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
  if prefix_len = 128 then
    Exact(Int(bv,128))
  else
    let mask = Bigint.of_string ("0b" ^ String.make (prefix_len/4) 'f' ^ String.make ((128 - prefix_len) / 4) '0' ) in
    let hi = Bigint.(((bv land mask) + (Bigint.shift_left Bigint.one (Int.(128 - prefix_len))) - Bigint.one)) in
    let lo = Bigint.(bv land mask) in
    Between (Int(lo,128), Int(hi,128))

let is_wildcard = function
  | Exact _ -> false
  | Mask (_, m) -> Bigint.(get_int m = zero)
  | Between(lo,Int(hi_v,sz)) ->
     Bigint.(get_int lo = zero
             && (hi_v >= (pow (of_int 2) (of_int sz)) - one))

let cap (m : t) (m' : t) =
  match m, m' with
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
       [Exact lo'']
     else if vleq lo'' hi'' then
       [Between (lo'', hi'')]
     else
       []
  | Mask _, _ | _, Mask _ ->
     failwith "Dont know how to intersect masks"



let rec has_inter (m : t) (m' : t) : bool =
  if is_wildcard m || is_wildcard m' then true else
    match m, m' with
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
         then has_inter (Exact v) mm
         else failwith "Cant' tell"

let has_inter_l (ms : t list) (ms' : t list) : bool =
  (* if ms = [] && ms' = [] then false
   * else *)
  List.fold2_exn ms ms' ~init:true
    ~f:(fun acc m m' -> acc && has_inter m m')


let is_subset (m : t) (m': t) : bool =
  match m, m' with
  | Exact i, Exact j -> veq i j
  | Exact i, Between (lo', hi') -> vleq lo' i && vleq i hi'
  | Between (lo, hi), Exact j -> veq lo j && veq hi j
  | Between (lo, hi), Between (lo', hi') -> vleq lo hi' && vleq lo' hi
  | Mask _, _ | _, Mask _ ->
     failwith "Dont know how to subset masks"


let get_bitmask = function
  | Mask(_,m) -> m
  | Exact(Int(_,sz)) -> Int(Bigint.of_string ("0b"^String.make sz '1'), sz)
  | Between(_,_) -> failwith "cannot get bitmask of a range"


let get_exact_val = function
  | Exact v -> v
  | _ -> failwith "called [Match.get_exact_val], but it wasn't a value"

let get_base_value = function
  | Exact base -> base
  | Mask (base,_) -> base
  | _ -> failwith "[Match.get_base_value] cannot get base value of range "


let exactify (m : t) : t =
  if is_wildcard m then
    m
  else
    match m with
    | Mask (v,_) | Exact (v) -> Exact v
    | Between (lo,hi) ->
       if Bigint.(get_int lo = zero)
       then Exact hi
       else Exact lo

let exactify_list = List.map ~f:exactify
