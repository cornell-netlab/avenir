open Core
open Ast
open Util

module Field = struct

  (** The order of the constructors defines the default variable ordering and has a massive
      performance impact. Do not change unless you know what you are doing. *)
  type t
    =
    | IP4Src
    | IP4Dst
    | TCPSrcPort
    | TCPDstPort
    | Vlan
    | VlanPcp
    | EthType
    | IPProto
    | EthSrc
    | EthDst
    | OutPort
    | InPort
[@@deriving sexp, enumerate, enum, hash]
  type field = t

  let num_fields = max + 1

  let of_string s =
    Sexp.of_string s |> t_of_sexp

  let to_string t =
    sexp_of_t t |> Sexp.to_string

  let is_valid_order (lst : t list) : bool =
    Set.Poly.(equal (of_list lst) (of_list all))

  let order = Array.init num_fields ~f:ident

  let set_order (lst : t list) : unit =
    assert (is_valid_order lst);
    List.iteri lst ~f:(fun i fld -> order.(to_enum fld) <- i)

  (* Not a clean way to invert a permutation, but fast *)
  let invert arr =
    let inverted = Array.init num_fields ~f:ident in
    Array.iteri arr ~f:(fun i elt -> inverted.(elt) <- i );
    inverted

  let get_order () =
    Array.to_list (invert order)
    |> List.filter_map ~f:of_enum

  (* compare depends on current order! *)
  let compare (x : t) (y : t) : int =
    (* using Obj.magic instead of to_enum for bettter performance *)
    Int.compare order.(Obj.magic x) order.(Obj.magic y)

  let equal x y = x = y

end

module Value = struct

  type t
    = Const of Int64.t
    | Mask of Int64.t * int
    [@@deriving sexp, hash]

    
  (* subseq_eq, meet and join are defined to make this fit interface of Vlr.Lattice *)
  let subset_eq a b =
    (* Note that Mask checking is a lot like OpenFlow.Pattern.Ip, but the int's are different sizes *)
    let subset_eq_mask a m b n =
      if m < n
        then false
        else
          Int64.shift_right_logical a (64-n) = Int64.shift_right_logical b (64-n)
    in
    match a, b with
    | Const  a           , Const b
    (* Note that comparing a mask to a constant requires the mask to be all 64 bits, otherwise they fail the lesser mask test *)
    | Mask(a, 64)        , Const b -> a = b
    | Mask             _ , Const            _
      -> false
    | Mask(a, m)         , Mask(b, n) -> subset_eq_mask a m  b n
    | Const a            , Mask(b, n) -> subset_eq_mask a 64 b n
                                                        
  let meet ?(tight=false) a b =
    let meet_mask a m b n =
      let lt = subset_eq (Mask(a, m)) (Mask(b, n)) in
      let gt = subset_eq (Mask(b, n)) (Mask(a, m)) in
      if lt && gt then
        Some(Mask(a, m))
      else if lt then
        if (not tight) || (m = (n + 1)) then Some(Mask(a, m)) else None
      else if gt then
        if (not tight) || (n = (m + 1)) then Some(Mask(b, n)) else None
      else
        None
    in
    match a, b with
    | Const  a   , Const b
    | Mask(a, 64), Const b -> if a = b then Some(Const a) else None
    | Mask     _ , Const _ -> None
    | Mask(a, m) , Mask(b, n) -> meet_mask a m  b n
    | Const a, Mask(b, n)     -> meet_mask a 64 b n

  let join ?(tight=false) a b =
    (* The intent here looks a lot like OpenFlow.Pattern.Ip.join, but the notion of "tightness" might not
       not apply.  Look at perhaps sharing the logic between the two, abstracting out bit length since this deals with
       64 bit ints *)
    let join_mask a m b n =
      let lt = subset_eq (Mask(a, m)) (Mask(b, n)) in
      let gt = subset_eq (Mask(b, n)) (Mask(a, m)) in
      if lt && gt then
        Some(Mask(a, m))
      else if lt then
        if (not tight) || (n = (m - 1)) then Some(Mask(b, n)) else None
      else if gt then
        if (not tight) || (m = (n - 1)) then Some(Mask(a, m)) else None
      else
        if (not tight) || m = n then
          let x, y = (Mask(a, m - 1), Mask(b, n - 1)) in
          if subset_eq x y && subset_eq y x then Some(x) else None
        else
          None (* XXX(seliopou): complete definition *)
    in
    match a, b with
    | Const  a   , Const b
    | Mask(a, 64), Const b -> if a = b then Some(Const a) else None
    | Mask     _ , Const _ -> None
    | Mask(a, m) , Mask(b, n) -> join_mask a m  b n
    | Const a, Mask(b, n)     -> join_mask a 64 b n

  (* Value compare is used in Pattern below, but is not public *)
  let compare x y = match (x, y) with
    | Const a, Mask (b, 64)
    | Mask (a, 64), Const b
    | Const a, Const b -> Int64.compare a b
    | Mask(a, m) , Mask(b, n) ->
      let shift = 64 - min m n in
      (match Int64.(compare (shift_right a shift) (shift_right b shift)) with
       | 0 -> Int.compare n m
       | c -> c)
    | Const _ , _ -> -1
    | _, Const _ -> 1

  let equal x y = compare x y = 0

  let to_string = function
    | Const(a)   -> Printf.sprintf "%Lu" a
    | Mask(a, m) -> Printf.sprintf "%Lu/%d" a m

                                   
  let is_top (m:t) : bool =
    (* Printf.printf "CHECKING TOP of %s\n" (to_string m); *)
    match m with
    | Mask(_,0) -> true
    |  _ -> false

  let of_int   t = Const (Int64.of_int   t)
  (* Private to this file only *)
  let of_int32 t = Const (Int64.of_int32 t)
  let of_int64 t = Const t
  let to_int64_exn = function
    | Const k -> k
    | _ -> assert false
end

exception FieldValue_mismatch of Field.t * Value.t

module Pattern = struct
  type t = Field.t * Value.t
                       [@@deriving compare]

  let to_string (f, v) =
    Printf.sprintf "%s = %s" (Field.to_string f) (Value.to_string v)

  let equal a b =
    compare a b = 0

  let to_int = Int64.to_int_exn
  let to_int32 = Int64.to_int32_exn

end


module Action = struct

  type t =
    | Act of int list
    | Contra [@@deriving sexp, compare, hash, eq]

  let to_string a =
    match a with
    | Act [] -> "skip"
    | Act i -> List.fold i ~init:"[" ~f:(Printf.sprintf "%s %d") ^" ]"
    | Contra -> "drop"
        

  let one = Act []

  let zero = Contra

  let prod a b =
    match a, b with
    | _, Contra | Contra, _ -> Contra
    | _, Act [] -> a
    | _, _ -> b
                                               

  let sum a b =
    let ab = match a, b with
      | Act [], _ | _, Act [] -> one
      | Contra, x | x, Contra -> x
      | Act i, Act j -> Act (List.dedup_and_sort ~compare:Int.compare (i@j))
    in
    (* Printf.printf "[Action.sum] %s + %s = %s\n%!"
     *   (a |> to_string) (b |> to_string) (ab |> to_string); *)
    ab

           
end
              
include Vlr.Make
          (Field)
          (Value)
          (Action)


let create_from_rows keys rows =
  let rec create_from_row (ms,a) neg =
    List.fold2_exn keys ms ~init:(mk_leaf (Act a))
      ~f:(fun pos k m ->
        (* Printf.printf "if %s ~ %s \nthen %d %s \nelse %d %s\n\n%!"
         *   (Field.to_string k)
         *   (Value.to_string m)
         *   pos (to_string pos)
         *   neg (to_string neg); *)
        let t = cond (k,m) pos neg in 
        (* Printf.printf "==== %s\n\n%!" (to_string t); *)
        t
      )
  in
  List.fold_right rows ~init:id ~f:(create_from_row)  
  
let prune_unreachable =
  map ~f:mk_leaf ~g:(fun (v,l) tru fls ->
      (* Printf.printf "Found %s ~ %s, " (Field.to_string v) (Value.to_string l); *)
        if Value.is_top l then
          (* (Printf.printf "pruning to : %s%!\n" (to_string tru); *)
          tru
        else
          (* (Printf.printf "not pruning\n%!"; *)
          cond (v,l) tru fls)


let tests_in keys =
  fold ~f:(fun _ -> true)
    ~g:(fun (v,_) acc_l acc_r ->
      List.exists keys ~f:((=) v)
      && acc_l && acc_r
    )
  
  
    
let rec delta d keys =
  match unget d with
  | Leaf a -> (get(Leaf a), get(Leaf Action.zero))
  | Branch {test=(g,m); tru; fls;_} ->
     if List.exists keys ~f:((=) g)
     then
       let (top_t, bottom_t) = delta tru keys in
       let (top_f, bottom_f) = delta fls keys in
       let top = cond (g,m) top_t top_f in
       let bot = sum bottom_t bottom_f in
       (* Printf.printf "\nsplitting %s on %s \ninto %s \nand %s\n\n%!"
        *   (d |> to_string)
        *   (List.fold keys ~init:"" ~f:(fun acc f -> Printf.sprintf "%s %s" acc (f |> Field.to_string)))
        *   (top |> to_string) (bot |> to_string); *)
       (top, bot)
                                                
        
     else (get(Leaf Action.one), d)
