open Core
open Util
open Ast
open Manip

(* TYPES *)
module Match = struct
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
      ~f:(fun acc (k,_) m ->
        acc %&% to_test_hole tbl k m
      )


  let list_to_string : t list -> string =
    List.fold ~init:"" ~f:(fun acc m -> Printf.sprintf "%s %s" acc (to_string m))

  let list_to_test (keys : (string * size) list) (matches : t list) =
    List.fold2_exn keys matches ~init:True ~f:(fun acc k m -> acc %&% to_test k m)


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


end

module Row = struct
  type action_data = value list

  (* Match expresssions, action data, action index*)
  type t = Match.t list * action_data * int

  let to_string ((mtchs, ad, actid) : t) =
    Printf.sprintf "%s,%s,%d"
      ( List.map mtchs ~f:(Match.to_string)
        |> List.reduce ~f:(Printf.sprintf "%s;%s")
        |> Option.value ~default:""
      )
      (List.map ad ~f:(string_of_value)
       |> List.reduce ~f:(Printf.sprintf "%s;%s")
       |> Option.value ~default:"")
      actid

  let test_of_data (tbl : string) (act_id : int) (vars : (string * size) list) (vals : action_data) =
    List.fold2_exn vars vals ~init:True
      ~f:(fun acc (x,sz) v ->
        assert (sz = size_of_value v);
        mkAnd acc @@
          (Hole.action_data_hole tbl act_id x sz %=% Value v)
      )

  let intersects (m1s, _,_ : t) (m2s, _, _ : t) : bool =
    List.fold2_exn m1s m2s ~init:true
      ~f:(fun acc m1 m2 -> acc && Match.has_inter m1 m2)

  let get_ith_match (i : int) ((ms, _,_) : t) =
    List.nth ms i

  let mk_new_row match_model phys tbl_name data_opt act : t option =
    match get_schema_of_table tbl_name phys with
    | None -> failwith ("Couldnt find keys for table " ^ tbl_name)
    | Some (ks, acts, _) ->
       let keys_holes =
         List.fold ks ~init:(Some [])
           ~f:(fun acc (v, sz) ->
             let hlo,hhi = Hole.match_holes_range tbl_name v in
             match acc,
                   fixup_expr match_model (Hole(hlo, sz)),
                   fixup_expr match_model (Hole(hhi, sz))
             with
             | None, _,_ -> None
             | Some ks, Hole _, Hole _ ->  begin
                 let h, hm = Hole.match_holes_mask tbl_name v in
                   match fixup_expr match_model (Hole(h, sz)),
                         fixup_expr match_model (Hole(hm,sz))
                   with
                   | Hole _,_ ->
                      Some (ks @ [Match.mask_ (mkInt(0,sz)) (mkInt(0,sz))])
                   (*    Printf.sprintf "when filling %s couldn't find %s in model %s" tbl_name h (string_of_map match_model)
                    * |> failwith *)
                | Value v,Hole _ ->
                   Some (ks @ [Match.exact_ v])
                | Value v, Value m ->
                   Some (ks @ [Match.mask_ v m])
                | _ -> failwith "Model did something weird"
                end
              | Some ks, Value lo, Value hi ->
                let k = if veq lo hi
                  then [Match.exact_ lo]
                  else if vleq hi lo
                  then failwith "Low value greater than high value in model from z3"
                  else [Match.between_ lo hi] in
                Some (ks @ k)
              | _, _,_ -> failwith "got something that wasn't a model"
            ) in
      let data =
        match data_opt with
        | Some ds -> ds
        | None ->
          match List.nth acts act with
          | None -> []
          | Some (params, _) ->
            (* Printf.printf "Params for act %d :%s\n%!" act
             *   (List.fold params ~init:"" ~f:(fun acc (p,_) -> Printf.sprintf "%s %s" acc p)); *)
            List.fold params ~init:[]
              ~f:(fun acc (p,sz) ->
                  match StringMap.find match_model @@ Hole.action_data tbl_name act p sz with
                  | None ->
                     (* Printf.printf "Cannot find expected key %s in %s \n%!" p (string_of_map match_model);*)
                     acc @ [Int (Random.int (pow 2 sz) |> Bigint.of_int_exn, sz)]
                  | Some v -> acc @ [v]
                )
      in
      match keys_holes with
      | None -> None
      | Some ks -> Some (ks, data, act)


  let remove_conflicts _ _ _ _ _ _ = None
  (* let remove_conflicts checker (params : Parameters.t) tbl_name keys (ms : Match.t list)  (rows : t list)  =
   *   let prop = Match.list_to_test keys ms %=>%
   *                (List.fold rows ~init:False ~f:(fun acc (ms',_,_) -> acc %+% Match.list_to_test keys ms')) in
   *   match fst (checker prop) with
   *   | Some _ ->
   *      let rows' =
   *        List.filter rows ~f:(fun ((ms', _,_)) ->
   *            if List.fold2_exn ms ms' ~init:true ~f:(fun acc m m' -> acc && Match.is_subset m m')
   *            then
   *              let _ = if params.interactive then
   *                        Printf.printf "- %s : %s" tbl_name (Match.list_to_string ms) in
   *              false
   *            else true
   *          ) in
   *      if rows = rows'
   *      then None
   *      else Some rows'
   *   | None -> None              *)

end

module Edit = struct
  type t = Add of string * Row.t (* Name of table *)
         | Del of string * int

  let table = function
    | Add (name, _ )
    | Del (name, _) -> name

  let is_delete = function
    | Add _ -> false
    | Del _ -> true

  let has_delete = List.exists ~f:(is_delete)

  let split = List.fold ~init:([],[])
                ~f:(fun (dels,adds) e ->
                  match e with
                  | Add _ -> (dels, adds @ [e])
                  | Del _ -> (dels @ [e], adds))

  let get_deletes = List.filter_map ~f:(function
                        | Add _ -> None
                        | Del (n,i) -> Some (n,i)
                      )

  let to_test phys e =
    match e with
    | Del(t,i) -> Hole.delete_hole i t %=% mkVInt(1,1)
    | Add(t,(ms,ds,i)) ->
       match get_schema_of_table t phys with
       | None -> failwith @@ Printf.sprintf "Couldn't find table %s" t
       | Some (keys,actions,_) ->
          let actSize = max (log2 (List.length actions)) 1 in
          Hole.add_row_hole t %=% mkVInt(1,1)
          %&%
            (Hole.which_act_hole t actSize  %=% mkVInt(i,actSize))
          %&%
            (Match.test_hole_of_lists t keys ms)
          %&%
            (Row.test_of_data t i (List.nth_exn actions i |> fst) ds)

  let test_of_list phys es =
    List.(map es ~f:(to_test phys) |> reduce_exn ~f:(%&%))

  let to_string e =
    match e with
    | Add (nm, row) -> Printf.sprintf "ADD,%s,%s" nm (Row.to_string row)
    | Del (nm, idx) -> Printf.sprintf "DEL,%s,%d" nm idx

  let eq_tests e e' =
    match e, e' with
    | Add(nm,(m,_,_)), Add(nm',(m',_,_)) when nm = nm'
      -> m = m'
    | _ -> false

  let get_ith_match ~i (e : t) =
    match e with
    | Add (_, row) -> Row.get_ith_match i row
    | Del (_, _) -> None

  let extract phys (m : value StringMap.t)  : t list =
    let dels, adds =
      StringMap.fold m ~init:([],[]) (*Deletions, additions*)
        ~f:(fun ~key ~data acc ->
          (* Printf.printf "%s\n" (string_of_map m); *)
        match Hole.find_add_row key with
        | None -> begin
            match Hole.find_delete_row key with
            | Some (table_name,row_idx) when data |> get_int = Bigint.one -> begin
                (Del(table_name, row_idx) :: fst acc, snd acc)
              end
            |  _ -> acc
          end
        | Some tbl ->
           if data |> get_int = Bigint.one then
             let act = match StringMap.find m (Hole.which_act_hole_name tbl) with
               | None ->
                  Printf.sprintf "WARNING:: Couldn't find %s even though I found %s to be true\n%!"  (Hole.which_act_hole_name tbl) key
                  |> failwith
               | Some v -> get_int v |> Bigint.to_int_exn in
             (* Printf.printf "Making new row from \n %s \n in tbl %s and action %d \n%!" (string_of_map m) tbl act; *)
             match Row.mk_new_row m phys tbl None act with
             | None -> failwith (Printf.sprintf "Couldn't make new row in table %s\n" tbl)
             | Some row ->
                (fst acc, Add (tbl, row) :: snd acc)
           else acc)
   in
   List.dedup_and_sort dels  ~compare:(fun i j ->
       match i, j with
       | Del(_,ix), Del(_, jx) -> compare jx ix
       | _, _ -> failwith "dels list contains an add")
   @ adds

end

(* END TYPES *)
