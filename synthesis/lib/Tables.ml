open Bignum
open Core
open Util
open Ast
open Manip

let (=) = Stdlib.(=)   
   
(* TYPES *)
module Match = struct
  type t =
    | Exact of value
    | Between of value * value
    | Mask of value * value

  let to_string m =
    match m with
    | Exact x -> string_of_value x
    | Between (lo,hi) -> Printf.sprintf "[%s,%s]" (string_of_value lo) (string_of_value hi)
    | Mask (v,m) -> Printf.sprintf "%s & %s" (string_of_value v) (string_of_value m)

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
        | Some i ->
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



  let has_inter (m : t) (m' : t) : bool =
    match m, m' with
    | Exact x, Exact y -> veq x y
    | Exact x, Between (lo, hi)
      | Between(lo,hi), Exact(x)
      -> vleq lo x && vleq x hi
    | Between(lo, hi), Between(lo',hi')
      -> Stdlib.max lo lo' <= Stdlib.min hi hi'
    | Mask _, _ | _, Mask _ ->
       failwith "Dont know how to intersect masks"


                                         
  let has_inter_l (ms : t list) (ms' : t list) : bool =
    if ms = [] && ms' = [] then false
    else 
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

  let intersects (m1s, _,_ : t) (m2s, _, _ : t) : bool =
    List.fold2_exn m1s m2s ~init:true
      ~f:(fun acc m1 m2 -> acc && Match.has_inter m1 m2)
    
  let get_ith_match (i : int) ((ms, _,_) : t) =
    List.nth ms i
      
  let mk_new_row match_model phys tbl_name data_opt act : t option =
    match get_schema_of_table tbl_name phys with
    | None -> failwith ("Couldnt find keys for table " ^ tbl_name)
    | Some (ks, acts, d) ->
       let keys_holes =
         List.fold ks ~init:(Some [])
           ~f:(fun acc (v, sz) ->
             let hlo,hhi = Hole.match_holes_range tbl_name v in
             match acc,
                   fixup_val match_model (Hole(hlo, sz)),
                   fixup_val match_model (Hole(hhi, sz))
             with
             | None, _,_ -> None
             | Some ks, Hole _, Hole _ ->  begin
                 let h, hm = Hole.match_holes_mask tbl_name v in
                   match fixup_val match_model (Hole(h, sz)),
                         fixup_val match_model (Hole(hm,sz))
                   with
                   | Hole _,_ ->
                      Some (ks @ [Match.Mask(mkInt(0,sz),mkInt(0,sz))])
                   (*    Printf.sprintf "when filling %s couldn't find %s in model %s" tbl_name h (string_of_map match_model)
                    * |> failwith *)
                | Value v,Hole _ ->
                   Some (ks @ [Match.Exact v])
                | Value v, Value m ->
                   Some (ks @ [Match.Mask (v,m)])
                | _ -> failwith "Model did something weird"
                end
             | Some ks, Value lo, Value hi ->
                let k = if veq lo hi
                        then [Match.Exact lo]
                        else if vleq hi lo
                        then failwith "Low value greater than high value in model from z3"
                        else [Match.Between (lo, hi)] in
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
                   match StringMap.find match_model p with
                   | None ->
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
    | Add (s, row) -> Row.get_ith_match i row
    | Del (s, row) -> None

  let extract phys (m : value StringMap.t)  : t list =
   let dels, adds =  StringMap.fold m ~init:([],[]) (*Deletions, additions*)
      ~f:(fun ~key ~data acc ->
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
