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

  let to_string m =
    match m with
    | Exact x -> string_of_value x
    | Between (lo,hi) -> Printf.sprintf "[%s,%s]" (string_of_value lo) (string_of_value hi)

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
         

  let holes encode_tag x sz =
    match encode_tag with
    | `Range -> (Var (x,sz) %<=% Hole ("?"^x^"_hi",sz))
                %&% (Var(x,sz) %>=% Hole("?"^x^"_lo",sz))
    | `Exact -> Var(x, sz) %=% Hole ("?"^x, sz)

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

  let has_inter (m : t) (m' : t) : bool =
    match m, m' with
    | Exact x, Exact y -> veq x y
    | Exact x, Between (lo, hi)
      | Between(lo,hi), Exact(x)
      -> vleq lo x && vleq x hi
    | Between(lo, hi), Between(lo',hi')
      -> Stdlib.max lo lo' <= Stdlib.min hi hi'

                                         
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
    

      
  let mk_new_row match_model phys tbl_name data_opt act : t option =
    match get_schema_of_table tbl_name phys with
    | None -> failwith ("Couldnt find keys for table " ^ tbl_name)
    | Some (ks, acts, d) ->
       let keys_holes =
         List.fold ks ~init:(Some [])
           ~f:(fun acc (v, sz) ->
             match acc,
                   fixup_val match_model (Hole("?"^v^"_lo", sz)),
                   fixup_val match_model (Hole("?"^v^"_hi", sz))
             with
             | None, _,_ -> None
             | Some ks, Hole _, Hole _ ->
                begin match fixup_val match_model (Hole("?"^v, sz)) with
                | Hole _ -> None
                | Value v ->
                   Some (ks @ [Match.Exact v])
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

  let to_string e =
    match e with
    | Add (nm, row) -> Printf.sprintf "ADD,%s,%s" nm (Row.to_string row)
    | Del (nm, idx) -> Printf.sprintf "DEL,%s,%d" nm idx

  let eq_tests e e' =
    match e, e' with
    | Add(nm,(m,_,_)), Add(nm',(m',_,_)) when nm = nm'
      -> m = m'
    | _ -> false


  let extract phys (m : value StringMap.t)  : t list =
    StringMap.fold m ~init:([],[]) (*Deletions, additions*)
      ~f:(fun ~key ~data acc ->
        match String.chop_prefix key ~prefix:"?AddRowTo" with
        | None -> acc
        | Some tbl ->
           if data |> get_int = Bigint.one then
             let actin_vname =  (Printf.sprintf "?ActIn%s" tbl)  in
             let act = match StringMap.find m actin_vname with
               | None ->
                  Printf.printf "WARNING:: Couldn't find %s even though I found %s to be true\n%!"
                    actin_vname
                    key;
                  failwith ""
               | Some v -> get_int v |> Bigint.to_int_exn in
             match Row.mk_new_row m phys tbl None act with
             | None -> failwith (Printf.sprintf "Couldn't make new row in table %s\n" tbl)
             | Some row ->
                (fst acc, Add (tbl, row) :: snd acc)
           else acc)
    |> uncurry (@)

end

module Instance = struct
  type t = Row.t list StringMap.t (* Keys are table names, Rows are table rows*)

  let empty = StringMap.empty

  let update (inst : t) (e : Edit.t) =
    match e with
    | Add (tbl, row) ->
       StringMap.update inst tbl
         ~f:(fun rows_opt ->
           match rows_opt with
           | None -> [row]
           | Some rows -> row::rows)
    | Del (tbl, i) ->
       StringMap.change inst tbl
         ~f:(function
           | None -> None
           | Some rows -> List.filteri rows ~f:(fun j _ -> i <> j) |> Some)

  let rec update_list (inst : t) (edits : Edit.t list) =
    match edits with
    | [] -> inst
    | (e::es) -> update_list (update inst e) es


  let rec overwrite (old_inst : t) (new_inst : t) : t =
    StringMap.fold new_inst ~init:old_inst
      ~f:(fun ~key ~data acc -> StringMap.set acc ~key ~data)


  let size : t -> int =
    StringMap.fold ~init:0 ~f:(fun ~key:_ ~data -> (+) (List.length data))

  let delete_hole i tbl = Hole(Printf.sprintf "?DeleteRow%dIn%s" i tbl, 1)
  let add_row_hole tbl = Hole (Printf.sprintf "?AddRowTo%s" tbl, 1)
  let which_act_hole tbl actSize =
    assert (actSize > 0);
    Hole (Printf.sprintf "?ActIn%s" tbl, actSize)
                        

  let tbl_hole encode_tag keys tbl row_hole act_hole i actSize =
    (List.fold keys ~init:True
       ~f:(fun acc (x,sz) ->
         acc %&% Match.holes encode_tag x sz))
    %&% (row_hole %=% mkVInt (1,1))
    %&% (act_hole %=% mkVInt (i,actSize))
    
  let rec apply ?no_miss:(no_miss = false) tag encode_tag ?cnt:(cnt=0) (inst : t) (prog : cmd) : (cmd * int) =
    match prog with
    | Skip
      | Assign _
      | Assert _
      | Assume _ -> (prog, cnt)
    | Seq (c1,c2) ->
       let (c1', cnt1) = apply ~no_miss tag encode_tag ~cnt inst c1 in
       let (c2', cnt2) = apply ~no_miss tag encode_tag ~cnt:cnt1 inst c2 in
       (c1' %:% c2', cnt2)
    | While _ -> failwith "while loops not supported"
    | Select (typ, ss) ->
       let (ss, ss_cnt) =
         List.fold ss ~init:([],cnt)
           ~f:(fun (acc, cnt) (t, c) ->
             let (c', cnt') = apply ~no_miss tag encode_tag ~cnt inst c in
             acc @ [(t,c')], cnt'
           ) in
       (mkSelect typ ss, ss_cnt)
    | Apply (tbl, keys, acts, default) ->
       let actSize = max (log2(List.length acts)) 1 in
       let row_hole = add_row_hole tbl in
       let act_hole = which_act_hole tbl actSize in
       let rows = StringMap.find_multi inst tbl in
       let selects =
         List.foldi rows ~init:[]
           ~f:(fun i acc (matches, data, action) ->
             let prev_tst =
               if List.for_all matches ~f:(function | Exact _ -> true | _ -> false) then
                 False
               else
                 let prev_rows =
                   if i + 1 >= List.length rows then [] else
                     List.sub rows ~pos:(i+1) ~len:(List.length rows - (i+1))
                 in
                 let overlapping_matches =
                   List.filter_map prev_rows
                     ~f:(fun (prev_ms,_,_) ->
                       if Match.has_inter_l matches prev_ms
                       then Some prev_ms
                       else None
                     )
                 in
                 List.fold overlapping_matches ~init:False
                   ~f:(fun acc ms -> acc %+% Match.list_to_test keys ms )
             in
             let tst = Match.list_to_test keys matches
                       %&% match tag with
                           | `WithHoles ds ->
                              (* delete_hole i tbl %=% mkVInt(0,1) *)
                              if List.exists ds ~f:((=) (tbl, i))
                              then delete_hole i tbl %=% mkVInt(0,1)
                              else True
                           | `NoHoles -> True in
             if action >= List.length acts then
               acc
             else begin
                 let cond = tst %&% !%(prev_tst) in
                 (* if params.debug then Printf.printf "[%s] Adding %s\n%!" tbl (string_of_test cond); *)
                 (cond, (List.nth acts action
                                         |> Option.value ~default:([], default)
                                         |> bind_action_data data))
                 :: acc
               end)
       in
       let holes =
         match tag with
         | `WithHoles _ ->
            List.mapi acts
              ~f:(fun i (params, act) ->
                (tbl_hole encode_tag keys tbl row_hole act_hole i actSize
                 %&% List.fold selects ~init:True
                       ~f:(fun acc (cond, _) -> acc %&% !%(cond))
                , holify (List.map params ~f:fst) act))
         | `NoHoles -> []
       in
       let dflt_row =
         let cond = if no_miss
                    then False
                    else List.foldi rows ~init:(True)
                           ~f:(fun i acc (ms,_,act) ->
                             acc %&%
                               if act >= List.length acts then True else
                                 !%(Match.list_to_test keys ms
                                    %&% match tag with
                                        | `WithHoles ds when List.exists ds ~f:((=) (tbl, i))
                                          -> delete_hole i tbl %=% mkVInt(0,1)
                                        | _ -> True)) in
         [(cond, default)] in
       let tbl_select = selects @ holes @ dflt_row |> (*mkPartial*) mkOrdered in
       (* Printf.printf "TABLE %s: \n %s\n%!" tbl (string_of_cmd tbl_select); *)
       (tbl_select, cnt)



  let update_consistently checker (params:Parameters.t) match_model (phys : cmd) (tbl_name : string) (act_data : Row.action_data option) (act : int) (acc : [`Ok of t | `Conflict of t]) : [`Ok of t | `Conflict of t] =
    let (keys,_,_) = get_schema_of_table tbl_name phys |> Option.value_exn in
    match acc with
    | `Ok pinst -> begin match StringMap.find pinst tbl_name,
                               Row.mk_new_row match_model phys tbl_name act_data act with
                 | _, None -> acc
                 | None,Some row ->
                    if params.interactive then
                      Printf.printf "+%s : %s\n%!" tbl_name (Row.to_string row);
                    `Ok (StringMap.set pinst ~key:tbl_name ~data:[row])
                 | Some rows, Some (ks, data,act) ->
                    if params.interactive then
                      Printf.printf "+%s : %s" tbl_name (Row.to_string (ks,data,act));
                    begin match Row.remove_conflicts checker params tbl_name keys ks rows with
                    | None ->
                       `Ok (StringMap.set pinst ~key:tbl_name
                                     ~data:((ks,data,act)::rows))
                    | Some rows' ->
                       `Conflict (StringMap.set pinst ~key:tbl_name
                                    ~data:((ks,data,act)::rows'))
                    end
                 end
    | `Conflict pinst ->
       begin match StringMap.find pinst tbl_name,
                 Row.mk_new_row match_model phys tbl_name (act_data) act with
       | _, None -> acc
       | None, Some row ->
          if params.interactive then
            Printf.printf "+%s : %s\n%!" tbl_name (Row.to_string row);
          `Conflict (StringMap.set pinst ~key:tbl_name ~data:[row])
       | Some rows, Some (ks, data, act) ->
          if params.interactive then
            Printf.printf "+%s : %s\n%!" tbl_name (Row.to_string (ks,data,act));
          begin match Row.remove_conflicts checker params tbl_name keys ks rows with
          | None -> `Conflict (StringMap.set pinst ~key:tbl_name
                                 ~data:((ks,data,act)::rows))
          | Some rows' ->
           `Conflict (StringMap.set pinst ~key:tbl_name
                        ~data:((ks,data,act)::rows'))
          end
       end

  let remove_deleted_rows (params : Parameters.t) match_model (pinst : t) : t =
    StringMap.fold pinst ~init:empty ~f:(fun ~key:tbl_name ~data acc ->
        StringMap.set acc ~key:tbl_name
          ~data:(
            List.filteri data ~f:(fun i _ ->
                match delete_hole i tbl_name with
                | Hole(s,_) ->
                   begin match StringMap.find match_model s with
                   | None -> true
                   | Some do_delete when get_int do_delete = Bigint.one ->
                      if params.interactive then Printf.printf "- %s : row %d\n%!" tbl_name i;
                      false
                   | Some x -> true
                   end
                | _ -> true
              )
          )

      )

  let fixup_edit checker (params : Parameters.t) (data : ProfData.t ref) match_model (action_map : (Row.action_data * size) StringMap.t option) (phys : cmd) (pinst : t) : [`Ok of t | `Conflict of t] =
    let st = Time.now() in
    match action_map with
    | Some m -> StringMap.fold ~init:(`Ok pinst) m ~f:(fun ~key:tbl_name ~data:(act_data,act) ->
                    update_consistently checker params match_model phys tbl_name (Some act_data) act)
    | None ->
       let tables_added_to =
         StringMap.fold match_model ~init:[]
           ~f:(fun ~key ~data acc ->
             if String.is_substring key ~substring:"AddRowTo"
                && data = Int(Bigint.one,1)
             then (String.substr_replace_all key ~pattern:"?" ~with_:""
                   |> String.substr_replace_first ~pattern:"AddRowTo" ~with_:"")
                  :: acc
             else acc
           ) in
       let pinst' = remove_deleted_rows params match_model pinst in
       let out = List.fold tables_added_to ~init:(`Ok pinst')
                   ~f:(fun inst tbl_name ->
                     let str = ("?ActIn" ^ tbl_name) in
                     match StringMap.find match_model ("?ActIn" ^ tbl_name) with
                     | None ->
                        Printf.sprintf "Couldn't Find var %s\n" str |> failwith
                     | Some v ->
                        let act = get_int v |> Bigint.to_int_exn in
                        update_consistently checker params match_model phys tbl_name None act inst )
       in
       data := {!data with fixup_time = Time.Span.(!data.fixup_time + Time.diff (Time.now ()) st)};
       out


end

                    (* END TYPES *)
