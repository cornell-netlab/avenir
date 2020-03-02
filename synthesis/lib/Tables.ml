open Bignum
open Core
open Util
open Ast
open Manip

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
    | Between (lo, hi) -> (Value(lo) %<=% Var k) %&% (Var k %<=% Value(hi))

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
                           
      

  let is_subset (m : t) (m': t) : bool =
    match m, m' with
    | Exact i, Exact j -> veq i j
    | Exact i, Between (lo', hi') -> vleq lo' i && vleq i hi'
    | Between (lo, hi), Exact j -> veq lo j && veq hi j
    | Between (lo, hi), Between (lo', hi') -> vleq lo hi' && vleq lo' hi


    
end
                                       
module Row = struct
  type action_data = value list
  type t = Match.t list * action_data * int
  let to_string ((mtchs, ad, actid) : t) =
    Printf.sprintf "%s   ---(%s)---> %d"
      (List.fold mtchs ~init:"" ~f:(fun acc m -> Printf.sprintf "%s, %s" acc (Match.to_string m)))
      (List.fold ad ~init:"" ~f:(fun acc d -> Printf.sprintf "%s, %s" acc (string_of_value d)))
      actid

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
                 ~f:(fun acc (p,_) ->
                   match StringMap.find match_model p with
                   | None ->
                      failwith ("Couldn't find " ^ p)
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
  type t = string * Row.t
  let to_string (nm,(row) : t) =
    Printf.sprintf "%s <++ %s" nm (Row.to_string row) 
                   
end               

module Instance = struct
  type t = Row.t list StringMap.t

  let empty = StringMap.empty  
                 
  let update (inst : t) ((tbl, row) : Edit.t) =
    StringMap.update inst tbl
      ~f:(fun rows_opt ->
        match rows_opt with
        | None -> [row]
        | Some rows -> row::rows)
      
  let rec update_list (inst : t) (edits : Edit.t list) =
    match edits with
    | [] -> inst
    | (e::es) -> update_list (update inst e) es


  let size : t -> int =
    StringMap.fold ~init:0 ~f:(fun ~key:_ ~data -> (+) (List.length data))
                             
  let delete_hole i tbl = Hole(Printf.sprintf "?DeleteRow%dIn%s" i tbl, 1)    

  let rec apply tag encode_tag ?cnt:(cnt=0) (inst : t) (prog : cmd) : (cmd * int) =
    match prog with
    | Skip 
      | Assign _
      | Assert _ 
      | Assume _ -> (prog, cnt)
    | Seq (c1,c2) ->
       let (c1', cnt1) = apply tag encode_tag ~cnt inst c1 in
       let (c2', cnt2) = apply tag encode_tag ~cnt:cnt1 inst c2 in
       (c1' %:% c2', cnt2)
    | While _ -> failwith "while loops not supported"
    | Select (typ, ss) ->
       let (ss, ss_cnt) =
         List.fold ss ~init:([],cnt)
           ~f:(fun (acc, cnt) (t, c) ->
             let (c', cnt') = apply tag encode_tag ~cnt inst c in
             acc @ [(t,c')], cnt'
           ) in
       (mkSelect typ ss, ss_cnt)
    | Apply (tbl, keys, acts, default) ->
       let actSize = max (log2(List.length acts)) 1 in
       let selects =
         let rows = StringMap.find_multi inst tbl in
         List.foldi rows ~init:[]
           ~f:(fun i acc (matches, data, action) ->
             let tst = List.fold2_exn keys matches
                         ~init:True
                         ~f:(fun acc x m ->
                           (acc %&% Match.to_test x m)
                           %&% match tag with
                               | `WithHoles -> (delete_hole i tbl %=% mkVInt(0,1))
                               | `NoHoles -> True ) in
             if action >= List.length acts then
               []
             else
               (tst, (List.nth acts action
                      |> Option.value ~default:([], default)
                      |> bind_action_data data))
               :: acc)
       in
       let add_row_hole = Hole ("?AddRowTo" ^ tbl, 1) in
       let which_act_hole = Hole ("?ActIn" ^ tbl, actSize) in
       let holes =
         match tag with
         | `WithHoles -> 
            List.mapi acts
              ~f:(fun i (scope, act) -> 
                (List.fold keys ~init:True
                   ~f:(fun acc (x,sz) ->
                     acc %&% Match.holes encode_tag x sz))
                %&% (add_row_hole %=% mkVInt (1,1))
                %&% (which_act_hole %=% mkVInt (i,actSize))
                  , holify (List.map scope ~f:fst) act)
         | `NoHoles -> []
       in
       let dflt_row =
         let cond =
           match tag with
           | `WithHoles -> True (*add_row_hole %=% mkVInt (0,1)*)
           | `NoHoles -> True in
         [(cond, default)] in
       (selects @ holes @ dflt_row |> mkOrdered
       , cnt (*+ 1*))
        
         

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




                    
