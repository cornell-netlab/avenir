open Core
open Util
open Ast
open Manip

(* TYPES *)

module Match = struct      
  type t =
    | Exact of int * int
    | Between of int * int * int 
                               
  let to_string m =
    match m with 
    | Exact (i,s) -> Printf.sprintf "%d#%d" i s
    | Between (lo,hi,s) -> Printf.sprintf "[%d,%d]#%d" lo hi s

  let to_test k m =
    match m with 
    | Exact (x,sz) -> (Var k %=% mkVInt(x,sz))
    | Between (lo, hi,sz) -> (mkVInt(lo,sz) %<=% Var k) %&% (Var k %<=% mkVInt(hi,sz))

  let holes encode_tag x sz =
    match encode_tag with
    | `Range -> (Var (x,sz) %<=% Hole ("?"^x^"_hi",sz))
                %&% (Var(x,sz) %>=% Hole("?"^x^"_lo",sz))
    | `Exact -> Var(x, sz) %=% Hole ("?"^x, sz)         
                                                               
  let list_to_string : t list -> string =
    List.fold ~init:"" ~f:(fun acc m -> Printf.sprintf "%s %s" acc (to_string m))

  let list_to_test (keys : (string * size) list) (matches : t list) =
    List.fold2_exn keys matches ~init:True ~f:(fun acc k m -> acc %&% to_test k m)

  let cap (m : t) (m' : t) =
    match m, m' with
    | Exact (i,_), Exact (j,_) ->
       if i = j then [m] else []
    | Exact (i,_), Between (lo, hi, _)
      | Between (lo, hi, _), Exact(i,_)->
       if lo <= i && i <= hi then
         [m]
       else
         []
    | Between (lo, hi, sz), Between (lo', hi', _) ->
       let lo'' = max lo lo' in
       let hi'' = min hi hi' in
       if lo'' < hi'' then
         [Between (lo'', hi'', sz)]
       else if lo'' = hi'' then
         [Exact(lo'',sz)]
       else
         []

  let has_inter (m : t) (m' : t) : bool =
    match m, m' with
    | Exact (x,_), Exact(y,_) -> x = y
    | Exact (x, _), Between(lo, hi,_)
      | Between(lo,hi,_), Exact(x,_)
      -> lo <= x && x <= hi
    | Between(lo, hi, _), Between(lo',hi',_)
      -> max lo lo' <= min hi hi'
                           
  let cup (m : t) (m' : t) : t list =
    match m, m' with
    | Exact (i,_), Exact (j,sz) ->
       if i = j then
         [m]
       else if off_by_one i j
       then [Between (min i j, max i j, sz)]
       else [m; m']
    | Exact (i,_), Between (lo, hi, sz)
      | Between (lo, hi, sz), Exact(i,_)->
       if lo <= i && i <= hi then
         [Between (lo, hi, sz)]
       else if i + 1 = lo || hi + 1 = i
       then [Between(min lo i, max hi i, sz)]
       else [m;m']
    | Between (lo, hi, sz), Between (lo', hi', _) ->
       if hi >= lo - 1 && lo <= hi + 1
       then [Between (min lo lo', max hi hi', sz)]
       else [m;m']

  let cup_list (ms : t list): t list =
    List.fold ms ~init:[]
      ~f:(fun acc m ->
        List.fold acc ~init:[]
          ~f:(fun acc' m' ->
            acc' @ cup m m' ))
      

  let is_subset (m : t) (m': t) : bool =
    match m, m' with
    | Exact (i, _), Exact (j, _) -> i = j
    | Exact (i, _), Between (lo', hi',_) -> lo' <= i && i <= hi'
    | Between (lo, hi, _), Exact (j,_) -> lo = j && hi = j
    | Between (lo, hi, _), Between (lo', hi', _) ->  lo <= hi' && lo' <= hi
                                                                           
  let rec minus (m : t) (ms : t list) : t list =
    let compare m m' =
      match m, m' with
      | Exact(i,_), Exact(j,_) -> compare i j
      | Exact(i,_), Between(lo,_,_) -> compare i lo
      | Between(lo, _,_), Exact(i,_) -> compare lo i
      | Between(lo, _, _), Between(lo',_,_) -> compare lo lo'
    in
    match m with
    | Exact(_, _) ->
       if List.exists ms ~f:(is_subset m)
       then []
       else [m]
    | Between(lo,hi, sz) ->
       match ms with
       | [] -> [m]
       | (m'::ms') ->
          match m' with
          | Exact(j,_) ->
             if j < lo || j > hi
             then minus (Between (lo,hi,sz)) ms'
             else if j = lo then
               minus (Between(lo+1, hi, sz)) ms'
             else if j = hi then
               minus (Between (lo, hi-1, sz)) ms'
             else
               minus (Between (lo, j-1, sz)) ms'
               @ minus (Between (j+1, hi, sz)) ms'
               |> cup_list
               |> List.sort ~compare

          | Between(lo', hi', _) ->
             if lo' <= lo && hi <= hi'
             then []
             else if lo' <= lo
             then minus (Between(hi'+1, hi, sz)) ms'
             else if hi <= hi' then
               minus (Between(lo, lo'-1,sz)) ms'
             else (*lo < lo' && hi > hi'*)
               minus (Between (lo, lo'-1, sz)) ms'
               @ minus (Between (hi'+1, hi, sz)) ms'
               |> cup_list
               |> List.sort ~compare
end
                                       
module Row = struct
  type action_data = (int * int) list
  type t = Match.t list * action_data * int
  let to_string (mtchs, ad, actid) =
    Printf.sprintf "%s   ---(%s)---> %d"
      (List.fold mtchs ~init:"" ~f:(fun acc m -> Printf.sprintf "%s, %s" acc (Match.to_string m)))
      (List.fold ad ~init:"" ~f:(fun acc (d,_) -> Printf.sprintf "%s, %d" acc d))
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
                | Value(Int (x, sz)) ->
                   Some (ks @ [Match.Exact (x,sz)])
                | _ -> failwith "Model did something weird"
                end
             | Some ks, Value(Int(lo,_)), Value(Int(hi,_)) ->
                let k = if lo = hi
                        then [Match.Exact (lo, sz)]
                        else if lo > hi
                        then failwith "Low value greater than high value in model from z3"
                        else [Match.Between (lo, hi,sz)] in
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
                      failwith ("Couldn't find " ^ p)
                   | Some v -> acc @ [(get_int v, sz)]
                 )
       in
       match keys_holes with
       | None -> None
       | Some ks -> Some (ks, data, act)


                           
                         
  let remove_conflicts checker keys (ms : Match.t list)  (rows : t list)  =
    let prop = Match.list_to_test keys ms %=>%
                 (List.fold rows ~init:False ~f:(fun acc (ms',_,_) -> acc %+% Match.list_to_test keys ms')) in
    match fst (checker prop) with
    | Some _ -> 
       let rows' =
         List.filter rows ~f:(fun ((ms', _,_)) ->
             not(List.fold2_exn ms ms' ~init:true ~f:(fun acc m m' -> acc && Match.is_subset m m'))
           ) in
       if rows = rows'
       then None
       else Some rows'
    | None -> None             
                         
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



  let update_consistently checker match_model (phys : cmd) (tbl_name : string) (act_data : Row.action_data option) (act : int) (acc : [`Ok of t | `Conflict of t]) =
    let (keys,_,_) = get_schema_of_table tbl_name phys |> Option.value_exn in
    match acc with
    | `Ok pinst -> begin match StringMap.find pinst tbl_name,
                               Row.mk_new_row match_model phys tbl_name act_data act with
                 | _, None -> acc
                 | None,Some row ->
                    `Ok (StringMap.set pinst ~key:tbl_name ~data:[row])
                 | Some rows, Some (ks, data,act) ->
                    begin match Row.remove_conflicts checker keys ks rows with
                    | None -> `Ok (StringMap.set pinst ~key:tbl_name
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
        `Conflict (StringMap.set pinst ~key:tbl_name ~data:[row])
       | Some rows, Some (ks, data, act) ->
          begin match Row.remove_conflicts checker keys ks rows with
          | None -> `Conflict (StringMap.set pinst ~key:tbl_name
                                 ~data:((ks,data,act)::rows))
          | Some rows' ->
           `Conflict (StringMap.set pinst ~key:tbl_name
                        ~data:((ks,data,act)::rows'))
          end
       end   

  let remove_deleted_rows match_model (pinst : t) : t =
    StringMap.fold pinst ~init:empty ~f:(fun ~key ~data acc ->
        StringMap.set acc ~key
          ~data:(
            List.filteri data ~f:(fun i _ ->
                match delete_hole i key with
                | Hole(s,_) ->
                   begin match StringMap.find match_model s with
                   | None -> true
                   | Some x -> get_int x = 1
                   end
                | _ -> true
              )
          )
        
      )

  let fixup_edit checker match_model (action_map : (Row.action_data * size) StringMap.t option) (phys : cmd) (pinst : t) : [`Ok of t | `Conflict of t] =
    match action_map with
    | Some m -> StringMap.fold ~init:(`Ok pinst) m ~f:(fun ~key:tbl_name ~data:(act_data,act) ->
                    update_consistently checker match_model phys tbl_name (Some act_data) act)
    | None -> 
       let tables_added_to =
         StringMap.fold match_model ~init:[]
           ~f:(fun ~key ~data acc ->
             if String.is_substring key ~substring:"AddRowTo"
             then (String.substr_replace_all key ~pattern:"?" ~with_:""
                   |> String.substr_replace_first ~pattern:"AddRowTo" ~with_:"")
                  :: acc
             else acc 
           ) in
       let pinst' = remove_deleted_rows match_model pinst in
       let out = List.fold tables_added_to ~init:(`Ok pinst')
         ~f:(fun inst tbl_name ->
           let act = StringMap.find_exn match_model ("?ActIn" ^ tbl_name) |> get_int in
           update_consistently checker match_model phys tbl_name None act inst
         )
       in
       out
         
         
end
                    
                    (* END TYPES *)

