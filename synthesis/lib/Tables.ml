open Core
open Util
open Ast
open Manip

module Row = struct
  type action_data = value list

  (* Match expresssions, action data, action index*)
  type t = Match.t list * action_data * int

  let action_data_to_string ad =
      (List.map ad ~f:(string_of_value)
       |> List.reduce ~f:(Printf.sprintf "%s;%s")
       |> Option.value ~default:"")

  let to_string ((mtchs, ad, actid) : t) =
    Printf.sprintf "%s,%s,%d"
      (Match.list_to_string mtchs)
      (action_data_to_string ad)
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
           ~f:(fun acc (key, sz, v_opt) ->
             match v_opt with
             | Some _ -> acc
             | None ->
                let hlo,hhi = Hole.match_holes_range tbl_name key in
                match acc,
                      fixup_expr match_model (Hole(hlo, sz)),
                      fixup_expr match_model (Hole(hhi, sz))
                with
                | None, _,_ -> None
                | Some ks, Hole _, Hole _ ->  begin
                    let h, hm = Hole.match_holes_mask tbl_name key in
                    match fixup_expr match_model (Hole(h, sz)),
                          fixup_expr match_model (Hole(hm,sz))
                    with
                    | Hole _,_ ->
                       Some (ks @ [Match.mask_ key (mkInt(0,sz)) (mkInt(0,sz))])
                    | Value v,Hole _ ->
                       Some (ks @ [Match.exact_ key v])
                    | Value v, Value m ->
                       Some (ks @ [Match.mask_ key v m])
                    | _ -> failwith "Model did something weird"
                  end
                | Some ks, Value lo, Value hi ->
                   let k = if vleq hi lo
                           then failwith "Low value greater than high value in model from z3"
                           else [Match.between_ key lo hi] in
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
               (* Printf.printf "Params for %s.action[%d] :%s\n%!" tbl_name act
                *   (List.fold params ~init:""
                *      ~f:(fun acc (p,_) -> Printf.sprintf "%s %s" acc p)); *)
               List.fold params ~init:[]
                 ~f:(fun acc (p,sz) ->
                   let v =
                     let p_hole = Hole.action_data tbl_name act p sz in
                     match StringMap.find match_model p_hole  with
                     | None ->
                        Printf.printf "[WARNING] couldn't find action data %s in %s \n%!" p_hole (string_of_map match_model);
                        Int (Random.int (pow 2 sz) |> Bigint.of_int_exn, sz)
                     | Some v ->
                        v
                   in
                   (* Printf.printf "\t%s -> %s\n" p (string_of_value v); *)
                   acc @ [v]
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

  let get_matches_exn = function
    | Add (_, (matches,_,_)) -> matches
    | Del _ -> failwith "[Edit.get_matches] tried to get matches of a deletion"


  let to_test phys e =
    match e with
    | Del(t,i) -> Hole.delete_hole i t %=% mkVInt(1,1)
    | Add(t,(ms,ds,i)) ->
       match get_schema_of_table t phys with
       | None -> failwith @@ Printf.sprintf "Couldn't find table %s" t
       | Some (_,actions,_) ->
          let actSize = max (log2 (List.length actions)) 1 in
          Hole.add_row_hole t %=% mkVInt(1,1)
          %&%
            (Hole.which_act_hole t actSize %=% mkVInt(i,actSize))
          %&%
            (Match.test_hole_of_lists t ms)
          %&%
            (Row.test_of_data t i (List.nth_exn actions i |> fst) ds)

  let test_of_list phys es =
    List.(map es ~f:(to_test phys) |> reduce_exn ~f:(%&%))

  let to_string e =
    match e with
    | Add (nm, row) -> Printf.sprintf "ADD,%s,%s" nm (Row.to_string row)
    | Del (nm, idx) -> Printf.sprintf "DEL,%s,%d" nm idx

  let list_to_string es =
    List.fold es ~init:"" ~f:(fun acc e -> Printf.sprintf "%s\n%s" acc (to_string e))

  let eq_tests e e' =
    match e, e' with
    | Add(nm,(m,_,_)), Add(nm',(m',_,_)) when nm = nm'
      -> m = m'
    | _ -> false

  let equal e e' = Stdlib.(e = e')

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
