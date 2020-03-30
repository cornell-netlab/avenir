open Core
open Util
open Ast
open Manip
open Tables
              
type trace = (Row.action_data * int) StringMap.t


let rec compute_cand_for_trace (tag : [`Exact | `Range]) (line: cmd) (pinst : Instance.t) t : cmd =
  match line with
  | Skip 
    | Assert _
    | Assume _ 
    | Assign _
    -> line
  | Seq (c1,c2) -> compute_cand_for_trace tag c1 pinst t
                   %:% compute_cand_for_trace tag c2 pinst t
  | Select(typ, cs) ->
     List.map cs ~f:(fun (b, c) -> (b, compute_cand_for_trace tag c pinst t))
     |> mkSelect typ
  | Apply(name, keys, acts, default) ->
     (*might need to use existing instance to negate prior rules *)
     (* let misses =
      *   match StringMap.find pinst name with
      *   | None -> True
      *   | Some rows ->
      *      List.fold rows ~init:True
      *        ~f:(fun acc (ms, _) ->
      *          acc %&% !%(List.fold2_exn keys ms ~init:True ~f:(fun acc k m -> acc %&% encode_match k m))
      *        )
      * in *)
     begin match StringMap.find t name with
     | None -> Assume False
     | Some (data, act_idx) ->
        if act_idx >= List.length acts
        then (*Assert (misses) %:%*) default
        else List.fold keys ~init:True (*misses*)
               ~f:(fun acc (x, sz) -> acc %&% (Match.holes tag x sz))
             |> Assert
             |> flip mkSeq (List.nth_exn acts act_idx |> Manip.bind_action_data data )
     end
  | While _ -> failwith "go away"
  

                                     
let apply_hints
      tag
      typ
      (h_opt : (trace -> trace list) option)
      m
      pline pinst : (cmd * (Row.action_data * int) StringMap.t option) list =
  match h_opt with
  | None -> [Instance.apply tag typ pinst pline |> fst, None]
  | Some h ->
     List.map (h m) ~f:(fun t -> (compute_cand_for_trace typ pline pinst t, Some t))




let rec project_cmd_on_acts c (subst : expr StringMap.t) : cmd list =
  (* Printf.printf "PROJECTING\n%!"; *)
  let holes = true in
  match c with
  | Skip -> [c]
  | Assume b ->
     begin  match subst |> substitute ~holes b with
     | False -> []
     | b' -> [Assume b']
     end
  | Assert b -> [subst |> substitute ~holes b |> Assert]
  | Assign (v, e) ->
     begin match StringMap.find subst v with
     | None -> [v %<-% e]
     | Some _ ->
        [v %<-% e]
        (* Printf.printf "Replacing Assignment in %s with %s " v (string_of_expr ev);
         * begin match ev %=% e with
         * | False -> []
         * | t -> [Assume t]
         * end *)
     end
  | Seq (c1,c2) ->
     liftL2 mkSeq
       (project_cmd_on_acts c1 subst)
       (project_cmd_on_acts c2 subst)
  | Select (typ, cs) ->
     let open List in
     cs >>= (fun (t, a) ->
       project_cmd_on_acts a subst >>= fun act ->
       let t' = substitute ~holes t subst in
       if t' = False then [] else [(t', act)])
     |> mkSelect typ
     |> return 
  | Apply _ -> failwith "Shouldnt have applys at this stage"
  | While _ -> failwith "idk what to do with while loops"
     
              
let compute_candidates h pkt phys =
  match h with
  | None -> [phys]
  | Some f ->
     (* Printf.printf "Compute the candidates\n%!"; *)
     let action_mapping =
       let p = StringMap.filter_keys pkt ~f:(String.is_prefix ~prefix:"?ActIn") in
       (* Printf.printf "action mapping is %s" (Packet.string__packet p);        *)
       p |> StringMap.map ~f:(fun v -> Value v)
     in
     List.(f action_mapping >>= project_cmd_on_acts phys)

      
