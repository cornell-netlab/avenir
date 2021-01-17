open Core
open Util
open Manip

type trace = (Row.action_data * int) StringMap.t

let rec compute_cand_for_trace (tag : [`Exact | `Mask]) (line : Cmd.t)
    (pinst : Instance.t) (trace : trace) : Cmd.t =
  let open Cmd in
  match line with
  | Skip | Assume _ | Assign _ -> line
  | Seq (c1, c2) ->
      compute_cand_for_trace tag c1 pinst trace
      %:% compute_cand_for_trace tag c2 pinst trace
  | Select (typ, cs) ->
      List.map cs ~f:(fun (b, c) ->
          (b, compute_cand_for_trace tag c pinst trace))
      |> select typ
  | Apply t -> (
    (*might need to use existing instance to negate prior rules *)
    match StringMap.find trace t.name with
    | None -> Assume False
    | Some (data, act_idx) ->
        if act_idx >= List.length t.actions then
          (*Assert (misses) %:%*)
          t.default
        else
          let actSize = max (log2 (List.length t.actions)) 1 in
          let cond = Hole.table_hole tag t.keys t.name act_idx actSize in
          let _, params, act = List.nth_exn t.actions act_idx in
          let args =
            let open Test in
            List.fold2_exn params data ~init:True ~f:(fun acc param arg ->
                acc %&% (Hole param %=% Value arg))
          in
          assume cond %:% assume args %:% holify List.(params >>| fst) act )

let apply_hints params tag typ (h_opt : (trace -> trace list) option) m pline
    pinst : (Cmd.t * (Row.action_data * int) StringMap.t option) list =
  match h_opt with
  | None -> [(Instance.apply params tag typ pinst pline, None)]
  | Some h ->
      List.map (h m) ~f:(fun t ->
          (compute_cand_for_trace typ pline pinst t, Some t))

let rec project_cmd_on_acts c (subst : Expr.t StringMap.t) : Cmd.t list =
  let open Cmd in
  let holes = true in
  match c with
  | Skip -> [c]
  | Assume b -> (
    match subst |> substitute ~holes b with False -> [] | b' -> [Assume b'] )
  | Assign (v, e) -> (
    match StringMap.find subst v with
    | None -> [v %<-% e]
    | Some _ -> [v %<-% e] )
  | Seq (c1, c2) ->
      liftL2 seq
        (project_cmd_on_acts c1 subst)
        (project_cmd_on_acts c2 subst)
  | Select (typ, cs) ->
      let open List in
      cs
      >>= (fun (t, a) ->
            project_cmd_on_acts a subst
            >>= fun act ->
            let t' = substitute ~holes t subst in
            if Test.equals t' False then [] else [(t', act)])
      |> select typ |> return
  | Apply _ -> failwith "Shouldnt have applys at this stage"

let compute_candidates h pkt phys =
  match h with
  | None -> [phys]
  | Some f ->
      let action_mapping =
        let p =
          StringMap.filter_keys pkt ~f:(String.is_prefix ~prefix:"?ActIn")
        in
        p |> StringMap.map ~f:(fun v -> Expr.Value v)
      in
      List.(f action_mapping >>= project_cmd_on_acts phys)
