open Core
open Ast
open Tables
open Util

type t = (Edit.t * Edit.t list) list


let make () : t = []


let similar (eold : Edit.t) (enew : Edit.t) =
  match eold, enew with
  | Add (ot, (oms, ods, oaid)), Add (nt, (nms, nds, naid))
       when ot = nt && oaid = naid && ods = nds ->
     List.fold2 oms nms ~init:(Some StringMap.empty)
       ~f:(fun acc om nm ->
         match acc with
         | None -> None
         | Some acc ->
            if om = nm then Some acc else
              match om, nm with
              | Exact ov, Exact nv ->
                 let ov_s = string_of_value ov in
                 begin
                   match StringMap.find acc ov_s with
                   | Some nv' -> if nv' = nv then Some acc else None
                   | None ->
                      StringMap.set acc ov_s nv |> Some
                 end
              | Mask (ov, om), Mask (nv,nm) ->
                 let ov_s = string_of_value ov in
                 let om_s = string_of_value om in
                 begin match StringMap.find acc ov_s, StringMap.find acc om_s with
                 | Some nv', Some nm' -> if nv' = nv && nm' = nm then Some acc else None
                 | None, None ->
                    StringMap.(set (set acc ov_s nv) om_s nm) |> Some
                 | _, _ -> None
                 end
              |  _ , _ -> None
       )
     |> or_unequal_lengths_to_option |> Option.join
  | _, _ -> None


let sub_consts (map : value StringMap.t) (e : Edit.t) : Edit.t option =
  match e with
  | Del _ -> None
  | Add (table, (ms, ad, idx)) ->
     let ms' = List.fold ms ~init:(Some []) ~f:(fun acc m ->
                   match acc with
                   | None -> None
                   | Some acc ->
                      match m with
                      | Exact v ->
                         let v_str = string_of_value v in
                         begin match StringMap.find map v_str with
                         | None -> acc @ [m] |> Some
                         | Some v' -> acc @ [Exact v'] |> Some
                         end
                      | Mask (v, msk) ->
                         let v_str = string_of_value v in
                         let msk_str = string_of_value msk in
                         begin match StringMap.find map v_str, StringMap.find map msk_str with
                         | None,None | None, Some _ ->
                            acc @ [m] |> Some
                         | Some v', None ->
                            acc @ [Mask(v',msk)] |> Some
                         | Some v', Some msk' ->
                            acc @ [Mask (v',msk')] |> Some
                         end
                      | _ -> None
                 ) in
     match ms' with
     | None -> None
     | Some ms' -> Add (table, (ms', ad, idx)) |> Some


let infer (cache : t) (e : Edit.t) =
  List.find_map cache
    ~f:(fun (log_edit, phys_edits) ->
      match similar log_edit e with
      | None -> None
      | Some subst ->
         List.fold phys_edits ~init:(Some [])
           ~f:(fun acc e -> match acc with
                            | None -> None
                            | Some acc ->
                               (* Printf.printf "%s\n%!" (Edit.to_string e); *)
                               match sub_consts subst e with
                               | None -> None
                               | Some e' ->
                                  (* Printf.printf "%s\n%!" (Edit.to_string e'); *)
                                  acc @ [e'] |> Some))


let update (cache : t) (log : Edit.t) (physs : Edit.t list) : t =
  if List.exists cache ~f:(fun (_,ps) ->  ps = physs)
  then cache
  else (log, physs) :: cache
