open Core
open Util
open Option.Let_syntax

type mapping = (Edit.t * Edit.t list) list

type t = mapping option ref

let cache : t = ref None

let make () : unit = cache := Some []

let clear () = make ()

let get_cache () =
  match !cache with
  | None -> failwith "Tried to access cache, but it was uninitialized"
  | Some m -> m

module ValueMap = Map.Make (Value)
module MatchMap = Map.Make (Match)

(** [abstract_data old_data new_data] returns a mapping from values in
    old_data to values in new_data. A pair [od ↦ nd] is added if the values
    are different and occur at the same place in the list and there isn't a
    previous pair in the map [od -> nd'] where [nd <> nd'] in which case it
    returns [None]. It also returns None if the lists are different lengths*)
let abstract_data (old_data : Row.action_data) (new_data : Row.action_data) =
  List.fold2 old_data new_data ~init:(Some ValueMap.empty)
    ~f:(fun acc_opt old_ new_ ->
      let%bind acc = acc_opt in
      if Value.equals old_ new_ then Some acc
      else
        match ValueMap.find acc old_ with
        | Some nd' when Value.equals nd' new_ -> Some acc
        | Some _ -> None
        | None -> ValueMap.set acc ~key:old_ ~data:new_ |> Some)
  |> or_unequal_lengths_to_option |> Option.join

(** [abstract_matches old_matches new_matches] returns a mapping from values
    in old_matches to values in new_matches. A pair [om ↦ nm] is added if
    the values are different and occur at the same place in the list and
    there isn't a previous pair in the map [om -> nm'] where [nm <> nm'], in
    which case it returns [None]. It also returns [None] if the lists are
    different lengths*)
let abstract_matches (old_matches : Match.t list)
    (new_matches : Match.t list) =
  List.fold2 old_matches new_matches ~init:(Some MatchMap.empty)
    ~f:(fun acc_opt old_ new_ ->
      let%bind acc = acc_opt in
      if Match.equal old_ new_ then Some acc
      else
        match MatchMap.find acc old_ with
        | Some new_' when Match.equal new_' new_ -> Some acc
        | Some _ -> None
        | None -> MatchMap.set acc ~key:old_ ~data:new_ |> Some)
  |> or_unequal_lengths_to_option |> Option.join

let similar (eold : Edit.t) (enew : Edit.t) =
  match (eold, enew) with
  | Add (ot, (oms, ods, oaid)), Add (nt, (nms, nds, naid))
    when String.(ot = nt) && oaid = naid ->
      let%map matches = abstract_matches oms nms in
      let adata = abstract_data ods nds in
      (adata, matches)
  | _, _ -> None

(** [sub_matches map ms] applies the mapping [map] to the matches [ms] *)
let sub_matches map =
  List.map ~f:(fun m ->
      match MatchMap.find map m with
      | None -> m
      | Some m' -> m')

(** [sub_action_data map data] applies the mapping [map] to action data
    [data]*)
let sub_action_data map =
  List.map ~f:(fun d ->
      match ValueMap.find map d with
      | None -> d
      | Some d' -> d')

(** [sub_edit adata map e] applies the match mapping [map] and the data
    mapping adata to the edit [e]*)
let sub_edit (adata : Value.t ValueMap.t option) (map : Match.t MatchMap.t)
    (e : Edit.t) : Edit.t option =
  match e with
  | Del _ -> None
  | Add (table, (ms, ad, idx)) -> (
      let ms' = sub_matches map ms in
      match adata with
      | None -> Add (table, (ms', ad, idx)) |> Some
      | Some dmap ->
          let ad' = sub_action_data dmap ad in
          Add (table, (ms', ad', idx)) |> Some )

(* let print_template subst =
 *   StringMap.iteri subst ~f:(fun ~key ~data ->
 *       Printf.eprintf "\t%s->%s\n%!" key (Match.to_string data)) *)

let sub_edits (adata, subst) edits =
  List.fold edits ~init:(Some []) ~f:(fun acc_opt e ->
      let%bind acc = acc_opt in
      let%map e' = sub_edit adata subst e in
      acc @ [e'])

let equivalences diffmap : StringSet.t list =
  StringMap.fold diffmap ~init:[] ~f:(fun ~key ~data eq_classes ->
      if List.exists eq_classes ~f:(Fn.flip StringSet.mem key) then
        eq_classes
      else
        StringMap.fold diffmap ~init:(StringSet.singleton key)
          ~f:(fun ~key:key_inner ~data:data_inner acc ->
            if Stdlib.(data = data_inner) then StringSet.add acc key_inner
            else acc)
        :: eq_classes)

let diff_eq_classes curr_edit_model old_edit_models =
  List.find_map old_edit_models ~f:(fun old_model ->
      if Model.equal curr_edit_model old_model then None
      else
        Model.diff curr_edit_model old_model
        |> equivalences
        |> List.filter ~f:(fun s -> StringSet.length s > 1)
        |> some_ident_if ~f:(Fn.non List.is_empty))

let char_map ecs =
  List.fold ecs ~init:StringMap.empty ~f:(fun acc ec ->
      StringMap.set acc ~key:(StringSet.choose_exn ec) ~data:[])

let randomized_model ~excluding chis =
  (* Pick characteristic elements for each eq-class *)
  (* prohibit used values*)
  let prohibited : Value.t list StringMap.t =
    List.fold excluding ~init:chis ~f:Model.extend_multi_model
  in
  (* randomly generate a new valuation not in the prohibited values *)
  StringMap.fold prohibited ~init:Model.empty ~f:(fun ~key ~data:prohibs m ->
      let random_x =
        random_int_nin (List.map prohibs ~f:Value.get_int_exn)
      in
      Model.set m ~key
        ~data:(Value.make (random_x, Value.size (List.hd_exn prohibs))))

(** expand characteristic valuation to each member of the equivalence class *)
let expand eqs chis =
  Model.fold chis ~init:Model.empty ~f:(fun ~key ~data acc ->
      match List.find eqs ~f:(Fn.flip StringSet.mem key) with
      | None -> Model.set acc ~key ~data
      | Some eqs ->
          StringSet.fold eqs ~init:acc ~f:(fun acc key ->
              Model.set acc ~key ~data))

let inferred_models phys substs editss =
  List.filter_map editss ~f:(fun es ->
      let%map es' = sub_edits substs es in
      Edit.list_to_model phys es')

let infer_fresh phys (curr_edits : Edit.t list) substs
    (old_edits : Edit.t list list) : Edit.t list option =
  let curr_model = Edit.list_to_model phys curr_edits in
  let old_models = inferred_models phys substs old_edits in
  let%map ecs = diff_eq_classes curr_model old_models in
  char_map ecs
  |> randomized_model ~excluding:old_models
  |> expand ecs
  |> Model.right_union curr_model
  |> Edit.of_model phys

let get_similars e =
  get_cache ()
  |> List.filter_map ~f:(fun (log_edit, phys_edits) ->
         let%map adata, subst = similar log_edit e in
         (phys_edits, adata, subst))

let infer (params : Parameters.t) (phys : Cmd.t) (e : Edit.t) =
  let%bind min_match_size = params.ecache in
  let similars = get_similars e in
  if List.length similars < min_match_size then None
  else
    let prev_solns = List.map similars ~f:fst3 in
    List.find_map similars ~f:(fun (phys_edits, adata, subst) ->
        (* apply mapping to physical edit list *)
        let%map phys_edits' = sub_edits (adata, subst) phys_edits in
        (* attempt to abstract over physical edit history*)
        infer_fresh phys phys_edits' (adata, subst) prev_solns
        |> Option.value ~default:phys_edits')

let update (log : Edit.t) (physs : Edit.t list) : unit =
  let c = get_cache () in
  if not (List.exists c ~f:(fun (_, ps) -> Edit.equal ps physs)) then
    cache := Some ((log, physs) :: c)
