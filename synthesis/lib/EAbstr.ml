open Core
open Util
open Option.Let_syntax
module ValueMap = Map.Make (Value)
module MatchMap = Map.Make (Match)

type mapping = (Edit.t * Edit.t list) list [@@deriving yojson]

let random_mapping () =
  let len = Random.int 16 in
  let random_entry _ =
    let re _ = Edit.random () in
    let entry_len = Random.int 8 in
    let first = Edit.random () in
    (first, List.init entry_len ~f:re) in
  List.init len ~f:random_entry

let equal (m : mapping) (m' : mapping) : bool =
  let entry_eq entry entry' =
    match entry, entry' with
    | (edit, edits), (edit', edits') when (Edit.equal edit edit') -> List.equal Edit.equal edits edits'
    | _ -> false in
  List.equal entry_eq m m'

let string_of_mapping (m : mapping) : string =
  let string_of_entry (e : Edit.t * Edit.t list) : string =
    let (x, y) = e in
    let init1 = (Edit.to_string x) ^ ":" in
    let fun1 = (fun a b -> a ^ "|" ^ (Edit.to_string b)) in
    List.fold y ~init: init1 ~f:fun1 in
  (List.map ~f:string_of_entry m) |> (String.concat ~sep:",")

type t = mapping option ref

let cache : t = ref None

let make ?(filename=None) () : unit =
  match filename with
  | None -> cache := Some []
  | Some file ->
    begin
      let json = Yojson.Safe.from_file file in
      Core.Printf.printf "%s" (Yojson.Safe.to_string json);
      match mapping_of_yojson (Yojson.Safe.from_file file) with
    | Ok c -> cache := Some c; Core.Printf.printf "%s" (string_of_mapping c)
    | Error e -> Core.Printf.printf "Failed to read in cache from file"; failwith e
    end

let clear () = make ()

let get_cache () =
  match !cache with
  | None -> failwith "Tried to access cache, but it was uninitialized"
  | Some m -> m

let dump_yojson (filename : string) : unit =
  (get_cache ()) |> mapping_to_yojson |> Yojson.Safe.to_file filename

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
      let sz = Value.size (List.hd_exn prohibs) in
      (* manually prohibit 0, usually it's not what you want when youre
         trying to create a random unique identifier -- it gets used for all
         sorts of things in practice *)
      let random_v = Value.random sz ~lo:1 ~exc:prohibs in
      Model.set m ~key ~data:random_v)

(** expand characteristic valuation to each member of the equivalence class *)
let expand eqs chis =
  Model.fold chis ~init:Model.empty ~f:(fun ~key ~data acc ->
      match List.find eqs ~f:(Fn.flip StringSet.mem key) with
      | None -> Model.set acc ~key ~data
      | Some eqs ->
          StringSet.fold eqs ~init:acc ~f:(fun acc key ->
              Model.set acc ~key ~data))

let randomize_along_ecs ecs ~excluding =
  Log.ecache
  @@ lazy
       (List.fold ecs ~init:"Randomizing Along Equivalence classes \n"
          ~f:(fun acc cls ->
            string_of_strset cls |> Printf.sprintf "%s\n\t{%s }" acc)) ;
  char_map ecs
  |> randomized_model ~excluding
  |> Log.(id_print ~s:Model.to_string ~p:ecache)
  |> expand ecs

let renew_equals (phys : Cmd.t) (olds : Edit.t list) (news : Edit.t list) =
  Log.ecache @@ lazy "[renew_equals]" ;
  let old_model = Edit.list_to_model phys olds in
  let new_model = Edit.list_to_model phys news in
  let old_pkt_model = Model.proj_packet_holes old_model in
  let new_pkt_model = Model.proj_packet_holes new_model in
  if Model.equal old_pkt_model new_pkt_model then (
    Log.ecache @@ lazy "Packets were equal, returning None" ;
    None )
  else
    let isect_model = Model.intersect old_pkt_model new_pkt_model in
    Log.ecache
    @@ lazy (Printf.sprintf "intersection %s" (Model.to_string isect_model)) ;
    let ecs = equivalences (Model.to_strmap isect_model) in
    let fresh_model = randomize_along_ecs ecs ~excluding:[isect_model] in
    Log.ecache
    @@ lazy (Printf.sprintf "perturbed %s" (Model.to_string fresh_model)) ;
    let renew_model = Model.right_union new_model fresh_model in
    Log.ecache
    @@ lazy (Printf.sprintf "new_model %s" (Model.to_string new_model)) ;
    Edit.of_model phys renew_model |> return

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
 *       Printf.printf "\t%s->%s\n%!" key (Match.to_string data)) *)

let sub_edits (adata, subst) edits =
  Log.ecache (lazy "trying new template, matched with") ;
  Log.ecache (lazy (Edit.list_to_string edits)) ;
  List.fold edits ~init:(Some []) ~f:(fun acc_opt e ->
      let%bind acc = acc_opt in
      let%map e' = sub_edit adata subst e in
      acc @ [e'])

let diff_eq_classes curr_edit_model old_edit_models =
  List.find_map old_edit_models ~f:(fun old_model ->
      if Model.equal curr_edit_model old_model then None
      else
        Model.diff curr_edit_model old_model
        |> equivalences
        |> List.filter ~f:(fun s -> StringSet.length s > 1)
        |> some_ident_if ~f:(Fn.non List.is_empty))

let inferred_models phys substs editss =
  List.filter_map editss ~f:(fun es ->
      let%map es' = sub_edits substs es in
      Edit.list_to_model phys es')

let infer_fresh phys (curr_edits : Edit.t list) substs
    (old_edits : Edit.t list list) : Edit.t list option =
  Log.ecache @@ lazy "[infer_fresh]" ;
  let curr_model = Edit.list_to_model phys curr_edits in
  Log.ecache
  @@ lazy (Printf.sprintf "current model %s" (Model.to_string curr_model)) ;
  let old_models = inferred_models phys substs old_edits in
  Log.ecache
  @@ lazy (Printf.sprintf "old model %s" (Model.to_string curr_model)) ;
  let%map ecs = diff_eq_classes curr_model old_models in
  randomize_along_ecs ecs ~excluding:old_models
  |> Log.(id_print ~s:Model.to_string ~p:ecache)
  |> Model.right_union curr_model
  |> Edit.of_model phys

let get_similars e =
  get_cache ()
  |> List.filter_map ~f:(fun (log_edit, phys_edits) ->
         let%map adata, subst = similar log_edit e in
         (phys_edits, adata, subst))

let freshen (params : Parameters.t) phys olds news substs prev_solns =
  Log.ecache
  @@ lazy (Printf.sprintf "guessing.... %s" (Edit.list_to_string news)) ;
  match infer_fresh phys news substs prev_solns with
  | Some fresh_phys_edits -> fresh_phys_edits
  | None when params.aggro_freshen ->
      renew_equals phys olds news |> Option.value ~default:news
  | None -> news

let infer (params : Parameters.t) (phys : Cmd.t) (e : Edit.t) =
  let%bind min_match_size = params.ecache in
  Log.ecache (lazy (Printf.sprintf "checking edit %s..." (Edit.to_string e))) ;
  let similars = get_similars e in
  if List.length similars < min_match_size then None
  else
    let prev_solns = List.map similars ~f:fst3 in
    List.find_map similars ~f:(fun (old_edits, adata, subst) ->
        (* apply mapping to physical edit list *)
        let%map new_edits = sub_edits (adata, subst) old_edits in
        (* attempt to abstract over physical edit history*)
        let fresh_phys_edits =
          freshen params phys old_edits new_edits (adata, subst) prev_solns
        in
        fresh_phys_edits)

let update (log : Edit.t) (physs : Edit.t list) : unit =
  let c = get_cache () in
  if not (List.exists c ~f:(fun (_, ps) -> Edit.equal ps physs)) then
    cache := Some ((log, physs) :: c)
