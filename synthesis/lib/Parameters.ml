open Core
open Util

type t =
  { (* interaction *)
    interactive: bool
  ; thrift_mode: bool
  ; hot_start: bool
  ; (*bounded search*)
    edits_depth: int
  ; search_width: int
  ; timeout: Timeout.t
  ; (* Verifications *)
    fastcx: bool
  ; vcache: bool
  ; ecache: int option
  ; read_ecache: string option
  ; write_ecache: string option
  ; aggro_freshen: bool
  ; minimize: bool
  ; do_slice: bool
  ; semantic_slicing: bool
  ; shortening: bool
  ; solve_strat: string list
  ; (*Model finding*)
    widening: bool
  ; monotonic: bool
  ; injection: bool
  ; hints: bool
  ; hint_type: string
  ; only_holes: bool
  ; unique_edits: bool
  ; domain: bool
  ; restrict_mask: bool
  ; no_defaults: bool
  ; no_deletes: bool
  ; use_all_cexs: bool
  ; reach_restrict: bool
  ; reach_filter: bool
  ; (* outdated *)
    restr_acts: bool
  ; allow_annotations: bool
  ; nlp: bool
  ; above: bool }

let default =
  { widening= false
  ; do_slice= false
  ; hot_start= false
  ; semantic_slicing= false
  ; vcache= false
  ; ecache= None
  ; read_ecache= None
  ; write_ecache= None
  ; aggro_freshen= false
  ; shortening= false
  ; edits_depth= -1
  ; search_width= -1
  ; thrift_mode= false
  ; monotonic= false
  ; injection= false
  ; interactive= false
  ; fastcx= false
  ; above= false
  ; minimize= false
  ; hints= false
  ; hint_type= ""
  ; only_holes= false
  ; restrict_mask= false
  ; no_defaults= false
  ; no_deletes= false
  ; use_all_cexs= false
  ; reach_restrict= false
  ; reach_filter= false
  ; restr_acts= false
  ; allow_annotations= false
  ; nlp= false
  ; unique_edits= false
  ; domain= false
  ; timeout= Timeout.start None
  ; solve_strat= [] }

let restart_timer p = {p with timeout= Timeout.restart p.timeout}

let ecache_union ec1 ec2 =
  match (ec1, ec2) with
  | None, None -> None
  | Some i1, Some i2 -> Some (max i1 i2)
  | None, Some i | Some i, None -> Some i

let hint_type_union ht1 ht2 =
  if String.(ht1 = ht2) then ht1
  else if String.(ht1 = "") then ht2
  else if String.(ht2 = "") then ht1
  else Printf.sprintf "Incompatible %s and %s" ht1 ht2 |> failwith

let filename_union f1 f2 =
  match f1, f2 with
  | None, None -> None
  | Some f, None | None, Some f -> Some f
  | Some f, Some f' -> if String.(f = f') then Some f else Printf.sprintf "Incompatible filenames %s and %s" f f' |> failwith

let union p1 p2 =
  { widening= p1.widening || p2.widening
  ; do_slice= p1.do_slice || p2.do_slice
  ; hot_start= p1.hot_start || p2.hot_start
  ; semantic_slicing= p1.semantic_slicing || p2.semantic_slicing
  ; vcache= p1.vcache || p2.vcache
  ; ecache= ecache_union p1.ecache p2.ecache
  ; read_ecache= filename_union p1.read_ecache p2.read_ecache
  ; write_ecache= filename_union p1.write_ecache p2.write_ecache
  ; aggro_freshen= p1.aggro_freshen || p2.aggro_freshen
  ; shortening= p1.shortening || p2.shortening
  ; edits_depth= max p1.edits_depth p2.edits_depth
  ; search_width= max p1.search_width p2.search_width
  ; thrift_mode= p1.thrift_mode || p2.thrift_mode
  ; monotonic= p1.monotonic || p2.monotonic
  ; injection= p1.injection || p2.injection
  ; interactive= p1.interactive || p2.interactive
  ; fastcx= p1.fastcx || p2.fastcx
  ; above= p1.above || p2.above
  ; minimize= p1.minimize || p2.minimize
  ; hints= p1.hints || p2.hints
  ; hint_type= hint_type_union p1.hint_type p2.hint_type
  ; only_holes= p1.only_holes || p2.only_holes
  ; restrict_mask= p1.restrict_mask || p2.restrict_mask
  ; no_defaults= p1.no_defaults || p2.no_defaults
  ; no_deletes= p1.no_deletes || p2.no_deletes
  ; use_all_cexs= p1.use_all_cexs || p1.use_all_cexs
  ; reach_restrict= p1.reach_restrict || p2.reach_restrict
  ; reach_filter= p1.reach_filter || p2.reach_filter
  ; restr_acts= p1.restr_acts || p2.restr_acts
  ; allow_annotations= p1.allow_annotations || p2.allow_annotations
  ; nlp= p1.nlp || p2.nlp
  ; unique_edits= p1.unique_edits || p2.unique_edits
  ; domain= p1.domain || p2.domain
  ; timeout= Timeout.union p1.timeout p2.timeout
  ; solve_strat= p1.solve_strat @ p2.solve_strat }

let to_string params =
  let s = if params.widening then "w_" else "" in
  let s = s ^ if params.monotonic then "m_" else "" in
  let s = s ^ if params.injection then "inj_" else "" in
  let s = s ^ if params.hints then "hints_" else "" in
  let s = s ^ if params.only_holes then "only_holes_" else "" in
  let s = s ^ if params.unique_edits then "unique_edits_" else "" in
  let s = s ^ if params.domain then "domain_" else "" in
  let s = s ^ if params.restrict_mask then "restrict_mask" else "" in
  let s = s ^ if params.no_deletes then "no_deletes" else "" in
  let s = s ^ if params.use_all_cexs then "use_all_cexs" else "" in
  s

let all_params params =
  List.map (range_ex 0 256) ~f:(fun i ->
      { params with
        widening= i land 1 > 0
      ; monotonic= i land 2 > 0
      ; injection= i land 4 > 0
      ; hints= i land 8 > 0
      ; only_holes= i land 16 > 0
      ; unique_edits= i land 32 > 0
      ; domain= i land 64 > 0
      ; restrict_mask= i land 128 > 0 } )
