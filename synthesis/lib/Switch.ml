open Core

let new_slicing = true

type t =
  {
    pipeline : Cmd.t ref;         (* switch program *)
    inst : Instance.t ref;      (* table rules *)
    edits : Edit.t list;        (* edits to table rules *)
    (* cached stuff *)
    gcl : Cmd.t option ref;       (* switch program with table rules inlined *)
    gcl_holes : Cmd.t option ref; (* switch program with table rules inlined and holes added *)
    dels : Instance.interp option ref;   (* ??? *)
    tag : [`Mask | `Exact] option ref;   (* flag determining whether holes can be masked *)
    edited_inst : Instance.t option ref; (* inst with edits applied *)
    do_slice : bool;
    drop_spec : Test.t option;             (* dead code? *)
  }

let make ?drop_spec:(drop_spec = None) pipeline inst edits : t =
  {pipeline = ref pipeline;
   inst = ref inst;
   edits;
   gcl = ref None;
   gcl_holes = ref None;
   dels = ref None;
   tag = ref None;
   edited_inst = ref None;
   do_slice = false;
   drop_spec;
  }

let pipeline p = !(p.pipeline)
let inst p = !(p.inst)
let edits p = p.edits
let drop_spec p = p.drop_spec

let edited_instance params (p : t) =
  match !(p.edited_inst) with
  | None ->
     let i = Instance.update_list params !(p.inst) p.edits in
     p.edited_inst := Some i;
     i
  | Some i -> i

let to_gcl (params : Parameters.t) (_ : (string * int) list) (p : t) =
  if p.do_slice then
    (* let edited_inst = edited_instance params p in
     * let ghost_edits = List.fold (edits p) ~init:StringMap.empty ~f:(fun acc e ->
     *                       let table = Edit.table e in
     *                       StringMap.set acc ~key:table ~data:(
     *                           let ms = Edit.get_matches_exn e in
     *                           Instance.get_rows edited_inst table
     *                           |> get_indices_matching ~f:(fun (ms', _,_) -> List.equal Match.equal ms ms')
     *                         )
     *                     ) in
     * let ghostly =
     *   Instance.apply ~no_miss:false ~ghost_edits params NoHoles `Exact edited_inst (pipeline p)
     * in
     * let () = Printf.printf "instrumented_program: %s\n%!" (string_of_cmd ghostly) in
     * let c  =
     *   ghostly
     *   |> StaticSlicing.static_slice (StringSet.of_list @@ fsts fvs)
     *   |> StaticSlicing.ghost_static_slice ghost_edits
     *   |> ConstantProp.propogate in *)
    (* let propd_line = ConstantProp.propogate (pipeline p) in *)
    if params.semantic_slicing then
      (* let () = Printf.printf "semantic slicing\n%!" in *)
      StaticSlicing.edit_slice params (inst p) (edits p) (pipeline p)
    else
      (* let () = Printf.printf "rule slicing\n%!" in *)
      let slice = StaticSlicing.rule_slice (params) (edited_instance params p) (edits p) (pipeline p) in
      let c = Instance.apply params ~no_miss:true NoHoles `Exact (slice) (pipeline p) in
      (* let () = Printf.printf "Sliced program has %d nodes\n%!" (num_nodes_in_cmd c) in *)
      c
  else
    match !(p.gcl) with
    | None ->
       let i = Instance.apply params NoHoles `Exact (edited_instance params p) !(p.pipeline) in
       p.gcl := Some i;
       i
    | Some i -> i


let to_gcl_holes params (p : t) dels tag =
  match !(p.gcl_holes), !(p.dels), !(p.tag) with
  | Some i, Some d, Some t when Instance.interp_equal d dels && Stdlib.(t = tag) -> i
  | _ ->
     let i = Instance.apply params ~no_miss:false dels tag (edited_instance params p) !(p.pipeline) in
     p.gcl_holes := Some i;
     p.dels := Some dels;
     p.tag := Some tag;
     i

let to_string (_ : Parameters.t) (p : t) =
  Printf.sprintf "%s\n[%s]\n%!"
    (Cmd.to_string !(p.pipeline))
    (List.fold (p.edits) ~init:""
       ~f:(fun acc e ->
         Printf.sprintf "%s%s%s" acc (if String.(acc = "") then "" else "\n") (Edit.to_string e)
       ))

let clear_cache (p : t) = {p with
                            gcl = ref None;
                            gcl_holes = ref None;
                            edited_inst = ref None}

let replace_edits (p : t) (edits : Edit.t list) = {p with edits} |> clear_cache
let append_edits (p : t) (edits : Edit.t list) = {p with edits = p.edits @ edits} |> clear_cache

let update_inst params (p : t) (edits : Edit.t list) =
  p.inst := Instance.update_list params !(p.inst) edits;
  p |> clear_cache

let replace_inst (p : t) (i : Instance.t) =
  {p with inst = ref i}
  |> clear_cache

let slice_old params p =
  let inst_slice = Instance.of_edits params p.edits in
  Instance.overwrite (inst p) inst_slice
  |> replace_inst p

let slice_new p =
  {p with do_slice = true}

let slice params =
  if new_slicing then
    slice_new
  else
    slice_old params


let replace_pipeline (p : t) (c : Cmd.t) =
  {p with pipeline = ref c}
  |> clear_cache


let commit_edits params (p : t) =
  let p' = update_inst params p (p.edits) in
  let p'' = replace_edits p' [] in
  p''.gcl := !(p.gcl);
  p''
