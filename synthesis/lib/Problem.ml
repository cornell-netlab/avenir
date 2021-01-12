open Core

type t =
  { (* logical program *)
    log: Switch.t
  ; (* physical program *)
    phys: Switch.t
  ; (* input-output counterexamples (for logical program) *)
    cexs: (Packet.t * Packet.t) list
  ; (* formula encoding search space that has been traversed *)
    model_space: Test.t
  ; (* previously obtained models *)
    attempts: Model.t list
  ; (* variables used to check equality, and their widths *)
    fvs: (string * int) list }

let make ?(phys_drop_spec = None) ~log ~phys ~fvs ~log_inst ~phys_inst
    ~log_edits () =
  { log= Switch.make log log_inst log_edits
  ; phys= Switch.make phys phys_inst [] ~drop_spec:phys_drop_spec
  ; fvs
  ; cexs= []
  ; attempts= []
  ; model_space= True }

let empty =
  make ~log:Skip ~phys:Skip ~fvs:[] ~log_inst:Instance.empty
    ~phys_inst:Instance.empty ~log_edits:[] ()

let to_string params (p : t) =
  Printf.sprintf
    "+-------------------------+\n\
     Logical:\n\
     %s\n\n\
     Physical:\n\
     %s\n\
     WRT:[%s]\n\
     Search Space is: %s paths\n\
     There are %d actions\n\
     across %d tables\n\
     +-------------------------------+\n"
    (Switch.to_string params p.log)
    (Switch.to_string params p.phys)
    ( List.map p.fvs ~f:(fun (x, sz) ->
          "(" ^ x ^ "#" ^ string_of_int sz ^ ")")
    |> List.reduce ~f:(fun x y -> x ^ "," ^ y)
    |> Option.value ~default:"" )
    (Bigint.to_string @@ Cmd.num_table_paths @@ Switch.pipeline p.phys)
    (List.length @@ Cmd.get_actions @@ Switch.pipeline p.phys)
    (List.length @@ Cmd.tables @@ Switch.pipeline p.phys)

let fvs (p : t) : (string * int) list = p.fvs

let cexs (p : t) : (Packet.t * Packet.t) list = p.cexs

let add_cex (p : t) cex = {p with cexs= cex :: p.cexs}

let model_space (p : t) : Test.t = p.model_space

let attempts (p : t) : Model.t list = p.attempts

let log (p : t) : Cmd.t = Switch.pipeline p.log

let log_inst (p : t) : Instance.t = Switch.inst p.log

let log_edits (p : t) : Edit.t list = Switch.edits p.log

let log_edited_instance params (p : t) : Instance.t =
  Switch.edited_instance params p.log

let log_gcl_program params (p : t) : Cmd.t =
  Switch.to_gcl params (fvs p) p.log

let phys (p : t) : Cmd.t = Switch.pipeline p.phys

let phys_inst (p : t) : Instance.t = Switch.inst p.phys

let phys_edits (p : t) : Edit.t list = Switch.edits p.phys

let phys_edited_instance params (p : t) : Instance.t =
  Switch.edited_instance params p.phys

let phys_gcl_program params (p : t) : Cmd.t =
  Switch.to_gcl params (fvs p) p.phys

let phys_gcl_holes params (p : t) dels tag : Cmd.t =
  Switch.to_gcl_holes params p.phys dels tag

let phys_drop_spec (p : t) : Test.t option = Switch.drop_spec p.phys

let slice params (p : t) : t =
  (* let log_inst_slice = Instance.update_list params Instance.empty (Switch.edits p.log) in
   * let phys_inst_slice = Instance.update_list params Instance.empty (Switch.edits p.phys) in
   * let log = Instance.overwrite (Switch.inst p.log) log_inst_slice |> Switch.replace_inst p.log in
   * let phys = Instance.overwrite (Switch.inst p.phys) phys_inst_slice |> Switch.replace_inst p.phys in
   * (\* if params.debug then *\)
   *   Printf.printf "SLICED PROBLEM:\n%s\n===??====\n%s\n%!"
   *     (Switch.to_gcl params log |> string_of_cmd)
   *     (Switch.to_gcl params phys |> string_of_cmd); *)
  if Edit.has_delete (log_edits p @ phys_edits p) then
    (* let () = Printf.printf "delete detected, slicing inapplicable\n%!" in *)
    p
  else
    (* let () = Printf.printf "slicing away!" in *)
    {p with log= Switch.slice params p.log; phys= Switch.slice params p.phys}

let append_phys_edits (p : t) (es : Edit.t list) : t =
  {p with phys= Switch.append_edits p.phys es}

let append_log_edits (p : t) (es : Edit.t list) : t =
  {p with log= Switch.append_edits p.log es}

let empty_log_edits (p : t) : bool = List.is_empty (Switch.edits p.log)

let empty_phys_edits (p : t) : bool = List.is_empty (Switch.edits p.phys)

let replace_log_edits (p : t) (log_edits : Edit.t list) : t =
  {p with log= Switch.replace_edits p.log log_edits}

let replace_phys_edits (p : t) (phys_edits : Edit.t list) : t =
  {p with phys= Switch.replace_edits p.phys phys_edits}

let delete_phys_edits (p : t) : t = replace_phys_edits p []

let commit_edits_phys params (p : t) : t =
  {p with phys= Switch.commit_edits params p.phys}

let commit_edits_log params (p : t) : t =
  {p with log= Switch.commit_edits params p.log}

let set_attempts (p : t) attempts = {p with attempts}

let reset_attempts (p : t) : t =
  (* Printf.printf "RESETTING ATTEMPTS\n%!"; *)
  set_attempts p []

let add_attempt (p : t) (attempt : Model.t) : t =
  (* Printf.printf "ADDING ATTEMPT\n%!"; *)
  set_attempts p @@ (attempt :: p.attempts)

let seen_attempt (p : t) (attempt : Model.t) : bool =
  List.exists p.attempts ~f:(Model.equal attempt)

let set_model_space (p : t) (model_space : Test.t) : t = {p with model_space}

let reset_model_space (p : t) : t =
  (* Printf.printf "RESETTING THE MODEL SPACE\n%!"; *)
  set_model_space p True

let refine_model_space (p : t) (b : Test.t) : t =
  (* Printf.printf "REFINING THE MODEL SPACE\n%!"; *)
  Test.and_ p.model_space b |> set_model_space p

let apply_edits_to_log params (p : t) (es : Edit.t list) : t =
  {p with log= Switch.update_inst params p.log es}

let apply_edits_to_phys params (p : t) (es : Edit.t list) : t =
  {p with phys= Switch.update_inst params p.phys es}

let update_phys (p : t) (phys_cmd : Cmd.t) : t =
  {p with phys= Switch.replace_pipeline p.phys phys_cmd}

let update_log (p : t) (log_cmd : Cmd.t) : t =
  {p with log= Switch.replace_pipeline p.log log_cmd}

let attempts_to_string (p : t) : string =
  List.map p.attempts ~f:Model.to_string
  |> List.fold ~init:"" ~f:(Printf.sprintf "%s,\n%s")

let num_attempts (p : t) : int = List.length p.attempts

let unique_in_table params (_ (*prog*) : Cmd.t) inst edits e =
  let open Edit in
  match e with
  | Del _ -> false
  | Add (tbl, (ms, _, _)) ->
      let index_of_e =
        List.findi edits ~f:(fun _ e' -> Edit.equal e e')
        |> Option.value_exn |> fst
      in
      let earlier_edits =
        List.filteri edits ~f:(fun i _ -> i < index_of_e)
      in
      let inst' = Instance.update_list params inst earlier_edits in
      let earlier_rows = Instance.get_rows inst' tbl in
      List.for_all earlier_rows ~f:(fun (ms', _, _) ->
          not (Match.has_inter_l ms ms'))

let exists_in_table params (_ (*prog*) : Cmd.t) inst edits e =
  let open Edit in
  match e with
  | Del _ -> false
  | Add (tbl, (ms, ad, aid)) ->
      let index_of_e =
        List.findi edits ~f:(fun _ e' -> Edit.equal e e')
        |> Option.value_exn |> fst
      in
      let earlier_edits =
        List.filteri edits ~f:(fun i _ -> i < index_of_e)
      in
      let inst' = Instance.update_list params inst earlier_edits in
      let earlier_rows = Instance.get_rows inst' tbl in
      List.exists earlier_rows ~f:(fun (ms', ad', aid') ->
          List.equal (fun m m' -> Match.equal m m') ms ms'
          && aid = aid'
          && List.equal (fun v v' -> Stdlib.(v = v')) ad ad')

(* The truth of a slice implies the truth of the full programs when
 * the inserted rules are disjoint with every previous rule (i.e. no overlaps or deletions)
 * Here we only check that the rules are exact, which implies this property given the assumption that every insertion is reachable
 *)
let slice_conclusive (params : Parameters.t) (data : ProfData.t ref)
    (problem : t) =
  let st = Time.now () in
  let log_edit = log_edits problem |> List.hd_exn in
  let res =
    unique_in_table params (log problem) (log_inst problem)
      (log_edits problem) log_edit
    && List.for_all (phys_edits problem) ~f:(fun e ->
           unique_in_table params (phys problem) (phys_inst problem)
             (phys_edits problem) e
           || exists_in_table params (phys problem) (phys_inst problem)
                (phys_edits problem) e)
  in
  ProfData.update_time !data.check_sliceable_time st ;
  res

let step_search_state problem es =
  append_phys_edits problem es |> reset_model_space |> reset_attempts

let negate_model_in_model_space p m es =
  let p = add_attempt p m in
  Edit.negate (phys p) m es |> refine_model_space p
