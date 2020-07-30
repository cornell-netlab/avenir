open Core
open Util
open Ast
open Tables

type t =
  {
    log : Switch.t;
    phys : Switch.t;
    cexs: (Packet.t * Packet.t) list;
    model_space : test;
    attempts : value StringMap.t list;
    fvs : (string * int) list
  }

let make ?phys_drop_spec:(phys_drop_spec = None) ~log ~phys ~fvs ~log_inst ~phys_inst ~log_edits () =
  {log = Switch.make log log_inst log_edits;
   phys = Switch.make phys phys_inst [] ~drop_spec:phys_drop_spec;
   fvs;
   cexs = [];
   attempts = [];
   model_space = True }

let empty = make
              ~log:Skip
              ~phys:Skip
              ~fvs:[]
              ~log_inst:Instance.empty
              ~phys_inst:Instance.empty
              ~log_edits:[]
              ()


let to_string params (p : t) =
  Printf.sprintf "+-------------------------+\nLogical:\n%s\n\nPhysical:\n%s\nWRT:[%s]\n+-------------------------------+\n"
    (Switch.to_string params p.log)
    (Switch.to_string params p.phys)
    (List.map p.fvs ~f:(fun (x,sz) -> "(" ^ x ^ "#" ^ string_of_int sz ^ ")")
     |> List.reduce ~f:(fun x y -> x ^","^ y)
     |> Option.value ~default:"")

let fvs (p : t) : (string * int) list = p.fvs
let cexs (p : t) : (Packet.t * Packet.t) list = p.cexs
let add_cex (p : t) cex = {p with cexs = cex::p.cexs}
let model_space (p : t) : test = p.model_space
let attempts (p : t) : value StringMap.t list = p.attempts

let log (p : t) : cmd = Switch.pipeline p.log
let log_inst (p : t) : Instance.t = Switch.inst p.log
let log_edits (p : t) : Edit.t list = Switch.edits p.log
let log_edited_instance params (p : t) : Instance.t = Switch.edited_instance params p.log
let log_gcl_program params (p : t) : cmd = Switch.to_gcl params p.log

let phys (p : t) : cmd = Switch.pipeline p.phys
let phys_inst (p : t) : Instance.t = Switch.inst p.phys
let phys_edits (p : t) : Edit.t list = Switch.edits p.phys
let phys_edited_instance params (p : t) : Instance.t = Switch.edited_instance params p.phys
let phys_gcl_program params (p : t) : cmd = Switch.to_gcl params p.phys

let phys_gcl_holes params (p : t) dels tag : cmd = Switch.to_gcl_holes params p.phys dels tag
let phys_drop_spec (p : t) : test option = Switch.drop_spec p.phys

let slice params (p : t) : t =
  let log_inst_slice = Instance.update_list params Instance.empty (Switch.edits p.log) in
  let phys_inst_slice = Instance.update_list params Instance.empty (Switch.edits p.phys) in
  let log = Instance.overwrite (Switch.inst p.log) log_inst_slice |> Switch.replace_inst p.log in
  let phys = Instance.overwrite (Switch.inst p.phys) phys_inst_slice |> Switch.replace_inst p.phys in
  (* Printf.printf "PROBLEM:\n%s\n%!" (to_string p); *)
  let p = {p with log; phys} in
  (* Printf.printf "SLICED PROBLEM:\n%s\n%!" (to_string params p); *)
  p

let append_phys_edits (p : t) (es : Edit.t list) : t =
  {p with phys = Switch.append_edits p.phys es}

let append_log_edits (p : t) (es : Edit.t list) : t =
  {p with log = Switch.append_edits p.log es}


let replace_log_edits (p : t) (log_edits : Edit.t list) : t =
  {p with log = Switch.replace_edits p.log log_edits}

let replace_phys_edits (p : t) (phys_edits : Edit.t list) : t =
  {p with phys = Switch.replace_edits p.phys phys_edits}

let delete_phys_edits (p : t) : t = replace_phys_edits p []

let commit_edits_phys params (p : t) : t = {p with phys = Switch.commit_edits params p.phys}
let commit_edits_log params (p : t) : t = {p with log = Switch.commit_edits params p.log}


let reset_attempts (p : t) : t = {p with attempts = []}
let add_attempt (p : t) (attempt : value StringMap.t) : t =
  {p with attempts = attempt :: p.attempts}
let seen_attempt (p : t)  (attempt : value StringMap.t) : bool =
  List.exists p.attempts ~f:(StringMap.equal veq attempt)

let reset_model_space (p : t) : t =
  {p with model_space = True}

let refine_model_space (p : t) (b : test) : t =
  {p with model_space = p.model_space %&% b }

let set_model_space (p : t) (model_space : test) : t =
  {p with model_space}

let apply_edits_to_log params (p : t) (es : Edit.t list) : t =
  {p with log = Switch.update_inst params p.log es}

let apply_edits_to_phys params (p : t) (es : Edit.t list) : t =
  {p with phys = Switch.update_inst params p.phys es}

let update_phys (p : t) (phys_cmd : cmd) : t =
  {p with phys = Switch.replace_pipeline p.phys phys_cmd}

let update_log (p : t) (log_cmd : cmd) : t =
  {p with log = Switch.replace_pipeline p.log log_cmd}


let attempts_to_string (p : t) : string =
  List.map p.attempts ~f:(string_of_map)
  |> List.fold ~init:"" ~f:(Printf.sprintf "%s\n%s")

let num_attempts (p : t) : int = List.length p.attempts
