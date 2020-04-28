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

let make ~log ~phys ~fvs ~log_inst ~phys_inst ~log_edits =
  {log = Switch.make log log_inst log_edits;
   phys = Switch.make phys phys_inst [];
   fvs;
   cexs = [];
   attempts = [];
   model_space = True }



let to_string (p : t) =
  Printf.sprintf "+-------------------------+\nLogical:\n%s\n\nPhysical:\n%s\nWRT:[%s]\n+-------------------------------+\n"
    (Switch.to_string p.log)
    (Switch.to_string p.phys)
    (List.map p.fvs ~f:(fun (x,sz) -> "(" ^ x ^ "#" ^ string_of_int sz ^ ")")
     |> List.reduce ~f:(fun x y -> x ^","^ y)
     |> Option.value ~default:"")

let fvs (p : t) : (string * int) list = p.fvs
let cexs (p : t) : (Packet.t * Packet.t) list = p.cexs
let model_space (p : t) : test = p.model_space
let attempts (p : t) : value StringMap.t list = p.attempts

let log (p : t) : cmd = Switch.pipeline p.log
let log_inst (p : t) : Instance.t = Switch.inst p.log
let log_edits (p : t) : Edit.t list = Switch.edits p.log
let log_edited_instance (p : t) : Instance.t = Switch.edited_instance p.log
let log_gcl_program (p : t) : cmd = Switch.to_gcl p.log

let phys (p : t) : cmd = Switch.pipeline p.phys
let phys_inst (p : t) : Instance.t = Switch.inst p.phys
let phys_edits (p : t) : Edit.t list = Switch.edits p.phys
let phys_edited_instance (p : t) : Instance.t = Switch.edited_instance p.phys
let phys_gcl_program (p : t) : cmd = Switch.to_gcl p.phys

let phys_gcl_holes (p : t) dels tag : cmd = Switch.to_gcl_holes p.phys dels tag

let slice (p : t) : t =
  let log_inst_slice = Instance.update_list Instance.empty (Switch.edits p.log) in
  let phys_inst_slice = Instance.update_list Instance.empty (Switch.edits p.phys) in
  let log = Instance.overwrite (Switch.inst p.log) log_inst_slice |> Switch.replace_inst p.log in
  let phys = Instance.overwrite (Switch.inst p.phys) phys_inst_slice |> Switch.replace_inst p.phys in
  (* Printf.printf "PROBLEM:\n%s\n%!" (to_string p); *)
  let p = {p with log; phys} in
  (* Printf.printf "SLICED PROBLEM:\n%s\n%!" (to_string p); *)
  p

let append_phys_edits (p : t) (es : Edit.t list) : t =
  {p with phys = Switch.append_edits p.phys es}

let replace_log_edits (p : t) (log_edits : Edit.t list) : t =
  {p with log = Switch.replace_edits p.log log_edits}

let replace_phys_edits (p : t) (phys_edits : Edit.t list) : t =
  {p with phys = Switch.replace_edits p.phys phys_edits}

let delete_phys_edits (p : t) : t = replace_phys_edits p []

let reset_attempts (p : t) : t = {p with attempts = []}
let add_attempt (p : t) (attempt : value StringMap.t) : t =
  {p with attempts = attempt :: p.attempts}

let add_cex (p : t) (cex : (Packet.t * Packet.t)) : t =
  {p with cexs = cex :: p.cexs}

let reset_model_space (p : t) : t =
  {p with model_space = True}

let refine_model_space (p : t) (b : test) : t =
  {p with model_space = p.model_space %&% b }

let set_model_space (p : t) (model_space : test) : t =
  {p with model_space}

let apply_edits_to_log (p : t) (es : Edit.t list) : t =
  {p with log = Switch.update_inst p.log es}

let apply_edits_to_phys (p : t) (es : Edit.t list) : t =
  {p with phys = Switch.update_inst p.phys es}

let update_phys (p : t) (phys_cmd : cmd) : t =
  {p with phys = Switch.replace_pipeline p.phys phys_cmd}

let attempts_to_string (p : t) : string =
  List.map p.attempts ~f:(string_of_map)
  |> List.fold ~init:"" ~f:(Printf.sprintf "%s\n%s")

let num_attempts (p : t) : int = List.length p.attempts
