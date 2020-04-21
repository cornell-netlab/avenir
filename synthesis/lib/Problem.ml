open Core
open Util
open Ast
open Tables

type t =
  {
    log : cmd;
    phys : cmd;
    log_inst : Instance.t;
    phys_inst : Instance.t;
    log_edits : Edit.t list;
    phys_edits : Edit.t list;
    cexs: (Packet.t * Packet.t) list;
    model_space : test;
    attempts : value StringMap.t list;
    fvs : (string * int) list
  }

let make ~log ~phys ~fvs ~log_inst ~phys_inst ~log_edits =
  {log; phys; fvs; log_inst; phys_inst; log_edits;
   phys_edits = [];
   cexs = [];
   attempts = [];
   model_space = True }



let to_string (p : t) =
  Printf.sprintf "+-------------------------+\nLogical:\n%s\n\nPhysical:\n%s\nWRT:[%s]\n+-------------------------------+\n"
    (Instance.apply `NoHoles `Exact (Instance.update_list p.log_inst p.log_edits) p.log
     |> fst |> string_of_cmd)
    (Instance.apply `NoHoles `Exact p.phys_inst p.phys
     |> fst |> string_of_cmd)
    (List.map p.fvs ~f:(fun (x,sz) -> "(" ^ x ^ "#" ^ string_of_int sz ^ ")")
     |> List.reduce ~f:(fun x y -> x ^","^ y)
     |> Option.value ~default:"")

let fvs (p : t) : (string * int) list = p.fvs
let cexs (p : t) : (Packet.t * Packet.t) list = p.cexs
let model_space (p : t) : test = p.model_space
let attempts (p : t) : value StringMap.t list = p.attempts


let gcl_prog get_inst p prog  =
  Instance.apply `NoHoles `Exact (get_inst p) prog |> fst


let log (p : t) : cmd = p.log
let log_inst (p : t) : Instance.t = p.log_inst
let log_edits (p : t) : Edit.t list = p.log_edits
let log_edited_instance (p : t) : Instance.t = Instance.update_list p.log_inst p.log_edits
let log_gcl_program (p : t) : cmd = gcl_prog log_edited_instance p p.log

let phys (p : t) : cmd = p.phys
let phys_inst (p : t) : Instance.t = p.phys_inst
let phys_edits (p : t) : Edit.t list = p.phys_edits
let phys_edited_instance (p : t) : Instance.t =
  Instance.update_list p.phys_inst p.phys_edits
let phys_gcl_program (p : t) : cmd = gcl_prog phys_edited_instance p p.phys

let phys_gcl_holes (p : t) dels tag : cmd =
  Instance.apply ~no_miss:false dels tag (phys_edited_instance p) p.phys
  |> fst

let slice (p : t) : t =
  let log_inst_slice = Instance.update_list Instance.empty p.log_edits in
  let phys_inst_slice = Instance.update_list Instance.empty p.phys_edits in
  let log_inst = Instance.overwrite p.log_inst log_inst_slice in
  let phys_inst = Instance.overwrite p.phys_inst phys_inst_slice in
  {p with log_inst; phys_inst;
          log_edits = []; phys_edits = [] }

let append_phys_edits (p : t) (es : Edit.t list) : t =
  {p with phys_edits = p.phys_edits @ es}

let replace_log_edits (p : t) (log_edits : Edit.t list) : t =
  {p with log_edits}

let replace_phys_edits (p : t) (phys_edits : Edit.t list) : t =
  {p with phys_edits}

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

let apply_edits_to_log (p : t) (e : Edit.t list) : t =
  {p with log_inst = Instance.update_list p.log_inst e}

let apply_edits_to_phys (p : t) (e : Edit.t list) : t =
  {p with phys_inst = Instance.update_list p.phys_inst e}

let update_phys (p : t) (phys : cmd) : t = {p with phys}


let attempts_to_string (p : t) : string =
  List.map p.attempts ~f:(string_of_map)
  |> List.fold ~init:"" ~f:(Printf.sprintf "%s\n%s")

let num_attempts (p : t) : int = List.length p.attempts
