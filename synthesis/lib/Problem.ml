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
    attempts : cmd list;
    fvs : (string * int) list
  }


let to_string (p : t) =
  Printf.sprintf "+-------------------------+\nLogical:\n%s\n\nPhysical:\n%s\nWRT:[%s]\n+-------------------------------+\n"
    (Instance.apply `NoHoles `Exact (Instance.update_list p.log_inst p.log_edits) p.log
     |> fst |> string_of_cmd)
    (Instance.apply `NoHoles `Exact p.phys_inst p.phys
     |> fst |> string_of_cmd)
    (List.map p.fvs ~f:(fun (x,sz) -> "(" ^ x ^ "#" ^ string_of_int sz ^ ")")
     |> List.reduce ~f:(fun x y -> x ^","^ y)
     |> Option.value ~default:"")

let extract_log_edited_instance (p : t) : Instance.t =
  Instance.update_list p.log_inst p.log_edits

let extract_phys_edited_instance (p : t) : Instance.t =
  Instance.update_list p.phys_inst p.phys_edits

let extract_phys_gcl_program (p : t) : cmd =
  Instance.apply `NoHoles `Exact (extract_phys_edited_instance p) p.phys |> fst

let extract_log_gcl_program (p : t) : cmd =
  Instance.apply `NoHoles `Exact (extract_log_edited_instance p) p.log |> fst
