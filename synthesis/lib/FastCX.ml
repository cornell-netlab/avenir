open Core
open Ast
open Tables
open Manip
open Prover
open Parameters
open Semantics
open Packet

let rec one_some (table: string) (lst : ((test * cmd) list)) : Ast.cmd option =
  let processed = List.map lst ~f:(fun (b, c) -> (b, truncated table c)) in
  let elim = List.filter processed ~f:(fun (b,c) -> if c = None then false else true) in
  if (List.is_empty elim) then None else match elim with
    | (b, Some c)::[] -> Some (Seq (Assume b, c))
    | _ -> failwith "not well formed"

and truncated (table : string) (program : Ast.cmd) : Ast.cmd option =
  match program with
  | Skip
  | Assign _
  | Assert _
  | Assume _
  | While _ -> None
  | Seq (c1, c2) ->
    (match truncated table c2 with
     | None -> truncated table c1
     | Some c2' -> Some (Seq (c1, c2')))
  | Select (_, lst) -> one_some table lst
  | (Apply (t, _, _, _)) -> if t = table then Some Skip else None

let hits_pred data prog inst edits e : test =
  match e with
  | Edit.Add (t, (ms, _, _)) ->
    let (ks, _, _) = get_schema_of_table t prog |> Option.value_exn in
    let phi = Match.list_to_test ks ms in
    let prefix = truncated t prog |> Option.value_exn in
    let (pref_gcl,_) = Instance.(apply `NoHoles `Exact (update_list inst edits) prefix) in
    wp pref_gcl phi
  | Edit.Del (_, _) -> failwith "unimplemented"

let hits_list_pred data prog inst edits =
  List.fold edits ~init:False
    ~f:(fun acc e -> acc %+% hits_pred data prog inst edits e)

let unreachable params (problem : Problem.t) (test : Ast.test) =
  match check_valid params (!%test) with
  | (Some x, _) ->
     let in_pkt = Packet.make x ~fvs:(Some problem.fvs) in
     let eval p i = trace_eval_inst p i ~wide:Packet.empty (in_pkt, None) in
     let ((log_pkt,_), _,_,_) = Instance.update_list problem.log_inst problem.log_edits
                                |> eval problem.log in
     let ((phys_pkt,_), _, _, _) = Instance.update_list problem.phys_inst problem.phys_edits
                                   |> eval problem.phys in
     if params.debug
     then begin
         let u_log,_ = problem.log |> (Instance.update_list problem.log_inst problem.log_edits
                                       |> Instance.apply ~no_miss:params.do_slice `NoHoles `Exact)
         in
         let u_rea,_ = problem.phys |> Instance.apply `NoHoles `Exact (Instance.update_list problem.phys_inst problem.phys_edits) in
         if params.debug then
           Printf.printf "-------------------------------------------\n%s \n???====?=====????\n %s\n-------------------------------------\n%!"
             (string_of_cmd u_log) (string_of_cmd u_rea);

         Printf.printf "LOG :%s -> %s\n" (Packet.string__packet in_pkt) (Packet.string__packet log_pkt);
         Printf.printf "PHYS:%s -> %s\n" (Packet.string__packet in_pkt) (Packet.string__packet phys_pkt)
       end;
     if Packet.equal log_pkt phys_pkt
     then `NotFound
     else `NoAndCE (Packet.make x,log_pkt)
  | (None, _) -> `Yes

let get_cex params data (problem : Problem.t) =
  let e = problem.log_edits |> List.hd_exn in
  hits_pred data problem.log problem.log_inst problem.log_edits e
  |> unreachable params problem
