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

let fastcx_gen data log e =
  match e with
  | Edit.Add (t, (ms, _, _)) ->
    let (ks, _, _) = get_schema_of_table t log |> Option.value_exn in
    let phi = Match.list_to_test ks ms in
    let prefix = truncated t log |> Option.value_exn in
    wp prefix phi
  | Edit.Del (_, _) -> failwith "unimplemented"

let unreachable params log linst phys pinst (test : Ast.test) =
  match check_valid params (!%test) with
  | (Some x, _) ->
     let ((log_pkt,_), _,_,_) = trace_eval_inst log linst ~wide:Packet.empty (x, None) in
     let ((phys_pkt,_), _, _, _) = trace_eval_inst phys pinst ~wide:(Packet.empty) (x, None) in
     if Packet.equal log_pkt phys_pkt
     then `Yes
     else `NoAndCE x
  | (None, _) -> `Yes

let get_cex params data log linst phys pinst e =
  fastcx_gen data log e
  |> unreachable params log linst phys pinst
