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
  | Apply t -> if t.name = table then Some Skip else None

let hits_pred params data prog inst edits e : test =
  match e with
  | Edit.Add (t, (ms, _, _)) ->
    let (ks, _, _) = get_schema_of_table t prog |> Option.value_exn in
    let phi = Match.list_to_test ks ms in
    let prefix = truncated t prog |> Option.value_exn in
    let (pref_gcl,_) = Instance.(apply params NoHoles `Exact (update_list params inst edits) prefix) in
    wp pref_gcl phi
  | Edit.Del (_, _) -> failwith "unimplemented"

let hits_list_pred params data prog inst edits =
  List.fold edits ~init:False
    ~f:(fun acc e -> acc %+% hits_pred params data prog inst edits e)


let make_cex params problem x =
     let open Problem in
     let in_pkt = Packet.make x ~fvs:(fvs problem |> Some) in
     let log  = log_gcl_program params problem in
     let phys = phys_gcl_program params problem in
     let log_pkt = eval_act log in_pkt in
     let phys_pkt = eval_act phys in_pkt in
     if params.debug
     then begin
         if params.debug then
           Printf.printf "-------------------------------------------\n%s \n???====?=====????\n %s\n-------------------------------------\n%!"
             (string_of_cmd log) (string_of_cmd phys);

         Printf.printf "LOG :%s -> %s\n" (Packet.string__packet in_pkt) (Packet.string__packet log_pkt);
         Printf.printf "PHYS:%s -> %s\n" (Packet.string__packet in_pkt) (Packet.string__packet phys_pkt)
       end;
     if Packet.equal ~fvs:(fvs problem |> Some) log_pkt phys_pkt
     then `NotFound in_pkt
     else`NoAndCE (in_pkt,log_pkt)


let unreachable params (problem : Problem.t) (test : Ast.test) =
  let open Option in
  (* let drop_spec = (Problem.phys_drop_spec problem >>| fun spec ->
   *                  (\* Printf.printf "Program %s\n " (string_of_cmd @@ Problem.phys_gcl_program params problem);
   *                   * Printf.printf "OUT DROPSPEC:%s\n%!" (string_of_test spec); *\)
   *                  wp (Problem.phys_gcl_program params problem) spec)
   *                 |> value_exn in *)
  (* Printf.printf "IN DROP_SPEC%s\n%!" (string_of_test drop_spec); *)
  let mk_query = holify_test (List.map ~f:fst @@ free_vars_of_test test) in
  let query = mk_query test in
  if params.debug then Printf.printf "FAST CX QUERY 1 : \n %s\n%!"(string_of_test query);
  match check_sat params @@ mk_query test with
  | (Some in_pkt, _) ->
     begin match make_cex params problem in_pkt with
     | (`NoAndCE _ as counter) | (`Yes as counter) -> counter
     | `NotFound in_pkt ->
        let query =  mk_query @@ test %&% !%(Packet.to_test ~fvs:(Problem.fvs problem) in_pkt) in
        if params.debug then Printf.printf "FAST CX QUERY 2 : \n %s \n%!" (string_of_test query);
        match check_sat params @@ mk_query @@ query with
        | Some in_pkt, _ -> make_cex params problem in_pkt
        | None, _ -> `NotFound in_pkt
     end
  | None, _ -> `Yes
  (* | (None, _) ->
   *    Printf.printf "FAST CX QUERY 2: \n %s\n%!"(string_of_test test);
   *    let query = holify_test (List.map ~f:fst @@ free_vars_of_test test) test in
   *    match check_sat params query with
   *    | Some x, _ -> make_cex params problem x
   *    | None, _ -> `Yes *)


let get_cex params data (problem : Problem.t) =
  let open Problem in
  let e = log_edits problem |> List.hd_exn in
  hits_pred params data (log problem) (log_inst problem) (log_edits problem) e
  |> unreachable params problem
