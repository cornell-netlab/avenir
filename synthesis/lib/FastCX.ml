open Core
open Ast
open Manip
open Prover
open Parameters
open Semantics
open VCGen

let rec one_some (table: string) (lst : ((Test.t * cmd) list)) : Ast.cmd option =
  let processed = List.map lst ~f:(fun (b, c) -> (b, truncated table c)) in
  let elim = List.filter processed ~f:(fun (_,c) -> if c = None then false else true) in
  if (List.is_empty elim) then None else match elim with
    | (b, Some c)::[] -> Some (Seq (Assume b, c))
    | _ -> failwith "not well formed"

and truncated (table : string) (program : Ast.cmd) : Ast.cmd option =
  match program with
  | Skip
  | Assign _
  | Assume _ -> None
  | Seq (c1, c2) ->
    (match truncated table c2 with
     | None -> truncated table c1
     | Some c2' -> Some (Seq (c1, c2')))
  | Select (_, lst) -> one_some table lst
  | Apply t -> if t.name = table then Some Skip else None


let is_reachable encode_tag params problem fvs in_pkt tbl_name keys =
  let phys = Problem.phys problem in
  let phys_inst = Problem.phys_inst problem in
  let phys_edits = Problem.phys_edits problem in
  let trunc =
    truncated tbl_name phys
    |> Option.value_exn ~message:(Printf.sprintf "Couldn't find table %s" tbl_name)
    |> Instance.(apply params NoHoles `Exact (update_list params phys_inst phys_edits))
  in
  passive_hoare_triple ~fvs
    (Packet.to_test in_pkt ~fvs)
    trunc
    Test.(Hole.match_holes_table encode_tag tbl_name keys
          %&% Instance.negate_rows phys_inst tbl_name)


let hits_pred params (_: ProfData.t ref) prog inst edits e : Test.t =
  let open Test in
  match e with
  | Edit.Add (t, (ms, _, _)) ->
    let phi = Test.(Match.list_to_test ms %&% Instance.negate_rows inst t) in
    (* Printf.printf "Condition: %s\n%!" (string_of_test phi); *)
    let prefix = truncated t prog |> Option.value_exn in
    let pref_gcl = Instance.(apply params NoHoles `Exact (update_list params inst edits) prefix) in
    wp `Negs pref_gcl phi
  | Edit.Del (t, i) ->
     let (ms, _, _) = Instance.get_row inst t i |> Option.value_exn in
     let phi = Match.list_to_test ms %&%
                 List.fold (Instance.get_rows_before inst t i) ~init:True
                   ~f:(fun acc (matches,_,_) ->
                     (* Printf.printf "combining %s\n%!" (Match.list_to_test ks matches |> string_of_test); *)
                     acc %&% !%(Match.list_to_test matches))
     in
     (* Printf.printf "Condition: %s\n%!" (string_of_test phi); *)
     let prefix = truncated t prog |> Option.value_exn in
    let pref_gcl = Instance.(apply params NoHoles `Exact (update_list params inst edits) prefix) in
    wp `Negs pref_gcl phi


let hits_list_pred params (data : ProfData.t ref) prog inst edits =
  (* Printf.printf "\tThere are %d edits to check\n" (List.length edits); *)
  List.fold edits ~init:[]
    ~f:(fun acc e -> hits_pred params data prog inst edits e :: acc)


let make_cex params problem (x : Packet.t) =
     let open Problem in
     let in_pkt = Packet.remake x ~fvs:(fvs problem |> Some) in
     let log  = log_gcl_program params problem in
     let phys = phys_gcl_program params problem in
     let log_pkt = eval_act log in_pkt in
     let phys_pkt = eval_act phys in_pkt in
     if params.debug
     then begin
         if params.debug then
           Printf.printf "-------------------------------------------\n%s \n???====?=====????\n %s\n-------------------------------------\n%!"
             (string_of_cmd log) (string_of_cmd phys);

         Printf.printf "LOG :%s -> %s\n" (Packet.to_string in_pkt) (Packet.to_string log_pkt);
         Printf.printf "PHYS:%s -> %s\n" (Packet.to_string in_pkt) (Packet.to_string phys_pkt)
       end;
     if Packet.equal ~fvs:(fvs problem |> Some) log_pkt phys_pkt
     then
       (* let () = Printf.printf "PACKETS EQUAL\n%!" in *)
       `NotFound in_pkt
     else
       (* let () = Printf.printf "PACKETS NEQ\n%!" in *)
       `NoAndCE (in_pkt,log_pkt)

let attempt_model test =
  let open Test in
  match test with
  | Eq(Hole(x,_), Value(v)) -> Packet.(set_field empty x v) |> Some
  | _ -> None


let unreachable params (problem : Problem.t) (test : Test.t) =
  let open Test in
  let n = 10 in
  let rec loop i phi =
    let query = !%(test %&% phi) in
    if params.debug then
      Printf.printf "FAST CX QUERY %d : \n %s\n%!" (n - i) (Test.to_string query);
    match attempt_model query with
    | Some in_pkt -> makecexloop params problem i in_pkt phi
    | None ->
       match check_valid params query with
       | (Some in_pkt, _) -> makecexloop params problem i in_pkt phi
       | None, _ when n = i-> `Yes
       | None, _ -> `NotFound (Packet.empty)
  and makecexloop params problem i in_pkt phi =
    match make_cex params problem in_pkt with
    | (`NoAndCE _ as counter) | (`Yes as counter) ->
       (* Printf.printf "****************************************************\n%!"; *)
     counter
    | `NotFound full_pkt ->
       if i <= 0
       then (* failwith "" *) `NotFound in_pkt
       else Packet.to_test ~fvs:(Problem.fvs problem) full_pkt
            |> Test.neg
            |> Test.and_ phi
            |> loop (i - 1)
  in
  loop n True

let get_cex ?neg:(neg = Test.True) params data (problem : Problem.t) =
  let open Problem in
  if params.debug then Printf.printf "\t   a fast Cex\n%!";
  let e = log_edits problem |> List.hd_exn in
  hits_pred params data (log problem) (log_inst problem) (log_edits problem) e
  |> Test.and_ neg
  |> unreachable params problem
