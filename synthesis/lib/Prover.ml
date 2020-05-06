open Core
open Ast
open Util
open Packet
open Z3

let print_debug = true

let debug term =
  if print_debug then
    let res = Smtlib.term_to_sexp term |> Smtlib.sexp_to_string in
    Printf.printf "TERM: %s\n%!" res
  else ()

let test_str test =
  if print_debug then
    let res = Ast.string_of_test test in Printf.printf "TEST: %s\n%!"res
  else ()

let expr_str test =
  if print_debug then
    let res = Ast.string_of_expr test in Printf.printf "EXPR: %s"res
  else ()

let quantify expr etyp styp =
  match etyp, styp with
  | `Var, `Valid -> Smtlib.const expr
  | `Hole, `Valid -> failwith "holes not allowed in valid queries"
  | _, `Sat -> Smtlib.const expr

let rec expr_to_term_help expr styp : Smtlib.term =
  match expr with
  | Value (Int (num, sz)) -> Smtlib.bbv (num) sz
  | Var (v, sz) -> quantify v `Var styp
  | Hole (h, sz) -> quantify h `Hole styp
  | Plus (e1, e2) -> Smtlib.bvadd
                       (expr_to_term_help e1 styp)
                       (expr_to_term_help e2 styp)
  | Times (e1, e2) -> Smtlib.bvmul
                        (expr_to_term_help e1 styp)
                        (expr_to_term_help e2 styp)
  | Minus (e1, e2) -> Smtlib.bvsub
                        (expr_to_term_help e1 styp)
                        (expr_to_term_help e2 styp)
  | Mask (e1,e2) -> Smtlib.bvand
                        (expr_to_term_help e1 styp)
                        (expr_to_term_help e2 styp)

let rec test_to_term_help test styp : Smtlib.term =
  match test with
  | True -> Smtlib.bool_to_term true
  | False -> Smtlib.bool_to_term false
  | Eq (e1, e2) -> Smtlib.equals
                     (expr_to_term_help e1 styp)
                     (expr_to_term_help e2 styp)
  | Le (e1, e2) -> Smtlib.bvule
                     (expr_to_term_help e1 styp)
                     (expr_to_term_help e2 styp)
  | And (t1, t2) -> Smtlib.and_
                      (test_to_term_help t1 styp)
                      (test_to_term_help t2 styp)
  | Or (t1, t2) -> Smtlib.or_
                     (test_to_term_help t1 styp)
                     (test_to_term_help t2 styp)
  | Impl (t1, t2) -> Smtlib.implies
                       (test_to_term_help t1 styp)
                       (test_to_term_help t2 styp)
  | Iff (t1, t2) -> Smtlib.equals
                      (test_to_term_help t2 styp)
                      (test_to_term_help t1 styp)
  | Neg t -> Smtlib.not_ (test_to_term_help t styp)

let rec model_to_packet (lst : (Smtlib.identifier * Smtlib.term) list) =
  let name_vals : (string * value) list =
    (List.map lst ~f:(fun (Id id, x) ->
         let id = match String.index id '@' with
           | None -> id
           | Some index -> String.drop_prefix id index in
         match x with
         | Smtlib.BitVec (n, w) -> let value =
                                     Int (Bigint.of_int n, w) in id, value
         | Smtlib.BigBitVec (n, w) -> let value =
                                        Int (n, w) in id, value
         | Smtlib.Int i -> let value =
                             Int (Bigint.of_int i, Int.max_value) in id, value
         | _ -> raise (Failure "not a supported model")))
  in StringMap.of_alist_exn name_vals

let toZ3String test = test_to_term_help test `Sat
                      |> Smtlib.term_to_sexp |> Smtlib.sexp_to_string

let expr_to_term e styp d = if d
  then (expr_str e; let res = expr_to_term_help e styp in debug res; res)
  else expr_to_term_help e styp

let test_to_term test styp d = if d
  then (test_str test; let res = test_to_term_help test styp in debug res; res)
  else test_to_term_help test styp

let vars_to_term vars d =
  let open Smtlib in
  (* if d
   * then (List.iter vars ~f:(fun (id, i) -> Printf.printf "VAR: %s %d\n%!" id i);
   *       List.map vars ~f:(fun (id, i) -> (Id id, BitVecSort i)))
   * else *) List.map vars ~f:(fun (id, i) -> (Id id, BitVecSort i))

let sat_prover = Smtlib.make_solver "/usr/bin/z3"
let valid_prover = Smtlib.make_solver (*"/usr/local/bin/boolector"*) "/usr/bin/z3"

let check_sat (params : Parameters.t) (test : Ast.test) =
  let open Smtlib in
  (* Printf.printf "Finding model for test of size %d\n%!" (num_nodes_in_test test); *)
  (* Printf.printf "\n%s\n\n%!" (string_of_test test); *)
  let vars = vars_to_term (free_vars_of_test test) params.debug in
  let st = Time.now() in
  let holes = holes_of_test test |> List.dedup_and_sort
                ~compare:(fun (idx, x) (idy, y) -> Stdlib.compare idx idy) in
  let () = List.iter holes
             ~f:(fun (id, i) ->
               (* Printf.printf "DECLARING %s\n%!" id; *)
               declare_const sat_prover (Id id) (BitVecSort i)) in
  let term = (test_to_term test `Sat params.debug) in
  let response = assert_ sat_prover (forall_ vars term);
    check_sat_using (UFBV : tactic) sat_prover in
  let dur = Time.(diff (now()) st) in
  let model =
    if response = Sat then
      let model = model_to_packet (get_model sat_prover) in
      if params.debug && print_debug then
        Printf.printf "MODEL: %s\n%!" (Packet.string__packet model);
      Some model
    else None
  in reset sat_prover; (model, dur)

let check_valid (params : Parameters.t) (test : Ast.test) =
  let open Smtlib in
  (* Printf.printf "Checking validity for test of size %d\n%!" (num_nodes_in_test test); *)
  let vars = free_vars_of_test test
             |> List.dedup_and_sort
                  ~compare:(fun (idx, x) (idy, y) -> Stdlib.compare idx idy) in
  let () =
    push valid_prover;
    List.iter vars
      ~f:(fun (id, i) -> declare_const valid_prover (Id id) (BitVecSort i)) in
  let st = Time.now() in
  let response =
    assert_ valid_prover (not_ (test_to_term test `Valid params.debug));
    check_sat valid_prover in
  let dur = Time.(diff (now()) st) in
  let model =
    if response = Sat
    then Some (model_to_packet (get_model valid_prover))
    else None in
  pop valid_prover;
  (model, dur)

let check_min (params : Parameters.t) (test : Ast.test) =
  let open Smtlib in
  let st = Time.now() in
  let vars = List.map (free_vars_of_test test)
      ~f:(fun (id, i) -> (Id id, BitVecSort i)) in
  let holes = holes_of_test test |> List.dedup_and_sort
                ~compare:(fun (idx, x) (idy, y) -> Stdlib.compare idx idy) in
  let constraints =
    List.fold holes
      ~init:[]
      ~f:(fun acc (hi, sz) ->
          if String.is_suffix hi ~suffix:"_hi"
          then
            let hivar = String.rev hi
                        |> String.substr_replace_first
                          ~pattern:"ih_" ~with_:""
                        |> String.rev in
            List.fold holes
              ~init:acc
              ~f:(fun acc' (lo, sz) ->
                  if String.is_suffix lo ~suffix:"_lo"
                  then
                    let lovar = String.rev lo
                                |> String.substr_replace_first
                                  ~pattern:"ol_" ~with_:""
                                |> String.rev in
                    if hivar = lovar
                    then acc @ [(Minus(Hole(hi,sz), Hole(lo,sz)))]
                    else acc'
                  else acc'
                )
          else acc
        ) in
  let ranges = List.map constraints
      ~f:(fun e -> expr_to_term e `Sat params.debug) in
  let () = List.iter holes
      ~f:(fun (id, i) -> declare_const sat_prover (Id id) (BitVecSort i)) in
  let term = (test_to_term test `Sat params.debug) in
  let response = assert_ sat_prover (forall_ vars term);
    List.iter ranges ~f:(fun t -> minimize sat_prover t);
    check_sat_using (UFBV : tactic) sat_prover in
  let dur = Time.(diff (now()) st) in
  let model =
    if response = Sat then
      Some (model_to_packet (get_model sat_prover))
    else None
  in reset sat_prover; (model, dur)
