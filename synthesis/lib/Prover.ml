open Core
open Ast
open Util
open Packet

let prover = Smtlib.make_solver "/usr/bin/z3"

let debug term = let res = Smtlib.term_to_sexp term |> Smtlib.sexp_to_string in Printf.printf "%s"res

let test_str test = let res = Ast.string_of_test test in Printf.printf "%s"res

let expr_str test = let res = Ast.string_of_expr test in Printf.printf "%s"res

let quantify expr etyp styp =
  match styp with
  | `Valid -> (match etyp with
      | "var" -> Smtlib.const expr
      | _ -> raise (Failure "not allowed here"))
  | `Sat -> (match etyp with
      | "hole" -> Smtlib.const expr
      | _ -> String expr)

let reset b = false

let rec expr_to_term_help expr styp : Smtlib.term =
  match expr with
  | Value (Int (num, sz)) -> Smtlib.bv (Bigint.to_int_exn num) sz
  | Var (v, sz) -> quantify v "var" styp
  | Hole (h, sz) -> quantify h "hole" styp
  | Plus (e1, e2) -> Smtlib.add (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | Times (e1, e2) -> Smtlib.mul (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | Minus (e1, e2) -> Smtlib.sub (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)

let rec test_to_term_help test styp : Smtlib.term =
  match test with
  | True -> Smtlib.bool_to_term true
  | False -> Smtlib.bool_to_term false
  | Eq (e1, e2) -> Smtlib.equals (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | Le (e1, e2) -> Smtlib.lte (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | And (t1, t2) -> Smtlib.and_ (test_to_term_help t1 styp) (test_to_term_help t2 styp)
  | Or (t1, t2) -> Smtlib.or_ (test_to_term_help t1 styp) (test_to_term_help t2 styp)
  | Impl (t1, t2) -> Smtlib.implies (test_to_term_help t1 styp) (test_to_term_help t2 styp)
  | Iff (t1, t2) -> Smtlib.equals (test_to_term_help t2 styp) (test_to_term_help t1 styp)
  | Neg t -> Smtlib.not_ (test_to_term_help t styp)

let rec model_to_packet (lst : (Smtlib.identifier * Smtlib.term) list) : Packet.t =
  let name_vals : (string * value) list = (List.map lst
                                             ~f:(fun (Id id, x) ->
                                                 (match x with
                                                  | Smtlib.Int i -> let value =
                                                               Int (Bigint.of_int i, Int.max_value) in id, value
                                                  | _ -> raise (Failure "not a supported model"))))
  in StringMap.of_alist_exn name_vals

let toZ3String test = test_to_term_help test `Sat |> Smtlib.term_to_sexp |> Smtlib.sexp_to_string

let expr_to_term e styp d = if d then (expr_str e; let res = expr_to_term_help e styp in debug res; res) else expr_to_term_help e styp

let test_to_term test styp d = if d then (test_str test; let res = test_to_term_help test styp in debug res; res) else test_to_term_help test styp

let declares = String.Hash_set.create ()

let check (params : Parameters.t) (test : Ast.test) =
  let open Smtlib in
  let vars : term list = List.map (free_vars_of_test test) ~f:(fun (id, i) -> Bind (Id id, BitVecSort i)) in
  let st = Time.now() in
  let holes = holes_of_test test in
  let () = List.iter holes ~f:(fun (id, i) -> if (Hash_set.mem declares id) then () else declare_const prover (Id id) (BitVecSort i); Hash_set.add declares id) in
  let response = assert_ prover (forall_ vars (test_to_term test `Sat params.debug));
    check_sat_using (UFBV : tactic) prover in
  let dur = Time.(diff (now()) st) in
  let model =
    if response = Sat then
      Some (model_to_packet (get_model prover))
    else None
  in (model, dur)

let check_valid (params : Parameters.t) (test : Ast.test) =
  let open Smtlib in
  let vars = free_vars_of_test test in
  let () = List.iter vars ~f:(fun (id, i) -> if (Hash_set.mem declares id) then () else declare_const prover (Id id) (BitVecSort i); Hash_set.add declares id) in
  let st = Time.now() in
  let response = assert_ prover (not_ (test_to_term test `Valid params.debug));
    check_sat_using (QFBV : tactic) prover in
  let dur = Time.(diff (now()) st) in
  let model =
    if response = Sat then
      Some (model_to_packet (get_model prover))
    else None
  in (model, dur)

let check_min (params : Parameters.t) (test : Ast.test) =
  let open Smtlib in
  let st = Time.now() in
  let vars : term list = List.map (free_vars_of_test test) ~f:(fun (id, i) -> Bind (Id id, BitVecSort i)) in
  let holes = holes_of_test test in
  let constraints =
    List.fold holes
      ~init:[]
      ~f:(fun acc (hi, sz) ->
          if String.is_suffix hi ~suffix:"_hi"
          then
            let hivar = String.rev hi |> String.substr_replace_first ~pattern:"ih_" ~with_:"" |> String.rev in
            List.fold holes
              ~init:acc
              ~f:(fun acc' (lo, sz) ->
                  if String.is_suffix lo ~suffix:"_lo"
                  then
                    let lovar = String.rev lo |> String.substr_replace_first ~pattern:"ol_" ~with_:"" |> String.rev in
                    if hivar = lovar
                    then acc @ [(Minus(Hole(hi,sz), Hole(lo,sz)))]
                    else acc'
                  else acc'
                )
          else acc
        ) in
  let ranges = List.map constraints ~f:(fun e -> expr_to_term e `Sat params.debug) in
  let () = List.iter holes ~f:(fun (id, i) -> if (Hash_set.mem declares id) then () else declare_const prover (Id id) (BitVecSort i); Hash_set.add declares id) in
  let response = assert_ prover (forall_ vars (test_to_term test `Sat params.debug));
    List.iter ranges ~f:(fun t -> minimize prover t);
    check_sat_using (UFBV : tactic) prover in
  let dur = Time.(diff (now()) st) in
  let model =
    if response = Sat then
      Some (model_to_packet (get_model prover))
    else None
  in (model, dur)
