open Core
open Ast
open Util
open Packet


let quantify expr etyp styp =
  match styp with
  | `Valid -> (match etyp with
      | "var" -> Smtlib.const expr
      | _ -> raise (Failure "not allowed here"))
  | `Sat -> (match etyp with
      | "hole" -> Smtlib.const expr
      | _ -> String expr)

let rec expr_to_term expr styp : Smtlib.term =
  match expr with
  | Value (Int (num, _)) -> Smtlib.int_to_term (Bigint.to_int_exn num)
  | Var (v, _) -> quantify v "var" styp
  | Hole (h, _) -> quantify h "hole" styp
  | Plus (e1, e2) -> Smtlib.add (expr_to_term e1 styp) (expr_to_term e2 styp)
  | Times (e1, e2) -> Smtlib.mul (expr_to_term e1 styp) (expr_to_term e2 styp)
  | Minus (e1, e2) -> Smtlib.sub (expr_to_term e1 styp) (expr_to_term e2 styp)

let rec test_to_term test styp : Smtlib.term =
  match test with
  | True -> Smtlib.bool_to_term true
  | False -> Smtlib.bool_to_term false
  | Eq (e1, e2) -> Smtlib.equals (expr_to_term e1 styp) (expr_to_term e2 styp)
  | Le (e1, e2) -> Smtlib.lte (expr_to_term e1 styp) (expr_to_term e2 styp)
  | And (t1, t2) -> Smtlib.and_ (test_to_term t1 styp) (test_to_term t2 styp)
  | Or (t1, t2) -> Smtlib.or_ (test_to_term t1 styp) (test_to_term t2 styp)
  | Impl (t1, t2) -> Smtlib.implies (test_to_term t1 styp) (test_to_term t2 styp)
  | Iff (t1, t2) -> Smtlib.equals (test_to_term t2 styp) (test_to_term t1 styp)
  | Neg t -> Smtlib.not_ (test_to_term t styp)

let rec model_to_packet (lst : (Smtlib.identifier * Smtlib.term) list) : Packet.t =
  let name_vals : (string * value) list = (List.map
                                             ~f:(fun (Id id, x) ->
                                                 (match x with
                                                  | Int i -> let value =
                                                               Int (Bigint.of_int i, Int.max_value) in id, value
                                                  | _ -> raise (Failure "not a supported model")))                                             lst)
  in StringMap.of_alist_exn name_vals

let toZ3String styp test = test_to_term styp test |> Smtlib.term_to_sexp |> Smtlib.sexp_to_string

let prover = Smtlib.make_solver "/usr/bin/z3"

let check (params : Parameters.t) typ (test : Ast.test) =
  let open Smtlib in
  let vars : term list = List.map (free_vars_of_test test) ~f:(fun (id, i) -> Bind (Id id, BitVecSort i)) in
  let st = Time.now() in
  let response = match typ with
    |`Sat -> assert_ prover (forall_ vars (test_to_term test `Sat));
      check_sat_using (UFBV : tactic) prover
    |`Valid -> assert_ prover (not_ (test_to_term test `Valid));
      check_sat_using (QFBV : tactic) prover in
  let dur = Time.(diff (now()) st) in
  let model =
    if response = Sat then
      Some (model_to_packet (get_model prover))
    else None
  in (model, dur)
