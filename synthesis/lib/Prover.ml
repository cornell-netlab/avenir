open Core
open Z3

let valid_prover = ref None

let sat_prover = ref None

let shortener = Bishtbl.make ()

let get r = Option.value_exn !r ~message:"Prover not initialized!!!"

let make_provers loc =
  sat_prover := Some (Smtlib.make_solver loc) ;
  valid_prover := !sat_prover

let debug term =
  lazy
    ( Smtlib.term_to_sexp term |> Smtlib.sexp_to_string
    |> Printf.sprintf "(assert %s)\n%!" )
  |> Log.z3

(* TODO @PYS I commented this out because the compiler was complaining -- is
   it meant to be used somewhere? *)
(* let expr_str test =
 *   if force_print || print_debug then
 *     let res = Ast.string_of_expr test in Printf.printf "EXPR: %s"res
 *   else () *)

let quantify expr etyp styp =
  match (etyp, styp) with
  | `Var, `Valid -> Smtlib.const expr
  | `Hole, `Valid -> failwith "holes not allowed in valid queries"
  | _, `Sat -> Smtlib.const expr

let check_size e1 e2 =
  let open Expr in
  if size e1 <> size e2 then
    Log.warn
    @@ lazy
         (Printf.sprintf "%s and %s are differently sized\n%!" (to_string e1)
            (to_string e2))

let rec expr_to_term_help expr styp : Smtlib.term =
  let open Expr in
  match expr with
  | Value v -> Value.to_smt v
  | Var (v, _) -> quantify v `Var styp
  | Hole (h, _) -> quantify h `Hole styp
  | Cast (i, e) ->
      let sz = size e in
      let t = expr_to_term_help e styp in
      if sz > i then Smtlib.extract (i - 1) 0 t
      else if sz < i then
        Smtlib.concat (expr_to_term_help (value (0, i - sz)) styp) t
      else t
  | Slice {hi; lo; bits} ->
      let term = expr_to_term_help bits styp in
      Smtlib.extract (hi - 1) lo term
  | Plus (e1, e2) ->
      check_size e1 e2 ;
      Smtlib.bvadd (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | SatPlus (e1, e2) ->
      check_size e1 e2 ;
      let t1 = expr_to_term_help e1 styp in
      let t2 = expr_to_term_help e2 styp in
      let sz = size e1 in
      let maxt =
        expr_to_term_help (Value (Value.big_make (Util.max_int sz, sz))) styp
      in
      let bad = Smtlib.(bvugt t2 (bvsub maxt t1)) in
      Smtlib.(ite bad maxt (bvadd t1 t2))
  | Times (e1, e2) ->
      check_size e1 e2 ;
      Smtlib.bvmul (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | Minus (e1, e2) ->
      check_size e1 e2 ;
      Smtlib.bvsub (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | SatMinus (e1, e2) ->
      check_size e1 e2 ;
      let t1 = expr_to_term_help e1 styp in
      let t2 = expr_to_term_help e2 styp in
      let sz = size e1 in
      let zero = expr_to_term_help (value (0, sz)) styp in
      let bad = Smtlib.(bvult t1 t2) in
      Smtlib.(ite bad zero (bvsub t1 t2))
  | Mask (e1, e2) ->
      check_size e1 e2 ;
      Smtlib.bvand (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | Xor (e1, e2) ->
      check_size e1 e2 ;
      Smtlib.bvneg
      @@ Smtlib.bvxnor
           (expr_to_term_help e1 styp)
           (expr_to_term_help e2 styp)
  | BOr (e1, e2) ->
      check_size e1 e2 ;
      Smtlib.bvor (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | Shl (e1, e2) ->
      Smtlib.bvshl (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | Concat (e1, e2) ->
      Smtlib.concat (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)

let rec test_to_term test styp : Smtlib.term =
  let open Test in
  match test with
  | True -> Smtlib.bool_to_term true
  | False -> Smtlib.bool_to_term false
  | Eq (e1, e2) ->
      check_size e1 e2 ;
      Smtlib.equals (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | Le (e1, e2) ->
      check_size e1 e2 ;
      Smtlib.bvule (expr_to_term_help e1 styp) (expr_to_term_help e2 styp)
  | And (t1, t2) -> Smtlib.and_ (test_to_term t1 styp) (test_to_term t2 styp)
  | Or (t1, t2) -> Smtlib.or_ (test_to_term t1 styp) (test_to_term t2 styp)
  | Impl (t1, t2) ->
      Smtlib.implies (test_to_term t1 styp) (test_to_term t2 styp)
  | Iff (t1, t2) ->
      Smtlib.equals (test_to_term t2 styp) (test_to_term t1 styp)
  | Neg t -> Smtlib.not_ (test_to_term t styp)

let toZ3String test =
  test_to_term test `Sat |> Smtlib.term_to_sexp |> Smtlib.sexp_to_string

(* TODO @PYS I commented this out because the compiler was complaining -- is
   it meant to be used somewhere?*)
(* let expr_to_term e styp d =
 *   if force_print || d
 *   then (expr_str e; let res = expr_to_term_help e styp in (\*debug res;*\) res)
 *   else expr_to_term_help e styp *)

let vars_to_term vars =
  let open Smtlib in
  List.map vars ~f:(fun (id, i) -> (Id id, BitVecSort i))

let check_sat (_ : Parameters.t) (longtest : Test.t) =
  let open Smtlib in
  if Test.equals longtest True then (Some Model.empty, Time.Span.zero)
  else if Test.equals longtest False then (None, Time.Span.zero)
  else
    let test = Shortener.shorten shortener longtest in
    let vars = vars_to_term (Test.vars test) in
    let st = Time.now () in
    let holes =
      Test.holes test
      |> List.dedup_and_sort ~compare:(fun (idx, _) (idy, _) ->
             Stdlib.compare idx idy)
    in
    let () =
      List.iter holes ~f:(fun (id, i) ->
          lazy (Printf.sprintf "(declare-const %s (_ BitVec %d))\n%!" id i)
          |> Log.z3 ;
          declare_const (get sat_prover) (Id id) (BitVecSort i))
    in
    let term = forall_ vars (test_to_term test `Sat) in
    let response =
      debug term ;
      assert_ (get sat_prover) term ;
      lazy (Printf.sprintf " Asserted % dnodes!\n%!" (Test.num_nodes test))
      |> Log.z3 ;
      check_sat (* _using (ParOr (UFBV, SMT)) *) (get sat_prover)
    in
    let dur = Time.(diff (now ()) st) in
    Log.debug @@ lazy "Got a Result" ;
    let model =
      if Stdlib.(response = Sat) then (
        let model =
          get sat_prover |> get_model |> Model.of_smt_model
          |> Shortener.unshorten_model shortener
        in
        Log.debug
        @@ lazy (Printf.sprintf "MODEL: %s\n%!" (Model.to_string model)) ;
        Some model )
      else if Stdlib.(response = Unknown) then failwith "UNKNOWN"
      else None
    in
    reset (get sat_prover) ;
    (model, dur)

let is_sat params test = check_sat params test |> fst |> Option.is_some

let check_valid (_ : Parameters.t) (longtest : Test.t) =
  let open Smtlib in
  let test = Shortener.shorten shortener longtest in
  let vars =
    Test.vars test
    |> List.dedup_and_sort ~compare:(fun (idx, _) (idy, _) ->
           Stdlib.compare idx idy)
  in
  let () =
    List.iter vars ~f:(fun (id, i) ->
        Log.z3
        @@ lazy (Printf.sprintf "(declare-const %s (_ BitVec %d))\n%!" id i) ;
        declare_const (get valid_prover) (Id id) (BitVecSort i))
  in
  let st = Time.now () in
  let term = not_ (test_to_term test `Valid) in
  let response =
    debug term ;
    assert_ (get valid_prover) term ;
    check_sat_using QFBV (get valid_prover)
  in
  let dur = Time.(diff (now ()) st) in
  let model =
    match response with
    | Sat ->
        get valid_prover |> get_model |> Packet.of_smt_model
        |> Shortener.unshorten_packet shortener
        |> Some
    | Unsat -> None
    | Unknown -> failwith "response unknown"
  in
  reset (get valid_prover) ;
  (model, dur)

let is_valid params test = check_valid params test |> fst |> Option.is_none

let cache = ref @@ QAbstr.make ()

let rec restriction_cegis ~gas (params : Parameters.t)
    (restriction : Test.t option) (query : Test.t) quantified_vars :
    Test.t option option =
  if gas <= 0 then None
  else
    let open Test in
    let restr_test = Option.value restriction ~default:True in
    match check_valid params (restr_test %=>% query) with
    | None, _ -> Some restriction
    | Some m, _ ->
        let restr_test' =
          List.fold quantified_vars ~init:restr_test ~f:(fun acc var ->
              acc
              %&%
              match Packet.get_val_opt m var with
              | None -> True
              | Some v -> Var (var, Value.size v) %<>% Value v)
        in
        restriction_cegis ~gas:(gas - 1) params (Some restr_test') query
          quantified_vars

let check_valid_cached (params : Parameters.t) (test : Test.t) =
  (* let params = {params with debug = true} in *)
  let st = Time.now () in
  let cache', res = QAbstr.cache_check params !cache test in
  cache := cache' ;
  match res with
  | `HitAbs ->
      Log.info
      @@ lazy
           (Printf.sprintf "\tCache_hit after %fms!\n%!"
              Time.(diff (now ()) st |> Span.to_ms)) ;
      (None, Time.(diff (now ()) st))
  | `Hit _ -> (None, Time.(diff (now ()) st))
  | `Miss test ->
      Log.info
      @@ lazy
           (Printf.sprintf
              "\tCouldn't abstract from %d previous tests : %d nodes!\n%!"
              (List.length !cache.seen) (Test.num_nodes test)) ;
      let dur' = Time.(diff (now ()) st) in
      Log.debug @@ lazy "Querying" ;
      let m, dur = check_valid params test in
      Log.debug @@ lazy "Queried" ;
      if Option.is_none m then (
        Log.debug @@ lazy "successfully!" ;
        cache := QAbstr.add_test test !cache )
      else Log.debug @@ lazy "Unsuccessfully" ;
      (m, Time.Span.(dur + dur'))
  | `AddAbs (qvars, query) -> (
      Log.info
      @@ lazy
           (Printf.sprintf
              "Checking abstraction from %d previous tests and %d \
               abstractions!\n\
               %!"
              (List.length !cache.seen)
              (List.length !cache.generals)) ;
      (* if params.debug then Printf.printf "ABSTRACTION: %s\n"
         (string_of_test query); *)
      let m = restriction_cegis ~gas:2 params None query qvars in
      let dur' = Time.(diff (now ()) st) in
      match m with
      | None ->
          (*Couldn't find a restriction to make it valid *)
          Log.info @@ lazy "Abstraction Failed" ;
          Interactive.pause params.interactive ;
          (* if params.debug then Printf.printf "\t%s\n%!" (string_of_test
             q); *)
          let m, _ = check_valid params test in
          let dur' = Time.(diff (now ()) st) in
          (m, dur')
      | Some restriction ->
          (*FOund a restriction to make it valid*)
          Log.info @@ lazy "Abstraction successful" ;
          Interactive.pause params.interactive ;
          cache := QAbstr.add_abs ~query ~restriction test !cache ;
          (None, dur') )

let is_valid_cached params test =
  check_valid_cached params test |> fst |> Option.is_none
