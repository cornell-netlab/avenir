open Core
open Ast
open Z3

let force_print = false
let print_debug = false

let valid_prover = ref None
let sat_prover = ref None
let shortener = Bishtbl.make ()

let get r = Option.value_exn !r ~message:"Prover not initialized!!!"

let make_provers loc =
  sat_prover := Some (Smtlib.make_solver loc);
  valid_prover := !sat_prover

let debug term =
  if force_print || print_debug then
    let res = Smtlib.term_to_sexp term |> Smtlib.sexp_to_string in
    Printf.eprintf "(assert %s)\n%!" res

let test_str test =
  if false then
    let res = Test.to_string test in Printf.printf "TEST: %s\n%!"res
  else ()

(* TODO @PYS I commented this out because the compiler was complaining -- is it meant to be used somewhere? *)
(* let expr_str test =
 *   if force_print || print_debug then
 *     let res = Ast.string_of_expr test in Printf.printf "EXPR: %s"res
 *   else () *)

let quantify expr etyp styp =
  match etyp, styp with
  | `Var, `Valid -> Smtlib.const expr
  | `Hole, `Valid -> failwith "holes not allowed in valid queries"
  | _, `Sat -> Smtlib.const expr

let rec expr_to_term_help expr styp : Smtlib.term =
  let open Expr in
  match expr with
  | Value v -> Value.to_smt v
  | Var (v, _) -> quantify v `Var styp
  | Hole (h, _) -> quantify h `Hole styp
  | Cast (i,e) ->
     let sz = size e in
     let t = expr_to_term_help e styp in
     if sz > i
     then Smtlib.extract (i-1) 0 t
     else if sz < i
     then Smtlib.concat (expr_to_term_help (value (0,i-sz))  styp) t
     else t
  | Slice {hi;lo;bits} ->
     let term = expr_to_term_help bits styp in
     Smtlib.extract (hi-1) lo term

  | Plus (e1, e2) ->
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     Smtlib.bvadd
       (expr_to_term_help e1 styp)
       (expr_to_term_help e2 styp)

  | SatPlus(e1,e2) ->
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     let t1 = expr_to_term_help e1 styp in
     let t2 = expr_to_term_help e2 styp in
     let sz = size e1 in
     let maxt = expr_to_term_help (Value(Value.big_make (Util.max_int sz, sz))) styp in
     let bad = Smtlib.(bvugt t2 (bvsub maxt t1)) in
     Smtlib.(ite bad maxt (bvadd t1 t2))


  | Times (e1, e2) ->
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     Smtlib.bvmul
       (expr_to_term_help e1 styp)
       (expr_to_term_help e2 styp)

  | Minus (e1, e2) ->
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     Smtlib.bvsub
       (expr_to_term_help e1 styp)
       (expr_to_term_help e2 styp)

  | SatMinus (e1,e2) ->
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     let t1 = expr_to_term_help e1 styp in
     let t2 = expr_to_term_help e2 styp in
     let sz = size e1 in
     let zero = expr_to_term_help (value (0, sz)) styp in
     let bad = Smtlib.(bvult t1 t2) in
     Smtlib.(ite bad zero (bvsub t1 t2))

  | Mask (e1,e2) ->
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     let sexp = Smtlib.bvand
                  (expr_to_term_help e1 styp)
                  (expr_to_term_help e2 styp) in
     (* expr_str expr;
      * debug sexp; *)
     sexp
  | Xor (e1,e2) ->
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     let sexp = Smtlib.bvneg @@
                  Smtlib.bvxnor
                    (expr_to_term_help e1 styp)
                    (expr_to_term_help e2 styp) in
     (* expr_str expr;
      * debug sexp; *)
     sexp
  | BOr (e1,e2) ->
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     let sexp = Smtlib.bvor
                  (expr_to_term_help e1 styp)
                  (expr_to_term_help e2 styp) in
     (* expr_str expr;
      * debug sexp; *)
     sexp
  | Shl (e1,e2) ->
     let sexp = Smtlib.bvshl
                  (expr_to_term_help e1 styp)
                  (expr_to_term_help e2 styp) in
     (* expr_str expr;
      * debug sexp; *)
     sexp
  | Concat (e1,e2) ->
       Smtlib.concat
         (expr_to_term_help e1 styp)
         (expr_to_term_help e2 styp)



let rec test_to_term_help test styp : Smtlib.term =
  let open Test in
  match test with
  | True -> Smtlib.bool_to_term true
  | False -> Smtlib.bool_to_term false
  | Eq (e1, e2) ->
     let open Expr in
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     Smtlib.equals
       (expr_to_term_help e1 styp)
       (expr_to_term_help e2 styp)
  | Le (e1, e2) ->
     let open Expr in
     if size e1 <> size e2
     then Printf.printf "%s and %s are differently sized\n%!" (to_string e1) (to_string e2);
     Smtlib.bvule
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


let toZ3String test = test_to_term_help test `Sat
                      |> Smtlib.term_to_sexp |> Smtlib.sexp_to_string

(* TODO @PYS I commented this out because the compiler was complaining -- is it meant to be used somewhere?*)
(* let expr_to_term e styp d =
 *   if force_print || d
 *   then (expr_str e; let res = expr_to_term_help e styp in (\*debug res;*\) res)
 *   else expr_to_term_help e styp *)

let test_to_term test styp d =
  if force_print || d
  then (test_str test; let res = test_to_term_help test styp in (*debug res;*) res)
  else test_to_term_help test styp

let vars_to_term vars d =
  let open Smtlib in
  if force_print || d && print_debug
  then begin
      let open List in
      iter vars ~f:(fun (id, i) -> Printf.printf "VAR: %s %d\n%!" id i);
      map vars ~f:(fun (id, i) -> (Id id, BitVecSort i))
    end
  else List.map vars ~f:(fun (id, i) -> (Id id, BitVecSort i))


let check_sat (params : Parameters.t) (longtest : Test.t) =
  let open Smtlib in
  (* Printf.printf "Finding model for test of size %d\n%!" (num_nodes_in_test test); *)
  (* Printf.printf "\n%s\n\n%!" (string_of_test test); *)
  if longtest = True then (Some Model.empty, Time.Span.zero) else
  if longtest = False then (None, Time.Span.zero) else
  let test = Shortener.shorten shortener longtest in
  if params.debug then assert (longtest = Shortener.unshorten shortener test);
  let vars = vars_to_term (Test.vars test) params.debug in
  let st = Time.now() in
  let holes = Test.holes test |> List.dedup_and_sort
                ~compare:(fun (idx, _) (idy, _) -> Stdlib.compare idx idy) in
  let () = List.iter holes
             ~f:(fun (id, i) ->
               if force_print || params.debug && print_debug then
                    Printf.eprintf "(declare-const %s (_ BitVec %d))\n%!" id i;
               declare_const (get sat_prover) (Id id) (BitVecSort i)) in
  let term = forall_ vars (test_to_term test `Sat params.debug) in
  let response =
    if force_print || params.debug && print_debug then debug term;
    assert_ (get sat_prover) term;
    if force_print || params.debug && print_debug then Printf.printf " Asserted % dnodes!\n%!" (Test.num_nodes test);
    check_sat(* _using (ParOr (UFBV, SMT)) *) (get sat_prover) in
  let dur = Time.(diff (now()) st) in
  if params.debug && print_debug then Printf.printf "Got a Result\n%!";
  let model =
    if response = Sat then
      (* let () = Printf.printf "Sat\n%!" in *)
      let model =
        get sat_prover
        |> get_model
        |> Model.of_smt_model
        |> Shortener.unshorten_model shortener in
      if params.debug && print_debug then
        Printf.printf "MODEL: %s\n%!" (Model.to_string model);
      Some model
      else if response = Unknown then
        failwith "UNKNOWN"
      else None
  in reset (get sat_prover);
     (model, dur)

let is_sat params test =
  check_sat params test |> fst |> Option.is_some

let check_valid (params : Parameters.t) (longtest : Test.t)  =
  let open Smtlib in
  (* Printf.printf "Checking validity for test of size %d\n%!" (num_nodes_in_test test); *)
  let test = Shortener.shorten shortener longtest in
  if params.debug then assert (longtest = Shortener.unshorten shortener test);
  (* printf.printf "Test:  %s\n %!" (string_of_test test ); *)
  let vars = Test.vars test
             |> List.dedup_and_sort
                  ~compare:(fun (idx, _) (idy, _) -> Stdlib.compare idx idy) in
  let () =
    List.iter vars
      ~f:(fun (id, i) ->
        if force_print || params.debug && print_debug then
          Printf.printf "(declare-const %s (_ BitVec %d))\n%!" id i;
        declare_const (get valid_prover) (Id id) (BitVecSort i)) in
  let st = Time.now() in
  let term = not_ (test_to_term test `Valid params.debug) in
  let response =
    if force_print || params.debug && print_debug then debug term;
    assert_ (get valid_prover) term;
    check_sat_using QFBV (get valid_prover) in
  let dur = Time.(diff (now()) st) in
  let model =
    match response with
    | Sat -> get valid_prover
             |> get_model
             |> Packet.of_smt_model
             |> Shortener.unshorten_packet shortener
             |> Some
    | Unsat ->  None
    | Unknown -> failwith "response unknown"
  in
  reset (get valid_prover);
  (model, dur)


let is_valid params test =
  check_valid params test |> fst |> Option.is_none

let cache = ref @@ QAbstr.make ()

let rec restriction_cegis ~gas (params : Parameters.t) (restriction : Test.t option) (query : Test.t) quantified_vars : Test.t option option =
  if gas <= 0 then
    None
  else
    let open Test in
    let restr_test = Option.value restriction ~default:True in
    if params.debug then Printf.printf "RESTRICTION: %s\n%!" (to_string restr_test);
    match check_valid params (restr_test %=>% query) with
    | None, _ -> Some (restriction)
    | Some m, _ ->
       let restr_test' =
         List.fold quantified_vars ~init:restr_test
           ~f:(fun acc var ->
             acc %&%
               match Packet.get_val_opt m var with
               | None ->
                  if params.debug then Printf.printf "Couldn't find %s in model\n%!" var;
                  True
               | Some v -> Var(var, Value.size v) %<>% Value v) in
       restriction_cegis ~gas:(gas - 1) params (Some restr_test') query quantified_vars


let check_valid_cached (params : Parameters.t) (test : Test.t) =
  (* let params = {params with debug = true} in *)
  let st = Time.now () in
  let (cache', res) = QAbstr.cache_check params !cache test in
  cache := cache';
  match res with
  | `HitAbs ->
     if params.debug then Printf.printf "\tCache_hit after %fms!\n%!"
       (Time.(diff (now()) st |> Span.to_ms));
     (None, Time.(diff (now ()) st))
  | `Hit _ -> (None, (Time.(diff (now ()) st)))
  | `Miss test ->
     if params.debug then begin
         Printf.printf "\tCouldn't abstract from %d previous tests : %d nodes!\n%!" (List.length !cache.seen) (Test.num_nodes test);
         List.iter !cache.seen ~f:(fun t -> Printf.printf "%d\n%!" (Test.num_nodes t))
         end;
     let dur' = Time.(diff (now()) st) in
     if params.debug then Printf.printf "Querying\n%!";
     let (m , dur) = check_valid params test in
     if params.debug then Printf.printf "Queried\n%!";
     if Option.is_none m then
       let () = if params.debug then Printf.printf "successfully!\n%!" in
       cache := QAbstr.add_test test !cache;
     else
       if params.debug then Printf.printf "Unsuccessfully\n%!";

     (m, Time.Span.(dur + dur'))
  | `AddAbs (qvars,query) ->
     if params.debug then Printf.printf "\tChecking abstraction from %d previous tests and %d abstractions!\n%!" (List.length !cache.seen) (List.length !cache.generals);
     (* if params.debug then Printf.printf "ABSTRACTION: %s\n" (string_of_test query); *)
     let m = restriction_cegis ~gas:2 params None query qvars  in
     let dur' = Time.(diff (now()) st) in
     match m with
     | None ->
        (*Couldn't find a restriction to make it valid *)
        if params.debug then Printf.printf "\tAbstraction Failed\n%!";
        Interactive.pause params.interactive;
        (* if params.debug then Printf.printf "\t%s\n%!" (string_of_test q); *)
        let (m , _) = check_valid params test in
        let dur' = Time.(diff (now()) st) in
        (m, dur')
     | Some restriction -> (*FOund a restriction to make it valid*)
        if params.debug then Printf.printf "\tAbstraction successful\n%!";
        Interactive.pause params.interactive;
        cache := QAbstr.add_abs ~query ~restriction test !cache;
        (None, dur')

let is_valid_cached params test =
  check_valid_cached params test |> fst |> Option.is_none
