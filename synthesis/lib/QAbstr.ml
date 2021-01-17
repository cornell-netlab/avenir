open Core
open Util

type gen = {query: Test.t; restriction: Test.t option}

type t = {seen: Test.t list; generals: gen list}

let disable = false

let make () = {seen= []; generals= []}

let gen = NameGen.make ()

let rec abstract_expr (m : string StringMap.t) (e1 : Expr.t) (e2 : Expr.t) :
    (string StringMap.t * Expr.t) option =
  let open Expr in
  let erecurse f m e11 e12 e21 e22 =
    match abstract_expr m e11 e21 with
    | None -> None
    | Some (m, e1) -> (
      match abstract_expr m e12 e22 with
      | None -> None
      | Some (m, e2) -> Some (m, f e1 e2) )
  in
  match (e1, e2) with
  | Value v1, Value v2 when not (Value.same_size v1 v2) -> None
  | Value v1, Value v2 -> (
      if Value.eq v1 v2 then Some (m, e1)
      else
        let v1_str = Value.to_string v1 in
        let v2_str = Value.to_string v2 in
        match (StringMap.find m v1_str, StringMap.find m v2_str) with
        | Some abstr1, Some abstr2 when String.(abstr1 = abstr2) ->
            Some (m, Var (abstr1, Value.size v1))
        | None, None ->
            let x = NameGen.get_fresh_name gen () in
            let m' =
              StringMap.set m ~key:v1_str ~data:x
              |> StringMap.set ~key:v2_str ~data:x
            in
            Some (m', Var (x, Value.size v1))
        | _, _ -> None )
  | Var s1, Var s2 | Hole s1, Hole s2 ->
      if Stdlib.(s1 = s2) then Some (m, e1) else None
  | Plus (e11, e12), Plus (e21, e22) -> erecurse plus m e11 e12 e21 e22
  | Times (e11, e12), Times (e21, e22) -> erecurse times m e11 e12 e21 e22
  | Minus (e11, e12), Minus (e21, e22) -> erecurse minus m e11 e12 e21 e22
  | Mask (e11, e12), Mask (e21, e22) -> erecurse mask m e11 e12 e21 e22
  | _, _ ->
      if false then
        Printf.printf "\n%s\n doesn't match \n%s\n%!" (to_sexp_string e1)
          (to_sexp_string e2) ;
      None

(** TODO:: Rewrite to use Let_Syntax *)
let rec abstract (m : string StringMap.t) (q1 : Test.t) (q2 : Test.t) :
    (string StringMap.t * Test.t) option =
  let trecurse f m t11 t12 t21 t22 =
    match abstract m t11 t21 with
    | None -> None
    | Some (m, t1) -> (
      match abstract m t12 t22 with
      | None -> None
      | Some (m, t2) -> Some (m, f t1 t2) )
  in
  let erecurse f m e11 e12 e21 e22 =
    match abstract_expr m e11 e21 with
    | None -> None
    | Some (m, e1) -> (
      match abstract_expr m e12 e22 with
      | None -> None
      | Some (m, e2) -> Some (m, f e1 e2) )
  in
  match (q1, q2) with
  | True, True -> Some (m, True)
  | False, False -> Some (m, False)
  | Neg t1, Neg t2 -> (
    match abstract m t1 t2 with
    | None -> None
    | Some (m', t') -> Some (m', Test.neg t') )
  | Eq (e11, e12), Eq (e21, e22) -> erecurse Test.eq m e11 e12 e21 e22
  | Le (e11, e12), Le (e21, e22) -> erecurse Test.leq m e11 e12 e21 e22
  | And (t11, t12), And (t21, t22) -> trecurse Test.and_ m t11 t12 t21 t22
  | Or (t11, t12), Or (t21, t22) -> trecurse Test.or_ m t11 t12 t21 t22
  | Impl (t11, t12), Impl (t21, t22) -> trecurse Test.impl m t11 t12 t21 t22
  | Iff (t11, t12), Iff (t21, t22) -> trecurse Test.iff m t11 t12 t21 t22
  | _, _ ->
      if false then
        Printf.printf "\n%s\n doesn't match \n%s\n%!"
          (Test.to_sexp_string q1) (Test.to_sexp_string q2) ;
      None

let rec abstracted_expr (e1 : Expr.t) (e2 : Expr.t)
    (valuation : Value.t StringMap.t) : Value.t StringMap.t option =
  let open Option in
  let recurse e11 e12 e21 e22 =
    abstracted_expr e11 e21 valuation >>= abstracted_expr e12 e22
  in
  match (e1, e2) with
  | Value v1, Value v2 when Value.eq v1 v2 -> Some valuation
  | Value _, Value _ ->
      (* if false then Printf.printf "Int values are different %s <> %s"
         (sexp_string_of_expr e1) (sexp_string_of_expr e2); *)
      None
  | Value v, Var (x, _) -> (
      let open StringMap in
      match find valuation x with
      | None -> Some (set valuation ~key:x ~data:v)
      | Some v' when Value.eq v v' -> Some valuation
      | Some _ -> None )
  | Var s1, Var s2 | Hole s1, Hole s2 ->
      if String.(fst s1 = fst s2) then Some valuation else None
  | Plus (e11, e12), Plus (e21, e22) -> recurse e11 e12 e21 e22
  | Times (e11, e12), Times (e21, e22) -> recurse e11 e12 e21 e22
  | Minus (e11, e12), Minus (e21, e22) -> recurse e11 e12 e21 e22
  | Mask (e11, e12), Mask (e21, e22) -> recurse e11 e12 e21 e22
  | _, _ ->
      (* if false then Printf.printf "\n%s\n doesn't match \n%s\n%!"
         (sexp_string_of_expr e1) (sexp_string_of_expr e2); *)
      None

let rec abstracted (q1 : Test.t) (q2 : Test.t)
    (valuation : Value.t StringMap.t) : Value.t StringMap.t option =
  (* if false then Printf.printf "ABSTRACTED size %d =?= size %d\n%!"
     (num_nodes_in_test q1) (num_nodes_in_test q2); *)
  let open Option in
  let trecurse t11 t12 t21 t22 =
    abstracted t11 t21 valuation >>= abstracted t12 t22
  in
  let erecurse e11 e12 e21 e22 =
    abstracted_expr e11 e21 valuation >>= abstracted_expr e12 e22
  in
  match (q1, q2) with
  | True, True | False, False -> Some valuation
  | Neg t1, Neg t2 -> abstracted t1 t2 valuation
  | Eq (e11, e12), Eq (e21, e22) -> erecurse e11 e12 e21 e22
  | Le (e11, e12), Le (e21, e22) -> erecurse e11 e12 e21 e22
  | And (t11, t12), And (t21, t22) -> trecurse t11 t12 t21 t22
  | Or (t11, t12), Or (t21, t22) -> trecurse t11 t12 t21 t22
  | Impl (t11, t12), Impl (t21, t22) -> trecurse t11 t12 t21 t22
  | Iff (t11, t12), Iff (t21, t22) -> trecurse t11 t12 t21 t22
  | _, _ ->
      if false then
        Printf.printf "\n\n%s\ndisagrees with\n%s\n\n"
          (Test.to_sexp_string q1) (Test.to_sexp_string q2) ;
      None

let string_of_map (m : string StringMap.t) =
  StringMap.fold m ~init:"" ~f:(fun ~key ~data acc ->
      Printf.sprintf "%s(%s -> %s) " acc key data)

let exists_matching_abstraction test generals =
  List.exists generals ~f:(fun {query; restriction} ->
      match (abstracted test query StringMap.empty, restriction) with
      | Some _, None -> true
      | Some m, Some r -> Test.equals (Manip.substV r m) True
      | _ -> false)

let cache_check (_ : Parameters.t) ({seen; generals} : t) test =
  if disable then ({seen= []; generals= []}, `Miss test)
  else
    let f phi =
      (* if params.debug then Printf.printf "\ncomparing to %s\n%!"
         (sexp_string_of_test phi); *)
      abstract StringMap.empty test phi
    in
    (* if params.debug then Printf.printf "Searching for %s\n%!"
       (sexp_string_of_test test); *)
    match List.find_map seen ~f with
    | None ->
        Log.debug @@ lazy "No match" ;
        ({seen; generals}, `Miss test)
    | Some (_, q) when Test.equals q test ->
        Log.debug @@ lazy "Queries were identical\n%!" ;
        ({seen; generals}, `Hit test)
    | Some (m, q) ->
        Log.debug @@ lazy "Found a match\n%!" ;
        if exists_matching_abstraction test generals then (
          Log.debug @@ lazy "Found an existing generalization" ;
          ({seen; generals}, `HitAbs) )
        else (
          Log.debug
          @@ lazy "No Existing generalization --- generalizing!\n%!" ;
          let qvars =
            StringMap.data m |> List.dedup_and_sort ~compare:String.compare
          in
          ({seen; generals}, `AddAbs (qvars, q)) )

let add_abs ~query ~restriction tst (c : t) =
  {seen= tst :: c.seen; generals= {query; restriction} :: c.generals}

let add_test test (c : t) = {c with seen= test :: c.seen}
