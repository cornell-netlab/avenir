open Core
open Ast
open Util

type t = {seen : test list;
          generals: (test * test) list;
         }

let disable = false

let make () = {seen = []; generals = []}

let gen = NameGen.make ()

let rec abstract_expr (m : string StringMap.t) (e1 : expr) (e2 : expr) : (string StringMap.t * expr) option =
  let erecurse f m e11 e12 e21 e22 =
    match abstract_expr m e11 e21 with
    | None -> None
    | Some (m, e1) ->
       match abstract_expr m e12 e22 with
       | None -> None
       | Some (m, e2) -> Some (m, f e1 e2)
  in
  match e1, e2 with
  | Value(Int(_,sz)), Value(Int(_,sz')) when sz <> sz' -> None
  | Value(v1), Value(v2) ->
     if veq v1 v2
     then Some (m, e1)
     else
       begin
       let v1_str = string_of_value v1 in
       let v2_str = string_of_value v2 in
       match StringMap.find m v1_str, StringMap.find m v2_str with
       | Some abstr1, Some abstr2 when abstr1 = abstr2 -> Some(m, Var(abstr1,size_of_value v1))
       | None, None ->
          let x = NameGen.get_fresh_name gen () in
          let m' = StringMap.set m ~key:v1_str ~data:x
                   |> StringMap.set ~key:v2_str ~data:x in
          Some (m', Var(x, size_of_value v1))
       | _, _ -> None
       end
  | Var s1, Var s2 | Hole s1, Hole s2 ->
     if Stdlib.(s1 = s2) then Some (m,e1) else None
  | Plus  (e11, e12), Plus  (e21, e22) -> erecurse mkPlus  m e11 e12 e21 e22
  | Times (e11, e12), Times (e21, e22) -> erecurse mkTimes m e11 e12 e21 e22
  | Minus (e11, e12), Minus (e21, e22) -> erecurse mkMinus m e11 e12 e21 e22
  | Mask  (e11, e12), Mask  (e21, e22) -> erecurse mkMask  m e11 e12 e21 e22
  | _, _ ->
     if false then Printf.printf "\n%s\n doesn't match \n%s\n%!" (sexp_string_of_expr e1) (sexp_string_of_expr e2);
     None



let rec abstract (m : string StringMap.t) (q1 : test) (q2 : test) : (string StringMap.t * test) option =
  (* if false then Printf.printf "QUERY SIZE: %d   CACHED TEST SIZE: %d\n" (num_nodes_in_test q1) (num_nodes_in_test q2); *)
  let trecurse f m t11 t12 t21 t22 =
    match abstract m t11 t21 with
    | None -> None
    | Some (m, t1) ->
       match abstract m t12 t22 with
       | None -> None
       | Some (m, t2) -> Some (m, f t1 t2)
  in
  let erecurse f m e11 e12 e21 e22 =
    match abstract_expr m e11 e21 with
    | None -> None
    | Some (m, e1) ->
       match abstract_expr m e12 e22 with
       | None -> None
       | Some (m, e2) -> Some (m, f e1 e2)
  in
  match q1, q2 with
  | True, True -> Some (m, True)
  | False, False -> Some (m, False)
  | Neg t1, Neg t2 -> begin match abstract m t1 t2 with
                      | None -> None
                      | Some (m', t') -> Some(m', !%(t'))
                      end
  | Eq  (e11,e12), Eq  (e21,e22) -> erecurse (fun e e' -> Eq(e,e'))   m e11 e12 e21 e22
  | Le  (e11,e12), Le  (e21,e22) -> erecurse (fun e e' -> Le(e,e'))   m e11 e12 e21 e22
  | And (t11,t12), And (t21,t22) -> trecurse (fun e e' -> And(e,e'))  m t11 t12 t21 t22
  | Or  (t11,t12), Or  (t21,t22) -> trecurse (fun e e' -> Or(e,e'))   m t11 t12 t21 t22
  | Impl(t11,t12), Impl(t21,t22) -> trecurse (fun e e' -> Impl(e,e')) m t11 t12 t21 t22
  | Iff (t11,t12), Iff (t21,t22) -> trecurse (fun e e' -> Iff(e,e'))  m t11 t12 t21 t22
  | _, _ ->
     if false then Printf.printf "\n%s\n doesn't match \n%s\n%!" (sexp_string_of_test q1) (sexp_string_of_test q2);
     None


let rec abstracted_expr (e1 : expr) (e2 : expr) (valuation : value StringMap.t) : value StringMap.t option =
  let open Option in
  let recurse e11 e12 e21 e22 =
    abstracted_expr e11 e21 valuation >>= abstracted_expr e12 e22
  in
  match e1, e2 with
  | Value(Int(v1,sz1)), Value(Int(v2,sz2)) when sz1 = sz2 && v2 = v1 -> Some valuation
  | Value _, Value _ ->
     (* if false then Printf.printf "Int values are different %s  <> %s" (sexp_string_of_expr e1) (sexp_string_of_expr e2); *)
     None
  | Value v, Var (x,_) ->
     let open StringMap in
     begin match find valuation x with
     | None -> Some (set valuation ~key:x ~data:v)
     | Some v' when veq v v' -> Some valuation
     | Some _ -> None
     end
  | Var s1, Var s2 | Hole s1, Hole s2 ->
     if String.(fst s1 = fst s2) then
       Some valuation
     else
       None

  | Plus  (e11, e12), Plus  (e21, e22) -> recurse e11 e12 e21 e22
  | Times (e11, e12), Times (e21, e22) -> recurse e11 e12 e21 e22
  | Minus (e11, e12), Minus (e21, e22) -> recurse e11 e12 e21 e22
  | Mask  (e11, e12), Mask  (e21, e22) -> recurse e11 e12 e21 e22
  | _, _ ->
     (* if false then Printf.printf "\n%s\n doesn't match \n%s\n%!" (sexp_string_of_expr e1) (sexp_string_of_expr e2); *)
     None


let rec abstracted (q1 : test) (q2 : test) (valuation : value StringMap.t) : value StringMap.t option =
  (* if false then Printf.printf "ABSTRACTED size %d  =?= size %d\n%!" (num_nodes_in_test q1) (num_nodes_in_test q2); *)
  let open Option in
  let trecurse t11 t12 t21 t22 = abstracted t11 t21 valuation >>= abstracted t12 t22 in
  let erecurse e11 e12 e21 e22 = abstracted_expr e11 e21 valuation >>= abstracted_expr e12 e22 in
  match q1, q2 with
  | True, True | False, False -> Some valuation
  | Neg t1, Neg t2 -> abstracted t1 t2 valuation
  | Eq  (e11,e12), Eq  (e21,e22) -> erecurse e11 e12 e21 e22
  | Le  (e11,e12), Le  (e21,e22) -> erecurse e11 e12 e21 e22
  | And (t11,t12), And (t21,t22) -> trecurse t11 t12 t21 t22
  | Or  (t11,t12), Or  (t21,t22) -> trecurse t11 t12 t21 t22
  | Impl(t11,t12), Impl(t21,t22) -> trecurse t11 t12 t21 t22
  | Iff (t11,t12), Iff (t21,t22) -> trecurse t11 t12 t21 t22
  | _, _ ->
     if false then Printf.printf "\n\n%s\ndisagrees with\n%s\n\n" (sexp_string_of_test q1) (sexp_string_of_test q2);
     None

let string_of_map (m : string StringMap.t) =
  StringMap.fold m ~init:""
    ~f:(fun ~key ~data acc ->
      Printf.sprintf
        "%s(%s -> %s) "
        acc
        key
        data
    )


let exists_matching_abstraction test generals =
  List.exists generals ~f:(fun (restr, phi) ->
      match abstracted test phi StringMap.empty with
      | Some m ->  Manip.substV restr m = True
      |  _ -> false)

let cache_check _ ({seen;generals} : t) test =
  if disable then ({seen=[];generals=[]}, `Miss test) else
  let f phi =
    (* if false then Printf.printf "\ncomparing to %s\n%!" (sexp_string_of_test phi); *)
    abstract StringMap.empty test phi
  in
  (* if false then Printf.printf "Searching for %s\n%!" (sexp_string_of_test test); *)
  match List.find_map seen ~f with
  | None ->
     if false then Printf.printf "No match\n%!";
     ({seen = seen; generals}, `Miss test)
  | Some (_,q) when q = test ->
     if false then Printf.printf "Queries were identical\n%!";
     ({seen; generals}, `Hit test)
  | Some (m,q) ->
     if false then Printf.printf "Found a match\n%!";
     if exists_matching_abstraction test generals then
       let () = if false then Printf.printf "Found an existing generalization\n%!" in
       ({seen; generals}, `HitAbs)
     else
       (* if false then Printf.printf "%s\n%!" (string_of_map m); *)
       let () = if false then Printf.printf "No Existing generalization --- generalizing!\n%!" in
       ({seen; generals}, `AddAbs (StringMap.keys m, q))


let add_abs g r tst (c : t) = {seen = tst::c.seen;
                               generals = (r,g)::c.generals}

let add_test test (c : t) = {c with seen = test::c.seen}
