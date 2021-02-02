open Core
open Util

let enable_smart_constructors = true

type styp =
  | Partial
  (* deprecated *)
  | Total
  (* deprecated *)
  | Ordered

let styp_equals typ1 typ2 =
  match (typ1, typ2) with
  | Partial, Partial | Total, Total | Ordered, Ordered -> true
  | _, _ -> false

let string_of_styp styp =
  match styp with
  | Partial -> "partial"
  | Total -> "total"
  | Ordered -> "ordered"

let sexp_string_of_styp styp = string_of_styp styp |> String.capitalize

module Key = struct
  type t = string * int * Value.t option

  let equals (k1, sz1, vopt1) (k2, sz2, vopt2) =
    String.(k1 = k2) && sz1 = sz2 && opt_equals ~f:Value.equals vopt1 vopt2

  let make (k, sz) = (k, sz, None)

  let set_val_opt (k, sz1, _) vopt = (k, sz1, vopt)

  let set_val k v = set_val_opt k (Some v)

  let to_string (x, sz, vopt) =
    Printf.sprintf "%s#%d%s" x sz
    @@
    match vopt with
    | None -> ""
    | Some v -> Printf.sprintf " = %s" (Value.to_string v)

  let var (x, _, _) = x

  let value (_, _, vopt) = vopt

  let has_value k = value k |> Option.is_some

  let to_sized (x, sz, _) = (x, sz)

  let str_map ~f (x, sz, v) = (f x, sz, v)

  let var_name (x, _, _) = x
end

type t =
  | Skip
  | Assign of (string * Expr.t)
  | Assume of Test.t
  | Seq of (t * t)
  | Select of (styp * (Test.t * t) list)
  | Apply of
      { name: string
      ; keys: Key.t list
      ; actions: (string * (string * int) list * t) list
      ; default: t }

let is_skip = function
  | Skip -> true
  | _ -> false

let var_equals (x1, sz1) (x2, sz2) = String.(x1 = x2) && sz1 = sz2

let rec equals c1 c2 =
  match (c1, c2) with
  | Skip, Skip -> true
  | Assign (f1, e1), Assign (f2, e2) -> String.(f1 = f2) && Expr.equals e1 e2
  | Assume t1, Assume t2 -> Test.equals t1 t2
  | Seq (c1, c1'), Seq (c2, c2') -> equals c1 c2 && equals c1' c2'
  | Select (typ1, ss1), Select (typ2, ss2) ->
      styp_equals typ1 typ2
      && List.equal
           (fun (t1, c1) (t2, c2) -> Test.equals t1 t2 && equals c1 c2)
           ss1 ss2
  | Apply t1, Apply t2 ->
      String.(t1.name = t2.name)
      && List.equal Key.equals t1.keys t2.keys
      && List.equal action_equals t1.actions t2.actions
      && equals t1.default t2.default
  | _, _ -> false

and action_equals (name1, args1, a1) (name2, args2, a2) =
  String.(name1 = name2) && List.equal var_equals args1 args2 && equals a1 a2

let rec seq first scnd =
  if not enable_smart_constructors then Seq (first, scnd)
  else
    match (first, scnd) with
    | Skip, x | x, Skip | Assume True, x | x, Assume False -> x
    | first, Seq (scnd1, scnd2) -> seq (Seq (first, scnd1)) scnd2
    | _, _ -> Seq (first, scnd)

let ( %:% ) = seq

let assign f v =
  match v with
  | Expr.Var (x, _) when String.(x = f) -> Skip
  | _ -> Assign (f, v)

let ( %<-% ) = assign

let assume t = if Test.(equals t True) then Skip else Assume t

let partial ss =
  if not enable_smart_constructors then Select (Partial, ss)
  else if List.length ss = 0 then Skip
  else Select (Partial, ss)

let total ss =
  if not enable_smart_constructors then Select (Total, ss)
  else if List.length ss = 0 then assume False
  else Select (Total, ss)

let rec assumes_false c =
  let f (_, c) = assumes_false c in
  match c with
  | Skip | Assign _ -> false
  | Seq (c1, c2) -> assumes_false c1 || assumes_false c2
  | Assume t -> Test.equals t False
  | Select (Ordered, cases) -> List.for_all cases ~f
  | Apply {actions; default; _} ->
      assumes_false default
      && List.for_all actions ~f:(fun (_, x, c) -> f (x, c))
  | Select _ -> failwith "deprecated"

let ordered ss =
  if not enable_smart_constructors then Select (Ordered, ss)
  else
    (*make these all a single pass *)
    let selects =
      List.remove_consecutive_duplicates ss ~which_to_keep:`First
        ~equal:(fun (cond, _) (cond', _) -> Test.equals cond cond')
      |> List.filter ~f:(fun (b, c) ->
             (not (Test.equals b False)) && not (assumes_false c))
      |> List.fold ~init:([], true) ~f:(fun (cases, reachable) (b, c) ->
             if reachable then (cases @ [(b, c)], not (Test.equals b True))
             else (cases, reachable))
      |> fst
    in
    if List.for_all selects ~f:(fun (_, c) -> equals c Skip) then Skip
    else
      match selects with
      | [] -> Skip
      | [(b, c)] -> assume b %:% c
      | _ -> Select (Ordered, selects)

let select styp =
  match styp with
  | Partial -> partial
  | Total -> total
  | Ordered -> ordered

let apply (name, keys, actions, default) =
  Apply {name; keys= List.map keys ~f:Key.make; actions; default}

(* let combineSelects e e' =
 *   match e, e' with
 *   | Select (xs_typ, xs), Select (ys_typ, ys) ->
 *     if styp_equals xs_typ ys_typ then
 *       select xs_typ (xs @ ys)
 *     else
 *       failwith ("[ERROR] Cannot combine selects with different types: "
 *                 ^ string_of_styp xs_typ
 *                 ^ " and "
 *                 ^ string_of_styp ys_typ)
 *   | _ -> failwith "Can only combine selects statements " *)

(* let (%%) = combineSelects *)

let rec repeat c n = if n = 0 then "" else c ^ repeat c (n - 1)

let rec to_string ?(depth = 0) (e : t) : string =
  match e with
  | Skip -> repeat "\t" depth ^ "skip"
  | Seq (firstdo, thendo) ->
      to_string ~depth firstdo ^ ";\n " ^ to_string ~depth thendo
  | Assume t -> repeat "\t" depth ^ "assume (" ^ Test.to_string t ^ ")"
  | Assign (field, expr) ->
      repeat "\t" depth ^ field ^ " := " ^ Expr.to_string expr
  | Select (styp, es) ->
      let modifier = string_of_styp styp in
      repeat "\t" depth ^ "if " ^ modifier
      ^ List.fold es ~init:"" ~f:(fun str (cond, act) ->
            str ^ "\n"
            ^ repeat "\t" (depth + 1)
            ^ Test.to_string cond ^ " ->\n "
            ^ to_string ~depth:(depth + 2) act
            ^ " []")
      ^ "\n" ^ repeat "\t" depth ^ "fi"
  | Apply t ->
      repeat "\t" depth ^ "apply (" ^ t.name ^ ",("
      ^ List.fold t.keys ~init:"" ~f:(fun str k ->
            str ^ Key.to_string k ^ ",")
      ^ ")" ^ ",("
      ^ List.foldi t.actions ~init:"" ~f:(fun i str a ->
            str
            ^ (if i > 0 then " |" else "")
            ^ " { "
            ^ ( if List.length (List.nth_exn t.actions i |> snd3) > 0 then
                "\\ ("
                ^ List.fold
                    (List.nth_exn t.actions i |> snd3)
                    ~init:""
                    ~f:(fun acc (x, sz) ->
                      Printf.sprintf "%s%s#%d," acc x sz)
                ^ ") -> "
              else "" )
            ^ to_string (trd3 a)
            ^ "}")
      ^ "), {" ^ to_string t.default ^ "})"

let rec to_sexp_string e : string =
  let string_select =
    concatMap
      ~f:(fun (cond, act) ->
        "(" ^ Test.to_sexp_string cond ^ "," ^ to_sexp_string act ^ ")")
      ~c:(fun acc d -> acc ^ ";" ^ d)
  in
  match e with
  | Skip -> "Skip"
  | Seq (p, q) -> "Seq(" ^ to_sexp_string p ^ "," ^ to_sexp_string q ^ ")"
  | Assume t -> "Assume(" ^ Test.to_sexp_string t ^ ")"
  | Assign (f, e) -> "Assign(\"" ^ f ^ "\"," ^ Expr.to_sexp_string e ^ ")"
  | Select (styp, es) ->
      let cases_string =
        match es with
        | [] -> "[]"
        | _ -> "[" ^ string_select es ^ "]"
      in
      "Select (" ^ sexp_string_of_styp styp ^ "," ^ cases_string ^ ")"
  | Apply t ->
      "Apply(" ^ t.name ^ ",["
      ^ List.fold_left t.keys ~init:"" ~f:(fun str k ->
            Printf.sprintf "%s;\"%s\"" str (Key.to_string k))
      ^ "],["
      ^ List.fold_left t.actions ~init:"" ~f:(fun str a ->
            str ^ ";((SOME ACTION DATA), " ^ to_sexp_string (trd3 a) ^ ")")
      ^ "]," ^ to_sexp_string t.default ^ ")"

(* let get_test_from_assume = function
 *   | Assume t -> t
 *   | c -> failwith @@ Printf.sprintf "tried to get test from command %s" (to_string c) *)

let rec tables (c : t) : string list =
  match c with
  | Skip | Assign _ | Assume _ -> []
  | Seq (c, c') -> tables c @ tables c'
  | Select (_, cs) ->
      concatMap cs ~c:( @ ) ~init:(Some []) ~f:(fun (_, c) -> tables c)
  | Apply t -> [t.name]

let rec num_nodes c =
  1
  +
  match c with
  | Skip -> 0
  | Assign (_, e) -> Expr.num_nodes e
  | Seq (c1, c2) -> num_nodes c1 + num_nodes c2
  | Assume t -> Test.num_nodes t
  | Select (_, cases) ->
      List.fold cases ~init:0 ~f:(fun acc (b, c) ->
          acc + Test.num_nodes b + num_nodes c)
  | Apply {keys; actions; default; _} ->
      List.length keys + num_nodes default
      + List.fold actions ~init:0 ~f:(fun acc (_, data, act) ->
            acc + List.length data + num_nodes act)

let rec get_actions (c : t) : (string * (string * int) list * t) list =
  match c with
  | Skip | Assign _ | Assume _ -> []
  | Seq (c1, c2) -> get_actions c1 @ get_actions c2
  | Select (_, cases) -> List.bind cases ~f:(fun (_, c) -> get_actions c)
  | Apply {actions; default; _} -> ("default", [], default) :: actions

let rec num_table_paths (c : t) : Bigint.t =
  let open Bigint in
  match c with
  | Skip | Assign _ | Assume _ -> one
  | Seq (c1, c2) -> num_table_paths c1 * num_table_paths c2
  | Select (_, cases) ->
      List.fold cases ~init:one ~f:(fun acc (_, c) ->
          let res = num_table_paths c in
          if res = one then acc else acc + res)
  | Apply {actions; _} -> of_int (List.length actions) + one

(* for the default action*)

let rec num_paths (c : t) : Bigint.t =
  let open Bigint in
  match c with
  | Skip | Assign _ | Assume _ -> one
  | Seq (c1, c2) -> num_paths c1 * num_paths c2
  | Select (_, cases) ->
      List.fold cases ~init:one ~f:(fun acc (_, c) -> acc + num_paths c)
  | Apply _ ->
      (* Assume apply will execute the default action *)
      one

let free_keys =
  List.filter_map ~f:(fun k ->
      if Key.has_value k then None else Some (Key.to_sized k))

let rec frees typ (c : t) : (string * int) list =
  ( match c with
  | Skip -> []
  | Assign (f, e) -> (
    match typ with
    | `Hole -> Expr.frees typ e
    | `Var -> (f, Expr.size e) :: Expr.frees typ e )
  | Seq (c, c') -> frees typ c @ frees typ c'
  | Assume t -> Test.frees typ t
  | Select (_, ss) ->
      List.fold ss ~init:[] ~f:(fun fvs (test, action) ->
          Test.frees typ test @ frees typ action @ fvs)
  | Apply t ->
      free_keys t.keys
      @ List.fold t.actions ~init:(frees typ t.default)
          ~f:(fun acc (_, data, a) ->
            acc
            @ ( frees typ a
              |> List.filter ~f:(fun (x, _) ->
                     List.for_all (List.map data ~f:fst) ~f:(String.( <> ) x))
              )) )
  |> dedup

let vars = frees `Var

let holes = frees `Hole

let rec multi_vals c : Value.t list =
  match c with
  | Skip -> []
  (* Only collect _tested_ inputs*)
  | Assign (_, e) -> Expr.multi_vals e
  | Seq (c, c') -> multi_vals c @ multi_vals c'
  | Assume t -> Test.multi_vals t
  | Select (_, ss) ->
      concatMap ss ~init:(Some []) ~c:( @ ) ~f:(fun (test, action) ->
          Test.multi_vals test @ multi_vals action)
  | Apply t ->
      List.fold t.actions ~init:(multi_vals t.default) ~f:(fun rst act ->
          rst @ multi_vals (trd3 act))

let rec holify_cmd ~f holes c : t =
  match c with
  | Skip -> c
  | Assign (v, e) -> v %<-% Expr.holify ~f holes e
  | Assume t -> Assume (Test.holify ~f holes t)
  | Seq (c, c') -> holify_cmd ~f holes c %:% holify_cmd ~f holes c'
  | Select (styp, cases) ->
      List.map cases ~f:(fun (t, c) ->
          (Test.holify ~f holes t, holify_cmd ~f holes c))
      |> select styp
  | Apply t ->
      Apply
        { name= t.name
        ; keys= t.keys
        ; actions=
            List.map t.actions ~f:(fun (n, data, act) ->
                let holes' =
                  List.filter holes ~f:(fun h ->
                      List.for_all (List.map data ~f:fst)
                        ~f:(String.( <> ) h))
                in
                (n, data, holify_cmd ~f holes' act))
        ; default= holify_cmd ~f holes t.default }

let holify ?(f = Fn.id) holes c = holify_cmd ~f holes c

let sequence = List.reduce_exn ~f:seq

let rec assigned_vars = function
  | Skip | Assume _ -> StringSet.empty
  | Assign (f, _) -> StringSet.singleton f
  | Seq (c1, c2) -> StringSet.union (assigned_vars c1) (assigned_vars c2)
  | Select (_, cs) ->
      List.fold cs ~init:StringSet.empty ~f:(fun acc (_, c) ->
          StringSet.union acc (assigned_vars c))
  | Apply t ->
      List.fold t.actions ~init:(assigned_vars t.default)
        ~f:(fun acc (_, _, act) -> StringSet.union acc @@ assigned_vars act)

let rec get_schema_of_table name phys =
  match phys with
  | Skip | Assume _ | Assign _ -> None
  | Seq (c1, c2) -> (
    match get_schema_of_table name c1 with
    | None -> get_schema_of_table name c2
    | Some ks -> Some ks )
  | Select (_, cs) ->
      List.find_map cs ~f:(fun (_, c) -> get_schema_of_table name c)
  | Apply t ->
      if String.(name = t.name) then Some (t.keys, t.actions, t.default)
      else None

let rec get_tables_actions = function
  | Skip | Assume _ | Assign _ -> []
  | Seq (c1, c2) -> get_tables_actions c1 @ get_tables_actions c2
  | Select (_, cs) ->
      List.fold cs ~init:[] ~f:(fun acc (_, c) -> acc @ get_tables_actions c)
  | Apply t -> [(t.name, t.actions)]

let rec get_tables_actsizes = function
  | Skip | Assume _ | Assign _ -> []
  | Seq (c1, c2) -> get_tables_actsizes c1 @ get_tables_actsizes c2
  | Select (_, cs) ->
      List.fold cs ~init:[] ~f:(fun acc (_, c) ->
          acc @ get_tables_actsizes c)
  | Apply t -> [(t.name, List.length t.actions)]

let table_vars ?(keys_only = false) ((keys : Key.t list), acts, default) =
  let open List in
  free_keys keys
  @
  if keys_only then []
  else
    ( acts
    >>= fun (_, ad, c) ->
    vars c
    |> filter ~f:(fun (x, _) ->
           for_all ad ~f:(fun (y, _) -> String.(x <> y))) )
    @ vars default

let rec get_tables_vars ?(keys_only = false) = function
  | Skip | Assume _ | Assign _ -> []
  | Seq (c1, c2) ->
      get_tables_vars ~keys_only c1 @ get_tables_vars ~keys_only c2
  | Select (_, cs) ->
      List.fold cs ~init:[] ~f:(fun acc (_, c) ->
          acc @ get_tables_vars ~keys_only c)
  | Apply t ->
      [(t.name, table_vars ~keys_only (t.keys, t.actions, t.default))]

let rec get_tables_keys = function
  | Skip | Assume _ | Assign _ -> []
  | Seq (c1, c2) -> get_tables_keys c1 @ get_tables_keys c2
  | Select (_, cs) -> List.bind cs ~f:(fun (_, c) -> get_tables_keys c)
  | Apply t -> [(t.name, t.keys)]
