open Core
open Util
open Ast
open Manip

type t = Add of string * Row.t (* Name of table *)
       | Del of string * int

let table = function
  | Add (name, _)
    | Del (name, _) -> name

let is_delete = function
  | Add _ -> false
  | Del _ -> true

let has_delete = List.exists ~f:(is_delete)

let split = List.fold ~init:([],[])
              ~f:(fun (dels,adds) e ->
                match e with
                | Add _ -> (dels, adds @ [e])
                | Del _ -> (dels @ [e], adds))

let get_deletes = List.filter_map ~f:(function
                      | Add _ -> None
                      | Del (n,i) -> Some (n,i)
                    )

let get_matches_exn = function
  | Add (_, (matches,_,_)) -> matches
  | Del _ -> failwith "[Edit.get_matches] tried to get matches of a deletion"

let get_row_exn = function
  | Add (_, r) -> r
  | Del _ -> failwith "[Edit.get_row_exn] tried to get row of a deletion"

let to_test phys e =
  match e with
  | Del(t,i) -> Hole.delete_hole i t %=% mkVInt(1,1)
  | Add(t,(ms,ds,i)) ->
     match get_schema_of_table t phys with
     | None -> failwith @@ Printf.sprintf "Couldn't find table %s" t
     | Some (_,actions,_) ->
        let actSize = max (log2 (List.length actions)) 1 in
        Hole.add_row_hole t %=% mkVInt(1,1)
        %&%
          (Hole.which_act_hole t actSize %=% mkVInt(i,actSize))
        %&%
          (Match.test_hole_of_lists t ms)
        %&%
          (Row.test_of_data t i (List.nth_exn actions i |> snd3) ds)

let test_of_list phys es =
  List.(map es ~f:(to_test phys) |> reduce_exn ~f:(%&%))

let to_string e =
  match e with
  | Add (nm, row) -> Printf.sprintf "ADD,%s,%s" nm (Row.to_string row)
  | Del (nm, idx) -> Printf.sprintf "DEL,%s,%d" nm idx

let to_bmv2_string (cmd : cmd) e =
  match e with
  | Add (nm, (matches, action_data, action_id)) ->
     let (_,actions,_) =
       get_schema_of_table nm cmd
       |> Option.value_exn ~message:(Printf.sprintf "Couldn't find %s" nm)
     in
     let (act_name,_,_) = List.nth_exn actions action_id in
     let bmv2_matches =
       List.fold matches ~init:""
         ~f:(fun acc m -> Printf.sprintf "%s %s" acc (Match.to_bmv2_string m))
     in
     let bmv2_data =
       List.fold action_data ~init:""
         ~f:(fun acc d -> Printf.sprintf "%s %s" acc (Value.to_bmv2_string d))
     in
     Printf.sprintf "table_add %s %s%s =>%s" nm act_name bmv2_matches bmv2_data

  | Del (t,i) ->
     Printf.sprintf "table_delete %s %d" t i

let list_to_string es =
  List.fold es ~init:"" ~f:(fun acc e -> Printf.sprintf "%s\n%s" acc (to_string e))

let eq_tests e e' =
  match e, e' with
  | Add(nm,(m,_,_)), Add(nm',(m',_,_)) when nm = nm'
    -> m = m'
  | _ -> false

let equal e e' = Stdlib.(e = e')

let get_ith_match ~i (e : t) =
  match e with
  | Add (_, row) -> Row.get_ith_match i row
  | Del (_, _) -> None

let read_vars cmd = function
  | Del _ -> failwith "[read_vars] undefined for del"
  | Add (table, (ms, _, aid)) ->
     match get_schema_of_table table cmd with
     | None ->
        failwith @@ Printf.sprintf "Couldn't find %s" table
     | Some (keys, actions, _) ->
        let reads = List.nth_exn actions aid |> action_reads in
        Match.relevant_keys ms
        |> StringSet.of_list
        |> StringSet.union reads
        |> StringSet.(union @@ of_list @@ fsts3 keys)

let write_vars cmd = function
  | Del _ -> failwith "[write_vars] undefined for del"
  | Add (table, (_, _, aid)) ->
     match get_schema_of_table table cmd with
     | None ->
        failwith @@ Printf.sprintf "Couldn't find %s" table
     | Some (_, actions,_) ->
        List.nth_exn actions aid
        |> trd3
        |> assigned_vars

let to_model_alist phys e =
  match e with
  | Del(t,i) ->
     [Hole.delete_row_hole_name i t, Value.make(1,1)]
  | Add(t,(ms,ds,i)) ->
     match get_schema_of_table t phys with
     | None -> failwith @@ Printf.sprintf "Couldn't find table %s" t
     | Some (_,actions,_) ->
        let actSize = max (log2 (List.length actions)) 1 in
        [Hole.add_row_hole_name t, Value.make(1,1);
         Hole.which_act_hole_name t, Value.make(i,actSize)]
        @ Match.list_to_model_alist t ms
        @ Row.model_alist_of_data t i (List.nth_exn actions i |> snd3) ds

let to_model phys e =
  to_model_alist phys e
  |> Model.of_alist_exn

let list_to_model phys es =
  List.bind es ~f:(to_model_alist phys)
  |> Model.of_alist_exn

(** [negate phys m es] computes a test ensuring that [es] can never again be
    synthesized, if [es] is nonempty, otherwise, it negates the assignments in
    the model [m].
    Assume that [Edit.extract phys m]  = [t].
 *)
let negate phys (model : Model.t) (es : t list) : test =
  !%(if List.is_empty es
     then Model.fold model
            ~init:True
            ~f:(fun ~key ~data:v acc ->
              mkAnd acc @@
                Hole(key, Value.size v) %=% Value v)
     else
       test_of_list phys es
    )

let extract_dels_adds phys (m : Model.t) =
  Model.fold m ~init:([],[]) (*Deletions, additions*)
    ~f:(fun ~key ~data acc ->
      (* Printf.printf "%s\n" (string_of_map m); *)
      match Hole.find_add_row key with
      | None -> begin
          match Hole.find_delete_row key with
          | Some (table_name,row_idx) when data |> Value.get_bigint = Bigint.one ->
             (Del(table_name, row_idx) :: fst acc, snd acc)
          |  _ -> acc
        end
      | Some tbl ->
         if data |> Value.get_bigint = Bigint.one then
           let act = match Model.find m (Hole.which_act_hole_name tbl) with
             | None ->
                Printf.sprintf "WARNING:: Couldn't find %s even though I found %s to be true\n%!"  (Hole.which_act_hole_name tbl) key
                |> failwith
             | Some v -> Value.get_int_exn v in
           (* Printf.printf "Making new row from \n %s \n in tbl %s and action %d \n%!" (string_of_map m) tbl act; *)
           match Row.mk_new_row m phys tbl None act with
           | None -> failwith (Printf.sprintf "Couldn't make new row in table %s\n" tbl)
           | Some row ->
              (fst acc, Add (tbl, row) :: snd acc)
         else acc)


let of_model phys (m : Model.t) =
  let dels, adds = extract_dels_adds phys m in
  List.dedup_and_sort dels ~compare:(fun i j ->
      match i, j with
      | Del(_,ix), Del(_, jx) -> compare jx ix
      | _, _ -> failwith "dels list contains an add")
  @ adds
