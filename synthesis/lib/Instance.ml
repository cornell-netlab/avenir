open Core
open Util
open Manip

type t = Row.t list StringMap.t

(* Keys are table names, Rows are table rows*)

type interp =
  | NoHoles
  | OnlyHoles of Hint.t list
  | WithHoles of (string * int) list * Hint.t list

let interp_equal i1 i2 =
  match (i1, i2) with
  | NoHoles, NoHoles -> true
  | OnlyHoles hs, OnlyHoles hs' -> List.equal Hint.equal hs hs'
  | WithHoles (vs, hs), WithHoles (vs', hs') ->
      List.equal
        (fun (x, sz) (x', sz') -> String.(x = x') && sz = sz')
        vs vs'
      && List.equal Hint.equal hs hs'
  | _ -> false

let to_string (inst : t) : string =
  StringMap.fold inst ~init:"" ~f:(fun ~key:table_name ~data:rows acc ->
      Printf.sprintf "%s\n%s\n%s" acc table_name
        (Row.list_to_string ~tab:"\t" rows))

let equal (i1 : t) (i2 : t) : bool =
  StringMap.equal (List.equal Row.equals) i1 i2

let empty = StringMap.empty

let update (params : Parameters.t) (inst : t) (e : Edit.t) =
  match e with
  | Add (tbl, row) ->
      StringMap.update inst tbl ~f:(fun rows_opt ->
          match rows_opt with
          | None -> [row]
          | Some rows when params.above -> rows @ [row]
          | Some rows -> row :: rows)
  | Del (tbl, i) ->
      StringMap.change inst tbl ~f:(function
        | None -> None
        | Some rows ->
            List.filteri rows ~f:(fun j _ -> i <> List.length rows - j - 1)
            |> Some)

let rec update_list params (inst : t) (edits : Edit.t list) =
  match edits with
  | [] -> inst
  | e :: es -> update_list params (update params inst e) es

let of_edits (params : Parameters.t) (edits : Edit.t list) =
  update_list params empty edits

let get_rows inst table : Row.t list =
  StringMap.find inst table |> Option.value ~default:[]

let get_row (inst : t) (table : string) (idx : int) : Row.t option =
  let rs = get_rows inst table in
  List.(nth (rev rs) idx)

let get_rows_before (inst : t) (table : string) (idx : int) : Row.t list =
  let rs = get_rows inst table |> List.rev in
  List.filteri rs ~f:(fun i _ -> i < idx) |> List.rev

let get_row_exn inst table idx : Row.t =
  match get_row inst table idx with
  | None -> failwith @@ Printf.sprintf "Invalid row %d in table %s" idx table
  | Some row -> row

let set_rows inst ~table ~rows = StringMap.set inst ~key:table ~data:rows

let negate_rows inst tbl =
  let open Test in
  get_rows inst tbl
  |> List.fold ~init:True ~f:(fun acc (matches, _, _) ->
         acc %&% !%(Match.list_to_test matches))

let overwrite (old_inst : t) (new_inst : t) : t =
  StringMap.fold new_inst ~init:old_inst ~f:(fun ~key ~data acc ->
      StringMap.set acc ~key ~data)

let size : t -> int =
  StringMap.fold ~init:0 ~f:(fun ~key:_ ~data -> ( + ) (List.length data))

let rec apply ?(no_miss = false) ?(ghost_edits = StringMap.empty)
    (params : Parameters.t) (tag : interp) encode_tag (inst : t)
    (prog : Cmd.t) : Cmd.t =
  match prog with
  | Skip | Assign _ | Assume _ -> prog
  | Seq (c1, c2) ->
      let c1' = apply ~no_miss ~ghost_edits params tag encode_tag inst c1 in
      let c2' = apply ~no_miss ~ghost_edits params tag encode_tag inst c2 in
      Cmd.seq c1' c2'
  | Select (typ, ss) ->
      let ss =
        List.fold ss ~init:[] ~f:(fun acc (t, c) ->
            let c' =
              apply params ~no_miss ~ghost_edits tag encode_tag inst c
            in
            acc @ [(t, c')])
      in
      Cmd.select typ ss
  | Apply t ->
      let actSize = max (log2 (List.length t.actions)) 1 in
      let rows = StringMap.find_multi inst t.name in
      let ghosts =
        StringMap.find ghost_edits t.name |> Option.value ~default:[]
      in
      let selects =
        match tag with
        | OnlyHoles _ -> []
        | _ ->
            (* Printf.printf "adding %d rows to %s\n%!" (List.length rows)
               t.name; *)
            List.foldi rows ~init:[] ~f:(fun i acc (matches, data, action) ->
                let instrument action =
                  if List.exists ghosts ~f:(( = ) i) then
                    let ghost = Printf.sprintf "%s_hits_row_%d" t.name i in
                    Cmd.(
                      sequence
                        [ ghost %<-% Expr.value (1, 1)
                        ; ordered
                            [ ( Test.(Var (ghost, 1) %=% Expr.value (1, 1))
                              , action )
                            ; (True, Skip) ] ])
                  else action
                in
                let cond =
                  Test.and_ (Match.list_to_test matches)
                  @@
                  match tag with
                  | WithHoles (ds, _) ->
                      let i = List.length rows - i - 1 in
                      if List.exists ds ~f:(Stdlib.( = ) (t.name, i)) then
                        Test.(
                          Hole.delete_hole i t.name %=% Expr.value (0, 1))
                      else Test.True
                  | _ -> True
                in
                if action >= List.length t.actions then acc
                else
                  let action =
                    List.nth_exn t.actions action |> bind_action_data data
                  in
                  (cond, instrument action) :: acc)
      in
      let holes =
        match tag with
        | NoHoles -> []
        | _ ->
            List.mapi t.actions ~f:(fun i (_, params, act) ->
                ( Hole.table_hole encode_tag t.keys t.name i actSize
                , Cmd.holify
                    ~f:(fun (h, sz) -> (Hole.action_data t.name i h sz, sz))
                    (List.map params ~f:fst) act ))
      in
      let dflt_row = if no_miss then [] else [(Test.True, t.default)] in
      let tbl_select =
        (if params.above then holes @ selects else selects @ holes)
        @ dflt_row
        |> Cmd.ordered
      in
      tbl_select

let verify_apply ?(no_miss = false) params inst cmd =
  (*TODO the `Exact tag is unused, should fold into the tag type*)
  apply ~no_miss params NoHoles `Exact inst cmd

let project slice inst =
  StringMap.merge slice inst ~f:(fun ~key -> function
    | `Left [] -> None
    | `Left _ ->
        Printf.sprintf "have slice for table, %s its' not in the instance"
          key
        |> failwith
    | `Right _ -> Some []
    | `Both (slice, rows) ->
        List.fold slice ~init:[] ~f:(fun acc n ->
            acc @ [List.nth_exn rows n])
        |> Some)

let fold inst ~init ~f =
  StringMap.fold inst ~init ~f:(fun ~key ~data -> f ~table:key ~rows:data)
