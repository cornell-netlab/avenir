open Core
open Util

let well_formed_adds (params : Parameters.t) problem encode_tag =
  let open Test in
  let phys_inst = Problem.phys_edited_instance params problem in
  let phys = Problem.phys problem in
  Cmd.tables phys
  |> concatMap ~c:and_ ~init:(Some True) ~f:(fun t ->
         let t_rows = Instance.get_rows phys_inst t in
         Hole.add_row_hole t
         %=% Expr.value (1, 1)
         %=>% concatMap t_rows ~init:(Some True) ~c:( %&% )
                ~f:(fun (ms, _, _) ->
                  !%(concatMap ms ~init:(Some False) ~c:( %&% )
                       ~f:(Match.to_valuation_test t encode_tag))))

let adds_are_reachable params (problem : Problem.t) fvs encode_tag =
  let open Test in
  let phys = Problem.phys problem in
  let in_pkt = Problem.cexs problem |> List.hd_exn |> fst in
  Cmd.get_tables_keys phys
  |> List.fold ~init:True ~f:(fun acc (tbl_name, keys) ->
         and_ acc
         @@ impl (Hole.add_row_hole tbl_name %=% Expr.value (1, 1))
         @@ FastCX.is_reachable encode_tag params problem fvs in_pkt tbl_name
              keys)

let single problem query_holes =
  let open Test in
  List.fold query_holes ~init:True ~f:(fun acc (h, sz) ->
      and_ acc
        ( if
          Hole.is_add_row_hole h
          && List.exists (Problem.phys_edits problem) ~f:(fun e ->
                 String.(
                   Edit.table e
                   = String.chop_prefix_exn h ~prefix:Hole.add_row_prefix))
        then Hole (h, sz) %=% Expr.value (0, sz)
        else acc ))

let restrict_mask query_holes =
  let open Test in
  List.fold query_holes ~init:True ~f:(fun acc (h, sz) ->
      and_ acc
      @@
      if String.is_suffix h ~suffix:"_mask" then
        let h_value = String.chop_suffix_exn h ~suffix:"_mask" in
        let all_1s = max_int sz in
        and_ acc
        @@ bigor
             [ Hole (h, sz) %=% Value (Value.big_make (all_1s, sz))
             ; bigand
                 [ Hole (h_value, sz) %=% Expr.value (0, sz)
                 ; Hole (h, sz) %=% Expr.value (0, sz) ] ]
      else True)

let active_domain_restrict params problem query_holes : Test.t =
  let open Test in
  let ints =
    Cmd.multi_vals (Problem.log_gcl_program params problem)
    @ Cmd.multi_vals (Problem.phys_gcl_program params problem)
    |> List.dedup_and_sort ~compare:Stdlib.compare
    |> List.filter ~f:(fun v ->
           Bigint.(Value.get_bigint v <> zero && Value.get_bigint v <> one))
  in
  List.fold query_holes ~init:True ~f:(fun acc (h, sz) ->
      let restr =
        List.fold ints ~init:False ~f:(fun acci v ->
            let szi = Value.size v in
            let i = Value.get_bigint v in
            or_ acci
            @@
            if
              sz = szi
              && (not (String.is_suffix h ~suffix:"_mask"))
              && (not (Hole.is_add_row_hole h))
              && (not (Hole.is_delete_hole h))
              && not (Hole.is_which_act_hole h)
            then
              Hole (h, sz)
              %=% Value (Value.big_make (i, szi))
              %+% (Hole (h, sz) %=% Expr.value (0, szi))
              %+% (Hole (h, sz) %=% Expr.value (1, szi))
            else False)
      in
      if Test.equals restr False then acc else acc %&% restr)

let no_defaults (params : Parameters.t) fvs phys =
  let open Test in
  List.filter (Cmd.holes phys) ~f:(fun (v, _) ->
      List.for_all fvs ~f:(fun (v', _) ->
          if
            List.exists
              [ v'
              ; Hole.add_row_prefix
              ; Hole.delete_row_prefix
              ; Hole.which_act_prefix ] ~f:(fun substring ->
                String.is_substring v ~substring)
          then (
            if params.debug then
              Printf.printf "%s matches %s, so skipped\n%!" v v' ;
            false )
          else (
            if params.debug then
              Printf.printf "%s misses  %s, so kept\n%!" v v' ;
            true )))
  |> List.fold ~init:True ~f:(fun acc (v, sz) ->
         acc %&% (Hole (v, sz) %<>% Expr.value (0, sz)))
