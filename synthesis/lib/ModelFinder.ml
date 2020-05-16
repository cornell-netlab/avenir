open Core
open Ast
open Manip
open Util
open Prover

type opts =
  {injection : bool;
   hints : bool;
   paths : bool;
   only_holes: bool;
   mask : bool}

type t = {
    schedule : opts list;
    search_space : (test * Hint.t list) list;
  }

let string_of_opts ({injection;hints;paths;only_holes;mask} : opts) : string =
  let s = if injection then "injection  " else "" in
  let s = if hints then s^"hints  " else s in
  let s = if paths then s^"paths  " else s in
  let s = if only_holes then s^"only holes  " else s in
  let s = if mask then s^"mask" else s in
  if s = "" then "none" else s

(* None > Mask > Paths > Injection > Hints > Only_Holes *)
let rec make_schedule ({injection;hints;paths;only_holes;mask} as opt) =
  opt ::
    (* if only_holes then
     *   make_schedule {opt with only_holes = false}
     * else if hints || injection then
     *   make_schedule {opt with injection = false; hints = false}
     * else *)
    if injection || hints || paths || only_holes then
      [{injection=false;hints=false;paths=false;only_holes=false;mask=mask}]
    else
      []

let make_searcher (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) : t =
  let schedule = make_schedule {
                     injection = params.injection;
                     hints = params.injection;
                     paths = params.monotonic;
                     only_holes = params.monotonic;
                     mask = params.widening;
                   } in
  {schedule; search_space = []}
  

let reindex_for_dels problem tbl i =
  Problem.phys_edits problem
  |>  List.fold ~init:(Some i)
        ~f:(fun cnt edit ->
          match cnt, edit with
          | Some n, Del (t,j) when t = tbl ->
             if i = j
             then None
             else Some (n-1)
          | _ -> cnt
        )

let compute_deletions pkt (problem : Problem.t) =
  let open Problem in
  let phys_inst = Problem.phys_inst problem in
  StringMap.fold phys_inst ~init:[]
    ~f:(fun ~key:table_name ~data:rows dels ->
      dels @ List.filter_mapi rows ~f:(fun i _ ->
                 match reindex_for_dels problem table_name i with
                 | None -> None
                 | Some i' -> Some (table_name, i')
               )
    )


let apply_opts (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) (opts : opts)  =
  let (in_pkt, out_pkt) = Problem.cexs problem |> List.hd_exn in
  let st = Time.now () in
  let deletions = compute_deletions in_pkt problem in
  let hints = if opts.hints then
                let open Problem in
                Hint.construct (log problem) (phys problem) (log_edits problem |> List.hd_exn)
              else [] in
  let hole_protocol = if opts.only_holes
                      then Instance.OnlyHoles hints
                      else Instance.WithHoles (deletions, hints) in
  let hole_type =  if opts.mask then `Mask else `Exact in
  let phys = Problem.phys_gcl_holes params problem hole_protocol hole_type  in
  if params.debug then Printf.printf "NEW Phys\n %s\n%!" (string_of_cmd phys);
  ProfData.update_time !data.model_holes_time st;
  let st = Time.now () in
  let fvs = List.(free_vars_of_cmd phys
                  |> filter ~f:(fun x -> exists (Problem.fvs problem) ~f:(Stdlib.(=) x))) in
  (* let fvs = problem.fvs in *)
  let in_pkt_form, out_pkt_form = Packet.to_test in_pkt ~fvs, Packet.to_test out_pkt ~fvs in
  let wp_list =
    if opts.paths then
      wp_paths ~no_negations:true phys out_pkt_form
    else
      [phys, wp phys out_pkt_form]
  in
  ProfData.update_time !data.search_wp_time st;
  let tests =
    List.filter_map wp_list
      ~f:(fun (cmd, in_pkt_wp) ->
        if in_pkt_wp = False || not (has_hole_test in_pkt_wp) then None else
          (* let st = Time.now() in *)
          (* Printf.printf "\n\nWP: %s\n\n%!" (string_of_test in_pkt_wp); *)

          let () = if params.debug then
                     Printf.printf "Checking path with hole!\n  %s\n\n%!" (string_of_cmd cmd) in
          let spec = in_pkt_form %=>% in_pkt_wp in
          (* let () = if params.debug then
           *            Printf.printf "test starts:\n%!  %s\n\n%!" (string_of_test spec) in *)
          let wf_holes = List.fold (Problem.phys problem |> get_tables_actsizes) ~init:True
                           ~f:(fun acc (tbl,num_acts) ->
                             acc %&%
                               (Hole(Hole.which_act_hole_name tbl,max (log2 num_acts) 1)
                                %<=% mkVInt(num_acts-1,max (log2 num_acts) 1))) in
          (* Printf.printf "well_formedness computed\n%!"; *)
          (* let widening_constraint =
           *   if opts.mask then
           *     List.fold (holes_of_test in_pkt_wp)  ~init:True
           *       ~f:(fun acc (hole, sz) ->
           *         match String.chop_suffix hole ~suffix:"_mask" with
           *         | Some _ ->
           *            (Hole(hole, sz) %=% mkVInt(pow 2 sz -1, sz)) %+% ((Hole(hole,sz) %=% mkVInt(0,sz)))
           *         | None -> acc
           *
           *       )
           *   else True
           * in
           * Printf.printf "Widening constriaint:\n  %s \n\n%!" (string_of_test widening_constraint); *)
          (* Printf.printf "Widening and well-formedness computed\n%!"; *)
          let pre_condition =
            (Problem.model_space problem) %&% wf_holes %&% spec
            |> Injection.optimization {params with injection = opts.injection} problem
          in
          (* Printf.printf "Pre_condition computed\n%!"; *)
          let query_test = wf_holes %&%(* widening_constraint %&%*) pre_condition in
          let active_domain_restrict =
            let ints = (multi_ints_of_cmd (Problem.log_gcl_program params problem))
                       @ (multi_ints_of_cmd (Problem.phys_gcl_program params problem)) in
            let holes = holes_of_test query_test |> List.dedup_and_sort ~compare:(Stdlib.compare) in
            List.fold holes ~init:True
              ~f:(fun acc (h,sz) ->
                if String.is_prefix h ~prefix:Hole.which_act_prefix then acc else
                if String.is_substring h ~substring:"_mask"
                then acc %&%  ((Hole(h, sz) %=% mkVInt(pow 2 sz - 1, sz)) %+% ((Hole(h,sz) %=% mkVInt(0,sz))))
                else
                  (* if List.exists (Problem.fvs problem) ~f:(fun (v,_) ->  String.is_substring h ~substring:v || not (String.is_substring h ~substring:"?"))
                   * then *)
                    let restr =
                      List.fold ints
                        ~init:(((Hole(h,sz) %=% mkVInt(0,sz)) %+% (Hole(h,sz) %=% mkVInt(1,sz))))
                        ~f:(fun acci (i,szi) ->
                          acci %+% if sz = szi
                                   then Hole(h,sz) %=% Value(Int(i,szi))
                                   else False)
                    in
                    acc %&% restr
                  (* else
                   *   acc *)
              )
          in
          (* Printf.printf "active domain restr \n %s\n%!" (string_of_test active_domain_restrict); *)
          let out_test = active_domain_restrict %&% query_test in
          (* Printf.printf "outtest_computed\n%!"; *)
          let () = if params.debug then Printf.printf "test is \n   %s\n\n%!" (string_of_test out_test) in
          Some (out_test, hints)) in
  tests

let holes_for_table table phys =
  match get_schema_of_table table phys with
  | None -> failwith ""
  | Some (ks, acts, _) ->
     List.bind ks ~f:(fun (k,_) ->
         let lo,hi = Hole.match_holes_range table k in
         let v,m = Hole.match_holes_mask table k in
         [Hole.match_hole_exact table k;lo;hi;v;m]
         |> List.dedup_and_sort ~compare:String.compare
       )
     @ List.(acts >>= fun (params,_) ->
             params >>| fst )

let holes_for_other_actions table phys actId =
  match get_schema_of_table table phys with
  | None -> failwith ""
  | Some (ks, acts, _) ->
     List.foldi acts ~init:[]
       ~f:(fun i acc (params,_) ->
         acc @ if i = Bigint.to_int_exn actId then [] else List.map params ~f:fst
       )

let minimize_model (model : value StringMap.t) (phys : cmd) : value StringMap.t =
  StringMap.keys model
  |> List.filter ~f:(fun k -> String.is_prefix k ~prefix:Hole.add_row_prefix)
  |> List.fold ~init:model
       ~f:(fun model key ->
         match StringMap.find model key with
         | Some Int(v,_) when v = Bigint.zero ->
            let table = String.chop_prefix_exn key ~prefix:Hole.add_row_prefix in
            holes_for_table table phys
            |> List.fold ~init:model ~f:(StringMap.remove)
         | Some Int(v,_) when v = Bigint.one ->
            let table = String.chop_prefix_exn key ~prefix:Hole.add_row_prefix in
            begin
              match (StringMap.find model (Hole.which_act_hole_name table)) with
              | Some (Int (actId, _)) ->
                 holes_for_other_actions table phys actId
                 |> List.fold ~init:model ~f:(StringMap.remove)
              | None -> model
            end
         | _ -> model
       )


let rec search (params : Parameters.t) data problem t : ((value StringMap.t * t) option)=
  match t.search_space, t.schedule with
  | [], [] ->
     if params.debug then Printf.printf "Search failed\n%!";
     None
  | [], (opts::schedule) ->
     let () =
       if params.debug then
         Printf.printf "\nusing optimization to |%s|\n\n%!"
           (string_of_opts opts)
     in
     let search_space = apply_opts params data problem opts in
     (* Printf.printf "Searching with %d opts and %d paths\n%!" (List.length schedule) (List.length search_space); *)
     search params data problem {schedule; search_space}
  | (test,hints)::search_space, schedule ->
     (* Printf.printf "Check sat\n%!"; *)
     (* if params.debug then Printf.printf "MODELSPACE:\n%s\nTEST\n%s\n%!" (Problem.model_space problem |> string_of_test) (test |> string_of_test); *)
     let model_opt, _ = check_sat params (Problem.model_space problem %&% test) in
     (* Printf.printf "Sat Checked\n%!\n"; *)
     match model_opt with
     | Some model ->
        (* Printf.printf "Found a model, done \n%!"; *)
        let model = minimize_model (Hint.add_to_model (Problem.phys problem) hints model) (Problem.phys problem) in
        if Problem.seen_attempt problem model then
          search params data problem {schedule; search_space}
        else begin
            if params.debug then
              Printf.printf "IsNOVEL??? \n    %s \n"
                (Problem.model_space problem
                 |> fixup_test model
                 |> string_of_test);
            Some (Hint.add_to_model (Problem.phys problem) hints model,t)
          end
     | _ ->
        (* Printf.printf "No model, keep searching with %d opts and %d paths \n%!" (List.length schedule) (List.length search_space); *)
        search params data problem {schedule; search_space}
