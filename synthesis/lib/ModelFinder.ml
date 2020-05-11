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
  


let compute_deletions pkt (problem : Problem.t) =
  let open Problem in
  let phys_inst = Problem.phys_inst problem in
  StringMap.fold phys_inst ~init:[]
    ~f:(fun ~key:table_name ~data:rows dels ->
      dels @ List.mapi rows ~f:(fun i _ -> (table_name,i))
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
        let widening_constraint =
          if opts.mask then
            List.fold (holes_of_test in_pkt_wp)  ~init:True
              ~f:(fun acc (hole, sz) ->
                match String.chop_suffix hole ~suffix:"_mask" with
                | Some hole_val ->
                   Hole(hole_val,sz) %=% (mkMask (Hole(hole_val,sz)) (Hole(hole, sz)))
                   %&% ((Hole(hole,sz) %=% mkVInt(0, sz))
                        %+% (Hole(hole,sz) %=% Value(Int(Bigint.((pow (of_int 2) (of_int sz)) - one), sz))))
                | None -> acc

              )
          else True
        in
        (* Printf.printf "Widening and well-formedness computed\n%!"; *)
        let pre_condition =
          (Problem.model_space problem) %&% wf_holes %&% spec
          |> Injection.optimization {params with injection = opts.injection} problem
        in
        (* Printf.printf "Pre_condition computed\n%!"; *)
        let out_test = wf_holes %&% widening_constraint %&% pre_condition in
        (* Printf.printf "outtest_computed\n%!"; *)
        let () = if params.debug then Printf.printf "test is \n   %s\n\n%!" (string_of_test pre_condition) in
        Some (out_test, hints)) in
  tests

let holes_for_table table phys =
  match get_schema_of_table table phys with
  | None -> failwith ""
  | Some (ks, acts, _) ->
     List.map ks ~f:(fun (k,_) -> "?" ^ k )
     @ List.(acts >>= fun (params,_) ->
             params >>| fst )

let minimize_model (model : value StringMap.t) (phys : cmd) : value StringMap.t =
  StringMap.keys model
  |> List.filter ~f:(String.is_prefix ~prefix:"?AddRowTo")
  |> List.fold ~init:model
       ~f:(fun model key ->
         match StringMap.find model key with
         | Some Int(v,_) when v = Bigint.zero ->
            let table = String.chop_prefix_exn key ~prefix:"?AddRowTo" in
            holes_for_table table phys
            |> List.fold ~init:model ~f:(StringMap.remove)
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
