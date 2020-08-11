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
   mask : bool;
   restrict_mask : bool;
   nlp : bool;
   annot : bool;
   single : bool;
   domain : bool;
   no_defaults : bool;
  }

type t = {
    schedule : opts list;
    search_space : (test * Hint.t list) list;
  }

let string_of_opts (opts) : string =
  let s = " " in
  let s = if opts.injection then s^"injection  " else s in
  let s = if opts.hints then s^"hints  " else s in
  let s = if opts.paths then s^"paths  " else s in
  let s = if opts.only_holes then s^"only holes  " else s in
  let s = if opts.mask then s^"mask " else s in
  let s = if opts.restrict_mask then s^"restrict_mask" else s in
  let s = if opts.nlp then s^"nlp" else s in
  let s = if opts.annot then s^"annot " else s in
  let s = if opts.single then s^"single " else s in
  let s = if opts.domain then s^"domain_restrict " else s in
  let s = if opts.domain then s^"no_defaults " else s in
  if s = "" then "none" else s

let no_opts =
  {injection = false;
   hints = false;
   paths = false;
   only_holes = false;
   mask = false;
   restrict_mask = false;
   nlp = false;
   annot = false;
   single = false;
   domain = false;
   no_defaults = false;
  }


(* None > Mask > Paths > Injection > Hints > Only_Holes *)
let rec make_schedule opt =
  opt ::
    if opt.injection || opt.hints || opt.paths || opt.only_holes || opt.nlp || opt.domain then
      let opt' = {opt with injection=false;hints=false;paths=false;only_holes=false; nlp=false;domain = false} in
      opt' :: make_schedule opt'
    else if opt.no_defaults then
      let opt' = {opt with no_defaults = false} in
      opt' :: make_schedule opt'
    else []

let make_searcher (params : Parameters.t) (_ : ProfData.t ref) (_ : Problem.t) : t =
  let schedule = make_schedule {
                     injection = params.injection;
                     hints = params.hints;
                     paths = params.monotonic;
                     only_holes = params.only_holes;
                     mask = params.widening;
                     restrict_mask = params.restrict_mask;
                     annot = params.allow_annotations;
                     nlp = params.nlp;
                     single = params.unique_edits;
                     domain = params.domain;
                     no_defaults = params.no_defaults;
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

let compute_deletions (_ : value StringMap.t) (problem : Problem.t) =
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
                          (* |> refine_counter params problem in *)
  (* let () = Printf.printf "in : %s \n out: %s\n%!" (string_of_map in_pkt) (string_of_map out_pkt) in *)
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
  let phys = Problem.phys_gcl_holes {params with no_defaults = opts.no_defaults} problem hole_protocol hole_type  in
  if params.debug then Printf.printf "NEW Phys\n %s\n%!" (string_of_cmd phys);
  ProfData.update_time !data.model_holes_time st;
  let st = Time.now () in
  let fvs = List.(free_vars_of_cmd phys
                  |> filter ~f:(fun x -> exists (Problem.fvs problem) ~f:(Stdlib.(=) x))) in
  (* let fvs = problem.fvs in *)
  let in_pkt_form, out_pkt_form = Packet.to_test in_pkt ~fvs, Packet.to_test out_pkt ~fvs in
  let wp_list =
    if opts.paths then
      wp_paths `NoNegs phys out_pkt_form |> List.map ~f:(fun (c,t) -> c, in_pkt_form %=>% t)
    else
      let (sub, _(*passive_phys*), good_N, _) = good_execs fvs phys in
      let test = (apply_init_test in_pkt_form) %=>% (good_N %=>% apply_finals_sub_test out_pkt_form sub) in
      Printf.printf "--------------%s---------------\n%!" (string_of_test test);
      [phys, test ]
  in
  ProfData.update_time !data.search_wp_time st;
  let tests =
    List.filter_map wp_list
      ~f:(fun (cmd, spec) ->
        if spec = False || not (has_hole_test spec) then None else

          let () = if params.debug then
                     Printf.printf "Checking path with hole!\n  %s\n\n%!" (string_of_cmd cmd) in

          let wf_holes = List.fold (Problem.phys problem |> get_tables_actsizes) ~init:True
                           ~f:(fun acc (tbl,num_acts) ->
                             acc %&%
                               (Hole(Hole.which_act_hole_name tbl,max (log2 num_acts) 1)
                                %<=% mkVInt(num_acts-1,max (log2 num_acts) 1))) in
          let pre_condition =
            (Problem.model_space problem) %&% wf_holes %&% spec
            |> Injection.optimization {params with injection = opts.injection} problem
          in

          let query_test = wf_holes %&% (* widening_constraint %&%*) pre_condition in
          let active_domain_restrict =
            let ints = (multi_ints_of_cmd (Problem.log_gcl_program params problem))
                       @ (multi_ints_of_cmd (Problem.phys_gcl_program params problem))
                       |> List.dedup_and_sort ~compare:(Stdlib.compare)
                       |> List.filter ~f:(fun (v,_) -> Bigint.(v <> zero && v <> one)) in
            let holes = holes_of_test query_test |> List.dedup_and_sort ~compare:(Stdlib.compare) in
            List.fold holes ~init:True
              ~f:(fun acc (h,sz) ->
                if opts.single && String.is_prefix h ~prefix:Hole.add_row_prefix then
                  if opts.single && List.exists (Problem.phys_edits problem)
                       ~f:(fun e ->
                         Tables.Edit.table e = String.chop_prefix_exn h ~prefix:Hole.add_row_prefix
                       )
                  then
                    acc %&% (Hole(h,1) %=% mkVInt(0,1))
                  else
                    acc
                else if String.is_prefix h ~prefix:Hole.which_act_prefix
                then acc
                else if opts.restrict_mask && String.is_substring h ~substring:"_mask"
                then
                  let allfs = Printf.sprintf "0b%s" (String.make sz '1') |> Bigint.of_string in
                  acc %&% ((Hole(h, sz) %=% Value(Int(allfs, sz))) %+%
                             (if opts.nlp && (List.exists ["ttl";"limit";"count"]
                                          ~f:(fun s -> String.is_substring h ~substring:s))
                             then False
                             else
                               let m = ((Hole(h,sz) %=% mkVInt(0,sz))
                                        %&% (Hole(String.chop_suffix_exn h ~suffix:"_mask",sz) %=% mkVInt(0,sz))) in
                               (* Printf.printf "\tmask restr %s \n%!" (string_of_test m); *)
                               m
                          ))
                else
                  let is_addr = List.exists ["addr";"dst";"src";"mac"]
                                  ~f:(fun substring -> String.is_substring h ~substring) in
                    let restr =
                      List.fold ints
                        ~init:(
                          if opts.nlp || not is_addr
                          then
                            Hole(h,sz) %=% mkVInt(0,sz) %+% (Hole(h,sz) %=% mkVInt(1,sz))
                          else
                            False)
                        ~f:(fun acci (i,szi) ->
                          acci %+% if sz = szi
                                      && (not opts.nlp ||  (not (List.exists ["ttl";"limit";"count"]
                                                       ~f:(fun s -> String.is_substring h ~substring:s))))
                                   then Hole(h,sz) %=% Value(Int(i,szi))
                                   else False)
                    in

                    let restr = if (not opts.domain) || restr = False then True else restr in
                    let is_chosen =
                      match Problem.log_edits problem |> List.hd with
                      | None | Some (Del _) -> None
                      | Some (Add(table, (ms,_,_)))
                        -> match get_schema_of_table table (Problem.log problem) with
                           | Some (ks,_,_) ->
                              List.fold2_exn ks ms ~init:None ~f:(fun acc (k,_) m ->
                                  match acc with
                                  | None ->
                                    if String.is_substring h ~substring:k then
                                      match m with
                                      | Exact (Int(v,_) as i) when Bigint.(v <> zero) ->
                                         Some i
                                      | _ -> None
                                    else None
                                  | Some i -> Some i
                                )
                           | None -> None
                    in
                    ( if opts.nlp then
                        match is_chosen with
                        | None -> True
                        | Some i -> Hole(h,sz) %=% Value i
                      else if opts.nlp && is_addr && not(String.is_substring h ~substring:"_mask")
                      then let t = (Hole(h,sz) %<>% mkVInt(0,sz)) %=>%
                                     (Hole(h ^ "_mask",sz) %=% mkVInt(0,sz)) in
                           (* Printf.printf "asserting %s \n" (sexp_string_of_test t); *)
                           t
                      else True)
                    (* %&% ((Hole.add_row_hole "validation" %=% mkVInt(1,1)) %=>% ((Hole.add_row_hole "fwd" %=% mkVInt(0,1))
                     *                                                             %&% (Hole.add_row_hole "acl" %=% mkVInt(0,1))))
                     * %&% ((Hole.add_row_hole "fwd" %=% mkVInt(1,1)) %=>% ((Hole.add_row_hole "validation" %=% mkVInt(0,1))
                     *                                                      %&% (Hole.add_row_hole "acl" %=% mkVInt(0,1))))
                     * %&% ((Hole.add_row_hole "acl" %=% mkVInt(1,1)) %=>% ((Hole.add_row_hole "fwd" %=% mkVInt(0,1))
                     *                                                      %&% (Hole.add_row_hole "acl" %=% mkVInt(0,1)))) *)
                    %&% acc %&% restr
                  (* else
                   *   acc *)
              )
          in
          (* if params.debug then Printf.printf "active domain restr \n %s\n%!" (string_of_test active_domain_restrict); *)
          let no_defaults = if opts.no_defaults
                            then (List.filter (holes_of_cmd phys)
                                   ~f:(fun (v,_) ->
                                     List.for_all fvs ~f:(fun (v',_) ->
                                         if List.exists [v'; Hole.add_row_prefix; Hole.delete_row_prefix; Hole.which_act_prefix]
                                              ~f:(fun substring -> String.is_substring v ~substring)
                                         then (if params.debug then Printf.printf "%s \\in %s, so skipped\n%!" v v'; false)
                                         else (if params.debug then Printf.printf "%s \\not\\in %s, so kept\n%!" v v'; true)
                                       )
                                   )
                                  |> List.fold ~init:True ~f:(fun acc (v,sz) ->
                                         acc %&% (Hole(v,sz) %<>% mkVInt(0,sz))
                                 ))
                            else True
          in
          if params.debug then Printf.printf "NoDefaults Restriction (%s) \n %s\n%!" (if opts.no_defaults then "on" else "off") (string_of_test no_defaults);
          let out_test =
            (if opts.annot then
               ((Hole("?AddRowTonexthop",1) %=% Hole("?AddRowToipv6_fib",1))
                 %&% (Hole("?AddRowToipv6_fib",1) %=% Hole("?AddRowToprocess_lag",1))
                 %&% (Hole("?AddRowTonexthop",1) %=% Hole("?AddRowToprocess_lag",1))
                 %&% (Hole("?ingress_metadata__egress_ifindex",16) %<>% mkVInt(0,16)))
                %&%
               (((Hole("?AddRowTonexthop",1) %=% mkVInt(1,1))
                %=>% ((Hole("?AddRowToipv6_fib",1) %=% mkVInt(1,1))
                      %&% (Hole("?ActInipv6_fib",1) %=% mkVInt(0,1))
                      %&% (Hole("n", 32) %=% Hole("?l3_metadata__nexthop_index",32))
                      %&% (Hole("n", 32) %<>% mkVInt(0,32))
               )))
               %&% (((Hole("?AddRowToipv6_fib",1) %=% mkVInt(1,1))
                     %=>% ((Hole("?AddRowToprocess_lag",1) %=% mkVInt(1,1))
                           %&% (Hole("?ActInprocess_lag",1) %=% mkVInt(0,1))
                           %&% (Hole("f", 16) %=% Hole("?ingress_metadata__egress_ifindex",16))
                           %&% (Hole("?ingress_metadata__egress_ifindex_mask",16) %=% mkVInt(65535,16))
                           %&% (Hole("f", 16) %<>% mkVInt(0,16))
                    )))
               %&% (if List.exists (Problem.phys_edits problem)
                        ~f:(fun e -> Tables.Edit.table e = "l3_rewrite")
                   then
                       Hole("?AddRowTol3_rewrite",1) %=% mkVInt(0,1)
                    else True )
               %&% (if List.exists (Problem.phys_edits problem)
                        ~f:(fun e -> Tables.Edit.table e = "ipv6_fib")
                   then
                       Hole("?AddRowToipv6_fib",1) %=% mkVInt(0,1)
                    else True)
               (* %&% (Hole("?AddRowTosmac_rewrite",1) %=% mkVInt(0,1)) *)
             else True)
            %&%
              no_defaults
            %&%
              active_domain_restrict
            %&%
              query_test in
          (* Printf.printf "outtest_computed\n%!"; *)
          (* let () = if params.debug then Printf.printf "test is \n   %s\n\n%!" (string_of_test out_test) in *)
          Some (out_test, hints)) in
  tests

let holes_for_table table phys =
  match get_schema_of_table table phys with
  | None -> failwith @@ Printf.sprintf "couldn't find schema for %s\n%!" table
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
  | None -> failwith @@ Printf.sprintf"couldnt find schema for %s\n%!" table
  | Some (_, acts, _) ->
     List.foldi acts ~init:[]
       ~f:(fun i acc (params,_) ->
         acc @ if i = Bigint.to_int_exn actId then [] else List.map params ~f:fst
       )

let minimize_model (model : value StringMap.t) (_ (*phys*) : cmd) : value StringMap.t = model
  (* StringMap.keys model
   * |> List.filter ~f:(fun k -> String.is_prefix k ~prefix:Hole.add_row_prefix)
   * |> List.fold ~init:model
   *      ~f:(fun model key ->
   *        match StringMap.find model key with
   *        | Some Int(v,_) when v = Bigint.zero ->
   *           let table = String.chop_prefix_exn key ~prefix:Hole.add_row_prefix in
   *           holes_for_table table phys
   *           |> List.fold ~init:model ~f:(StringMap.remove)
   *        | Some Int(v,_) when v = Bigint.one ->
   *           let table = String.chop_prefix_exn key ~prefix:Hole.add_row_prefix in
   *           begin
   *             match (StringMap.find model (Hole.which_act_hole_name table)) with
   *             | Some (Int (actId, _)) ->
   *                holes_for_other_actions table phys actId
   *                |> List.fold ~init:model ~f:(StringMap.remove)
   *             | None -> model
   *           end
   *        | _ -> model
   *      ) *)


let rec search (params : Parameters.t) data problem t : ((value StringMap.t * t) option)=
  match params.timeout with
  | Some (st,dur) when Time.(Span.(diff (now()) st > dur)) -> None
  | _ ->
  match t.search_space, t.schedule with
  | [], [] ->
     if params.debug then Printf.printf "Search failed\n%!";
     None
  | [], (opts::schedule) ->
     let () =
       if params.debug then
         Printf.printf "\nusing optimization |%s|\n\n%!"
           (string_of_opts opts)
     in
     let search_space = apply_opts params data problem opts in
     (* Printf.printf "Searching with %d opts and %d paths\n%!" (List.length schedule) (List.length search_space); *)
     search params data problem {schedule; search_space}
  | (test,hints)::search_space, schedule ->
     (* Printf.printf "Check sat\n%!"; *)
     (* if params.debug then Printf.printf "MODELSPACE:\n%s\nTEST\n%s\n%!" (Problem.model_space problem |> string_of_test) (test |> string_of_test); *)
     let model_opt, dur = check_sat params (Problem.model_space problem %&% test) in
     ProfData.incr !data.model_z3_calls;
     ProfData.update_time_val !data.model_z3_time dur;
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
