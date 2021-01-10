open Core
open Ast
open Manip
open VCGen
open Util
open Prover

type opts =
  {injection : bool;
   hints : bool;
   hint_type : [`Vals | `NoVals];
   paths : bool;
   only_holes: bool;
   mask : bool;
   restrict_mask : bool;
   nlp : bool;
   annot : bool;
   single : bool;
   domain : bool;
   no_defaults : bool;
   no_deletes : bool;
   double : bool;
   reachable_adds : bool
  }

type t = {
    schedule : opts list;
    search_space : (test * Model.t) list;
  }

let condcat b app s =
  if b
  then Printf.sprintf "%s %s" s app
  else s

let string_of_opts (opts) : string =
  condcat opts.injection "injection" " "
  |> condcat opts.hints "hints"
  |> condcat opts.paths "paths"
  |> condcat opts.only_holes "only_holes"
  |> condcat opts.mask "mask"
  |> condcat opts.restrict_mask "restrict_mask"
  |> condcat opts.nlp "NLP"
  |> condcat opts.annot "annotations"
  |> condcat opts.single "single"
  |> condcat opts.domain "domain"
  |> condcat opts.no_defaults "no_defaults"
  |> condcat opts.double "double"
  |> condcat opts.reachable_adds "reachable_adds "
  |> condcat opts.no_deletes "no_deletes"

let no_opts =
  {injection = false;
   hints = false;
   hint_type = `Vals;
   paths = false;
   only_holes = false;
   mask = false;
   restrict_mask = false;
   nlp = false;
   annot = false;
   single = false;
   domain = false;
   no_defaults = false;
   no_deletes = false;
   double = false;
   reachable_adds = false;
  }


(* None > Mask > Paths > Injection > Hints > Only_Holes *)
let rec make_schedule opt =
  opt ::
    if opt.double then
      let opt' = {opt with double = false} in
      make_schedule opt'
    else if opt.injection || opt.hints || opt.paths || opt.only_holes || opt.nlp || opt.domain then
      let opt' = {opt with injection=false;hints=false;paths=false;only_holes=false; nlp=false;domain = false} in
      make_schedule opt'
    else if opt.no_defaults then
      let opt' = {opt with no_defaults = false} in
      make_schedule opt'
    else []

let make_searcher (params : Parameters.t) (_ : ProfData.t ref) (_ : Problem.t) : t =
  let schedule = make_schedule {
                     injection = params.injection;
                     hints = params.hints;
                     hint_type = if params.hint_type = "mask" then
                                   `NoVals
                                 else if params.hint_type = "exact"
                                 then
                                   `Vals
                                 else
                                   Printf.sprintf "Unrecognized hint type %s, expected \"mask\" or \"exact\"" (params.hint_type)
                                   |> failwith;

                     paths = params.monotonic;
                     only_holes = params.only_holes;
                     mask = params.widening;
                     restrict_mask = params.restrict_mask;
                     annot = params.allow_annotations;
                     nlp = params.nlp;
                     single = params.unique_edits;
                     domain = params.domain;
                     no_defaults = params.no_defaults;
                     no_deletes = params.no_deletes;
                     double = params.use_all_cexs;
                     reachable_adds = params.reach_restrict;
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

let compute_deletions opts (problem : Problem.t) =
  if opts.no_deletes then
    []
  else
    let phys_inst = Problem.phys_inst problem in
    StringMap.fold phys_inst ~init:[]
      ~f:(fun ~key:table_name ~data:rows dels ->
        dels @ List.filter_mapi rows ~f:(fun i _ ->
                   match reindex_for_dels problem table_name i with
                   | None -> None
                   | Some i' -> Some (table_name, i')))

let well_formed_adds (params : Parameters.t) (problem : Problem.t) encode_tag =
  let phys_inst = Problem.phys_edited_instance params problem in
  let phys = Problem.phys problem in
  tables_of_cmd phys
  |> concatMap ~c:(%&%) ~init:(Some True)
       ~f:(fun t ->
         let t_rows = Instance.get_rows phys_inst t in
         (Hole.add_row_hole t %=% mkVInt(1,1)) %=>%
           concatMap t_rows ~init:(Some True) ~c:(%&%)
             ~f:(fun (ms,_,_) ->
               !%(concatMap ms ~init:(Some False) ~c:(%&%)
                    ~f:(Match.to_valuation_test t encode_tag)
                 )
             )
       )


let adds_are_reachable params (problem : Problem.t) (opts : opts) fvs encode_tag =
  if not opts.reachable_adds then True else
    let phys = Problem.phys problem in
    let in_pkt = Problem.cexs problem |> List.hd_exn |> fst in
    get_tables_keys phys
    |> List.fold
      ~init:True
      ~f:(fun acc (tbl_name,keys) ->
        mkAnd acc @@
          mkImplies (Hole.add_row_hole tbl_name %=% mkVInt(1,1)) @@
            FastCX.is_reachable encode_tag params problem fvs in_pkt tbl_name keys
      )

let non_empty_adds (problem : Problem.t) =
  Problem.phys problem
  |> tables_of_cmd
  |> List.fold ~init:None
       ~f:(fun acc tbl ->
         match acc with
         | None -> Some (Hole.add_row_hole tbl %=% mkVInt(1,1))
         | Some acc -> Some (acc %+% (Hole.add_row_hole tbl %=% mkVInt(1,1)))
       )
  |> Option.value ~default:True

let single problem (opts : opts) query_holes =
  if not opts.single then True else
    List.fold query_holes ~init:True
      ~f:(fun acc (h,sz) ->
        mkAnd acc
          (if Hole.is_add_row_hole h
              && List.exists (Problem.phys_edits problem)
                   ~f:(fun e ->
                     Edit.table e = String.chop_prefix_exn h ~prefix:Hole.add_row_prefix
                   )
           then
             (Hole(h,sz) %=% mkVInt(0,sz))
           else
             acc))

let restrict_mask (opts : opts) query_holes =
  if not opts.restrict_mask then True else
    List.fold query_holes ~init:True
      ~f:(fun acc (h,sz) ->
        mkAnd acc @@
          if String.is_suffix h ~suffix:"_mask"
          then
            let h_value = String.chop_suffix_exn h ~suffix:"_mask" in
            let all_1s = max_int sz in
            mkAnd acc @@
              bigor [
                  Hole(h, sz) %=% Value(Value.big_make (all_1s, sz));
                  bigand [
                      (Hole(h_value,sz) %=% mkVInt(0,sz));
                      (Hole(h,sz) %=% mkVInt(0,sz));
                    ]
                ]
          else True)

let active_domain_restrict params problem opts query_holes : test =
  if not opts.domain then True else
    let ints = (multi_ints_of_cmd (Problem.log_gcl_program params problem))
               @ (multi_ints_of_cmd (Problem.phys_gcl_program params problem))
               |> List.dedup_and_sort ~compare:(Stdlib.compare)
               |> List.filter ~f:(fun v -> Bigint.(Value.get_bigint v <> zero && Value.get_bigint v <> one)) in
    let test = List.fold query_holes ~init:True
                 ~f:(fun acc (h,sz) ->
                   let restr =
                     List.fold ints
                       ~init:(False)
                       ~f:(fun acci v ->
                         let szi = Value.size v in
                         let i = Value.get_bigint v in
                         mkOr acci @@
                           if sz = szi
                              && not (String.is_suffix h ~suffix:"_mask")
                              && not (Hole.is_add_row_hole h)
                              && not (Hole.is_delete_hole h)
                              && not (Hole.is_which_act_hole h)
                           then (Hole(h,sz) %=% Value(Value.big_make (i,szi))
                                 %+% (Hole(h,sz) %=% mkVInt(0,szi))
                                 %+% (Hole(h,sz) %=% mkVInt(1,szi)))
                           else False)
                   in
                   if restr = False then acc else (acc %&% restr)
                 )
    in
    (* let () = Printf.printf "\n\n\n\nactive domain restr \n %s\n\n\n\n%!" (string_of_test test) in *)
    test

let no_defaults (params : Parameters.t) opts fvs phys =
  if not opts.no_defaults then True else
    List.filter (holes_of_cmd phys)
      ~f:(fun (v,_) ->
        List.for_all fvs ~f:(fun (v',_) ->
                if List.exists [v'; Hole.add_row_prefix; Hole.delete_row_prefix; Hole.which_act_prefix]
                     ~f:(fun substring -> String.is_substring v ~substring)
                then (if params.debug then Printf.printf "%s matches %s, so skipped\n%!" v v'; false)
                else (if params.debug then Printf.printf "%s misses  %s, so kept\n%!" v v'; true)
          )
      )
    |> List.fold ~init:True ~f:(fun acc (v,sz) ->
           acc %&% (Hole(v,sz) %<>% mkVInt(0,sz)))


let rec construct_model_query opts form fvs cexs in_pkt phys out_pkt =
  match form with
  | `Passive ->
     let sub, passive_phys = passify fvs ((*Packet.to_assignment in_pkt %:%*) phys) in
     let phys' = CompilerOpts.optimize fvs passive_phys in
     (* let () = Printf.printf "Optimized passive program\n%s\n\n%!" (string_of_cmd phys'); in *)
     let good = good_wp phys'
                |> Log.print_and_return_test ~pre:"normal executions:\n" ~post:"\n---------------------\n" false
     in
     let testify = Packet.to_test ~fvs in
     let ensure_cexs = if opts.double then
                         let in_test = concatMap (fsts cexs) ~f:(testify) ~c:(%+%) in
                         let out_test = concatMap (snds cexs) ~f:(testify) ~c:(%+%) in
                         hoare_triple_passified sub in_test good out_test
                       else
                         True
     in
     ensure_cexs %&%
       hoare_triple_passified sub (testify in_pkt) good (testify out_pkt)
     |> Log.print_and_return_test ~pre:"Passive optimized query:\n" ~post:"\n---------------------\n" false


  | `PassiveAggressive ->
     let sub, passive_phys = passify fvs (Packet.to_assignment in_pkt %:% phys |> CompilerOpts.optimize fvs) in
     let phys' = CompilerOpts.optimize fvs passive_phys in
     (* let () = Printf.printf "optimized physical program \n%s\n---\n%!" (string_of_cmd phys') in *)
     let phys' = CompilerOpts.passive_optimize (apply_finals_sub_packet out_pkt sub) phys' in
     (* let () = Printf.printf "passive optimized physical program \n%s\n---\n%!" (string_of_cmd phys') in *)
     let out_test =
       good_wp phys'
       |> Log.print_and_return_test true ~pre:"PassiveAggressive formula \n" ~post:"\n------\n"
     in
     if List.is_empty @@ free_of_test `Var out_test
     then out_test
     else begin
         (* let () = Printf.printf "Manual QE failed, letting Z3 do it" in *)
         construct_model_query opts `Passive fvs cexs in_pkt phys out_pkt
       end
     (* Log.check_qe true out_test;
      * out_test *)

  | `WP ->
     let phys' = Packet.to_assignment in_pkt %:% phys in
     let opt_phys = CompilerOpts.optimize fvs phys' in
     let out_pkt_form = Packet.to_test ~fvs out_pkt in
     wp `Negs opt_phys out_pkt_form



let compute_vc (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t)  (opts : opts)  =
  let (in_pkt, out_pkt) = Problem.cexs problem |> List.hd_exn in
  let st = Time.now () in
  let deletions = compute_deletions opts problem in
  let hints = if opts.hints then
                let open Problem in
                (* Log.print_edits (log_edits problem); *)
                Hint.construct (phys problem) (log_edits problem |> List.hd_exn)
              else [] in
  let hole_protocol = if opts.only_holes
                      then Instance.OnlyHoles hints
                      else Instance.WithHoles (deletions, hints) in
  let hole_type =  if opts.mask then `Mask else `Exact in
  let phys = Problem.phys_gcl_holes {params with no_defaults = opts.no_defaults} problem hole_protocol hole_type in
  ProfData.update_time !data.model_holes_time st;
  let st = Time.now () in
  let fvs = List.(free_vars_of_cmd phys
                  |> filter ~f:(fun x -> exists (Problem.fvs problem) ~f:(Stdlib.(=) x))) in
  (* let fvs = problem.fvs in *)
  if params.debug then Printf.printf "Constructing Wps \n%!";
  let wp_list =
    if opts.paths then
      let phys = Packet.to_assignment in_pkt %:% phys |> CompilerOpts.optimize fvs in
      wp_paths `NoNegs phys (Packet.to_test ~fvs out_pkt)
    else
      (* let (sub, _(\*passive_phys*\), good_N, _) = good_execs fvs phys in *)
      let test =
        construct_model_query opts `Passive fvs (Problem.cexs problem) in_pkt phys out_pkt
      in
      Log.check_qe false test;
      (* Printf.printf "--------------%s---------------\n%!" (string_of_test test); *)
      [phys, test];
  in
  (* Printf.printf "Constructed WPS\n%!"; *)
  ProfData.update_time !data.search_wp_time st;
  (wp_list, phys, hints)


let with_opts (params : Parameters.t) (problem : Problem.t) (opts : opts) (wp_list,phys,hints) : (test * Model.t) list  =
  let hole_type = if opts.mask then `Mask else `Exact in
  let fvs = Problem.fvs problem  in
  let tests =
    List.filter_map wp_list
      ~f:(fun (cmd, spec) ->
        if spec = False || not (has_hole_test spec) then None else

          let () = if params.debug then
                     Printf.printf "Checking path with hole!\n  %s\n\n%!" (string_of_cmd cmd) in

          let () = if false then
                     Printf.printf"TEST:\n%s\n--------\n\n%!" (string_of_test spec) in

          let wf_holes = List.fold (Problem.phys problem |> get_tables_actsizes) ~init:True
                           ~f:(fun acc (tbl,num_acts) ->
                             acc %&%
                               (Hole(Hole.which_act_hole_name tbl,max (log2 num_acts) 1)
                                %<=% mkVInt(num_acts-1,max (log2 num_acts) 1)))
                         |> Log.print_and_return_test params.debug ~pre:"WF holes:\n" ~post:"\n--------\n\n"
          in

          let pre_condition =
            (Problem.model_space problem) %&% wf_holes %&% spec
            |> Injection.optimization {params with injection = opts.injection} problem
          in
          let query_test = wf_holes %&% pre_condition in
          let query_holes = holes_of_test query_test |> List.dedup_and_sort ~compare:(Stdlib.compare) in
          if params.debug then
            Printf.printf "There are %d hints in ModelFinder\n%!" (List.length hints);
          Log.print_hints params.debug hints;
          let partial_model = Hint.list_to_model opts.hint_type (Problem.phys problem) hints in
          Log.print_hints_map params.debug partial_model;
          Interactive.pause params.interactive;

          let out_test =
            bigand [
                query_test
                |> fixup_test partial_model
                |> Log.print_and_return_test params.debug ~pre:"The Query:\n" ~post:"\n--------\n\n";

                adds_are_reachable params problem opts fvs hole_type
                |> fixup_test partial_model
                |> Log.print_and_return_test params.debug ~pre:"Adds_are_reachable:\n" ~post:"\n--------\n\n ";

                restrict_mask opts query_holes
                |> fixup_test partial_model
                |> Log.print_and_return_test params.debug ~pre:"Restricting Masks:\n" ~post:"\n--------\n\n";

                no_defaults params opts fvs phys
                |> fixup_test partial_model
                |> Log.print_and_return_test params.debug ~pre:"No Defaults:\n" ~post:"\n--------\n\n";

                single problem opts query_holes
                |> fixup_test partial_model
                |> Log.print_and_return_test params.debug ~pre:"Single:\n" ~post:"\n--------\n\n";

                active_domain_restrict params problem opts query_holes
                |> fixup_test partial_model
                |> Log.print_and_return_test params.debug ~pre:"Active Domain Restriction:\n" ~post:"\n--------\n\n";

                (* well_formed_adds params problem hole_type
                 * |> fixup_test partial_model
                 * |> Log.print_and_return_test params.debug  ~pre:"Well-Formed Additions:\n" ~post:"\n--------\n\n"; *)

                non_empty_adds problem
                |> fixup_test partial_model
                |> Log.print_and_return_test params.debug ~pre:"Non-Empty Additions:\n" ~post:"\n--------\n\n"
              ]
          in
          Some (out_test, partial_model))
  in
  tests


let compute_queries (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) (opts : opts) : (test * Model.t) list =
  let st = Time.now() in
  let queries = compute_vc params data problem opts
                |> with_opts params problem opts in
  ProfData.update_time !data.search_wp_time st;
  queries

let holes_for_table table phys =
  match get_schema_of_table table phys with
  | None -> failwith @@ Printf.sprintf "couldn't find schema for %s\n%!" table
  | Some (ks, acts, _) ->
     List.bind ks ~f:(fun (k,_,v_opt) ->
         match v_opt with
         | Some _ -> []
         | None ->
            let lo,hi = Hole.match_holes_range table k in
            let v,m = Hole.match_holes_mask table k in
            [Hole.match_hole_exact table k;lo;hi;v;m]
            |> List.dedup_and_sort ~compare:String.compare
       )
     @ List.(acts >>= fun (_, params,_) ->
             params >>| fst )

let holes_for_other_actions table phys actId =
  match get_schema_of_table table phys with
  | None -> failwith @@ Printf.sprintf "couldnt find schema for %s\n%!" table
  | Some (_, acts, _) ->
     List.foldi acts ~init:[]
       ~f:(fun i acc (_, params,_) ->
         acc @ if i = Bigint.to_int_exn actId then [] else List.map params ~f:fst
       )


let rec search (params : Parameters.t) data problem t : ((Model.t * t) option) =
  if Timeout.timed_out params.timeout then
    None
  else
    match t.search_space, t.schedule with
    | [], [] ->
       if params.debug then Printf.printf "Search failed\n%!";
       None
    | [], (opts::schedule) ->
       let () =
         if params.debug then
           Printf.printf "trying heuristics |%s|\n\n%!"
             (string_of_opts opts)
       in
       let search_space = compute_queries params data problem opts in
       (* Printf.printf "search space rebuild, recursing!\n%!"; *)
       search params data problem {schedule; search_space}
    | (test, partial_model) :: search_space, schedule ->
       Log.check_attempts params.debug problem;
       Log.print_hints_map params.debug partial_model;
       if params.debug then Printf.printf "Sending query to Z3\n%!";
       let model_opt, dur =
         check_sat params @@
           fixup_test partial_model @@
             bigand [
                 Problem.model_space problem;
                 test
               ] in
       ProfData.incr !data.model_z3_calls;
       ProfData.update_time_val !data.model_z3_time dur;
       if params.debug then Printf.printf "Sat Checked\n%!\n";
       match model_opt with
       | Some raw_model ->
          if params.debug then Printf.printf "Found a model, done \n%!";
          if Problem.seen_attempt problem raw_model then begin
              Printf.printf "%s\n%!" (Problem.attempts_to_string problem);
              Printf.printf "\ncurrent model is %s\n%!" (Model.to_string raw_model);
              Printf.printf "\nmodel_space is %s \n%!" (string_of_test @@ Problem.model_space problem);
              Printf.printf "\nmodel has already been seen and is allowed? %s"
                (if Problem.model_space problem |> fixup_test raw_model = True
                 then "yes! thats a contradiction\n%!"
                 else "no, but somehow we synthesized it... a contradiction\n%!"
                );
              failwith ""
            end
          else begin
              Printf.sprintf "IsNOVEL??? \n    %s \n"
                (Problem.model_space problem
                 |> fixup_test raw_model
                 |> string_of_test)
              |> Log.log params.debug;
              Some (Model.join partial_model raw_model, t)
            end
       | _ ->
          (* Printf.printf "No model, keep searching with %d opts and %d paths \n%!" (List.length schedule) (List.length search_space); *)
          search params data problem {schedule; search_space}
