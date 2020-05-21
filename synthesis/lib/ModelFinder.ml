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
  }

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
    (* if injection || hints || paths || only_holes then
     *   [{opt with injection=false;hints=false;paths=false;only_holes=false}]
     * else *)
      []

let make_searcher (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) : t =
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


(* let perturb p x v sz =
 *   if Bigint.(v <> zero && v + one = pow (of_int 2) (of_int sz)) then
 *     StringMap.set p ~key:x ~data:(Int(Bigint.(v - one), sz))
 *   else
 *     StringMap.set p ~key:x ~data:(Int(Bigint.(v + one),sz))
 *
 * let refine_counter params problem ((inp,outp) : Packet.t * Packet.t) =
 *   let l = Problem.log_gcl_program params problem in
 *   let p = Problem.phys_gcl_program params problem in
 *   Problem.fvs problem
 *   |> List.dedup_and_sort ~compare:(fun (x,_) (y,_) -> String.compare x y)
 *   |> List.fold ~init:(inp,outp)
 *        ~f:(fun (inp_acc, outp_acc) (x,sz) ->
 *          match StringMap.find inp x, StringMap.find outp x with
 *          | None, None -> (inp, outp)
 *          | Some Int(v_in, sz), Some (Int(v_out, sz')) ->
 *             let inp' = perturb inp x v_in sz in
 *             let loutp'= Semantics.eval_act l inp' in
 *             if Bigint.(v_in = v_out) then
 *               let poutp' = Semantics.eval_act p inp' in
 *               Printf.printf "%s unchanged %s |-> %s \n" x (Bigint.Hex.to_string v_in) (Bigint.Hex.to_string v_out);
 *               if StringMap.find_exn loutp' x = StringMap.find_exn inp' x then
 *                 let () = Printf.printf "\t up to perturbation\n%!" in
 *                 (StringMap.remove inp_acc x, StringMap.remove outp_acc x)
 *               else
 *                 let () = Printf.printf "\t perturbation changed it %s |-> %s \n%!"
 *                            (string_of_value @@ StringMap.find_exn loutp' x)
 *                            (string_of_value @@ StringMap.find_exn inp' x) in
 *                 (inp_acc,outp_acc)
 *             else
 *               (inp_acc,outp_acc)
 *          | _, _ -> (inp_acc, outp_acc)
 *        ) *)



let apply_opts (params : Parameters.t) (data : ProfData.t ref) (problem : Problem.t) (opts : opts)  =
  let (in_pkt, out_pkt) = Problem.cexs problem |> List.hd_exn in
                          (* |> refine_counter params problem in *)
  (* let () = Printf.printf "in : %s \n out: %s\n%!" (string_of_map in_pkt) (string_of_map out_pkt) in *)
  let st = Time.now () in
  let deletions = [] in (*compute_deletions in_pkt problem in*)
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

                  (* if List.exists (Problem.fvs problem) ~f:(fun (v,_) ->  String.is_substring h ~substring:v || not (String.is_substring h ~substring:"?"))
                   * then *)
                  (* let is_mask_val =
                   *   List.exists holes
                   *     ~f:(fun (hm,_) ->
                   *       match String.chop_suffix hm ~suffix:"_mask" with
                   *       | Some h' -> h = h'
                   *       | None -> false
                   *     )
                   * in *)
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
            active_domain_restrict %&% query_test in
          (* Printf.printf "outtest_computed\n%!"; *)
          let () = if params.debug then Printf.printf "test is \n   %s\n\n%!" (string_of_test out_test) in
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
