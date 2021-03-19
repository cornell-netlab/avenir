open Core
open Async
module Value = Avenir.Value
module Cmd = Avenir.Cmd
module Parser = Avenir.Parser
module Lexer = Avenir.Lexer
module Prover = Avenir.Prover
module Synthesis = Avenir.Synthesis
module Encode = Avenir.Encode
module Manip = Avenir.Manip
module Benchmark = Avenir.Benchmark
module Classbenching = Avenir.Classbenching
module OneBigTable = Avenir.OneBigTable
module Parameters = Avenir.Parameters
module ProfData = Avenir.ProfData
module Problem = Avenir.Problem
module Edit = Avenir.Edit
module Instance = Avenir.Instance
module Runtime = Avenir.Runtime
module Server = Avenir.Server

let opt_params : Parameters.t Command.Param.t =
  let open Command.Let_syntax in
  [%map_open
    let widening = flag "-w" no_arg ~doc:"Do widening"
    and do_slice = flag "-s" no_arg ~doc:"Do slicing optimization"
    and semantic_slicing =
      flag "-S" no_arg ~doc:"Do semantic slicing, -s required"
    and edits_depth =
      flag "-e" (required int) ~doc:"E maximum number of physical edits"
    and search_width =
      flag "-b" (required int) ~doc:"B maximum number of attempts per CE"
    and monotonic =
      flag "-m" no_arg ~doc:"[DEPRECATED] Prune rows with no holes"
    and injection = flag "--inj" no_arg ~doc:"Try injection optimization"
    and fastcx =
      flag "--fastcx" no_arg ~doc:"Generate counterexample quickly"
    and vcache = flag "--cache-queries" no_arg ~doc:"Enable query caching"
    and ecache =
      flag "--cache-edits" (optional int)
        ~doc:
          "N Enable edit caching when there are at least N previous \
           successes"
    and aggro_freshen =
      flag "--aggro-freshen" no_arg
        ~doc:
          "N Enable edit caching when there are at least N previous \
           successes"
    and shortening = flag "--shortening" no_arg ~doc:"shorten queries"
    and above =
      flag "--above" no_arg
        ~doc:"synthesize new edits above existing instance, not below"
    and minimize =
      flag "--min" no_arg ~doc:"try and eliminate each edit in turn"
    and hints = flag "--hints" (optional string) ~doc:"Use syntactic hints"
    and only_holes = flag "--holes" no_arg ~doc:"Holes only"
    and allow_annotations = flag "--annot" no_arg ~doc:"Use hard-coded edits"
    and nlp =
      flag "--nlp" no_arg
        ~doc:"[DEPRECATED] variable name based domain restrictions"
    and unique_edits =
      flag "--unique-edits" no_arg ~doc:"Only one edit allowed per table"
    and domain =
      flag "--domain-restrict" no_arg
        ~doc:"Restrict allowed values to those that occur in the programs"
    and restrict_mask = flag "--restrict-masks" no_arg ~doc:"Restrict masks"
    and restr_acts =
      flag "--restrict-acts" no_arg ~doc:"Restrict unlikely actions"
    and no_defaults =
      flag "--no-defaults" no_arg
        ~doc:"Prefer solutions that don't rely on default actions"
    and no_deletes = flag "--no-deletes" no_arg ~doc:"try with no deletions"
    and use_all_cexs =
      flag "--use-all-cexs" no_arg
        ~doc:"attempt to use information from all cexs to compute models"
    and reach_restrict =
      flag "--reach-restrict" no_arg
        ~doc:
          "ensure all rules generated from model are reachable by the \
           counterexample"
    and reach_filter =
      flag "--reach-filter" no_arg
        ~doc:"remove unreachable rules generated by model"
    and hot_start =
      flag "--hot-start" no_arg
        ~doc:
          "process all the insertions to populate the caches, then rerun, \
           and measure the second run"
    and solve_strat =
      flag "--using" (listed string)
        ~doc:
          "STRAT The Z3 strategy to use -- listed names are combined using \
           Z3's or-else combinator"
    in
    if Option.is_some ecache then Avenir.EAbstr.make () ;
    Parameters.
      { default with
        widening
      ; do_slice
      ; semantic_slicing
      ; edits_depth
      ; search_width
      ; monotonic
      ; injection
      ; fastcx
      ; vcache
      ; ecache
      ; aggro_freshen
      ; shortening
      ; above
      ; minimize
      ; hints= Option.is_some hints
      ; hint_type= Option.value hints ~default:""
      ; only_holes
      ; allow_annotations
      ; nlp
      ; unique_edits
      ; domain
      ; restrict_mask
      ; restr_acts
      ; no_defaults
      ; no_deletes
      ; use_all_cexs
      ; reach_restrict
      ; reach_filter
      ; hot_start
      ; solve_strat }]

let mng_params =
  let open Command.Let_syntax in
  [%map_open
    let verbosity = flag "-v" (listed string) ~doc:"Set Verbosity Level"
    and thrift_mode =
      flag "--thrift" no_arg ~doc:"parse & write bmv2/thrift commands"
    and interactive = flag "-i" no_arg ~doc:"interactive mode"
    and timeout =
      flag "--timeout" (optional float) ~doc:"Optional timeout in seconds"
    and prover_loc =
      flag "--loc" (optional string)
        ~doc:"<fp> Path to SMTLIB location, defaults to /usr/bin/z3"
    and read_ecache =
        flag "--read-ecache" (optional string) ~doc:"Initializes edit cache from a specified file instead of initializing with an empty cache"
    and write_ecache =
        flag "--write-ecache" (optional string) ~doc:"Writes out edit cache into a specified file after completion"
    in
    Option.value prover_loc ~default:"/usr/bin/z3" |> Prover.make_provers ;
    Avenir.Log.set_level verbosity ;
    Parameters.
      { default with
        thrift_mode
      ; interactive
      ; timeout= Avenir.Timeout.start timeout
      ; read_ecache
      ; write_ecache }]

let problem_flags =
  let open Command.Let_syntax in
  [%map_open
    let logical = anon ("logical" %: string)
    and physical = anon ("physical" %: string)
    and logical_edits = anon ("logical_edits" %: string)
    and physical_edits = anon ("physical_edits" %: string)
    and fvs = anon ("fvs" %: string)
    and p4 = flag "-P4" no_arg ~doc:"input full P4 programs"
    and log_incl =
      flag "-I1" (listed string)
        ~doc:"<dir> add directory to include search path for logical file"
    and phys_incl =
      flag "-I2" (listed string)
        ~doc:"<dir> add directory to include search path for physical file"
    and data =
      flag "-data" (optional string) ~doc:"The logical experiment to run"
    and onos =
      flag "-onos" no_arg ~doc:"Parse logical edits as onos insertions"
    in
    fun (params : Parameters.t) ->
      let var_mapping = Benchmark.parse_fvs fvs in
      let fvs = List.map var_mapping ~f:snd in
      let log =
        if p4 then
          Encode.encode_from_p4 log_incl logical false
          |> Encode.unify_names var_mapping
          |> Benchmark.zero_init fvs |> Benchmark.drop_handle fvs
        else Benchmark.parse_file logical
      in
      let phys =
        if p4 then
          Encode.encode_from_p4 phys_incl physical false
          |> Encode.unify_names var_mapping
          |> Benchmark.zero_init fvs |> Benchmark.drop_handle fvs
        else Benchmark.parse_file physical
      in
      let parse_runtime =
        if params.thrift_mode then Runtime.parse_bmv2 else Runtime.parse
      in
      let log_inst =
        parse_runtime log logical_edits
        |> Instance.(update_list params empty)
      in
      let phys_inst =
        parse_runtime phys physical_edits
        |> Instance.(update_list params empty)
      in
      let phys_drop_spec = None in
      let data_to_synthesize =
        Option.map data ~f:(fun d ->
            if onos then
              Benchmark.(
                onos_to_edits var_mapping d "ipv6" "hdr.ipv6.dst_addr")
            else parse_runtime log d |> List.(map ~f:return) )
      in
      ( params
      , Problem.make ~log ~phys ~log_inst ~phys_inst ~log_edits:[] ~fvs
          ~phys_drop_spec
      , data_to_synthesize )]

let synthesize =
  let open Command.Let_syntax in
  Command.basic ~summary:"Synthesize"
    [%map_open
      let params, mk_prob, data_opt =
        problem_flags <*> map2 opt_params mng_params ~f:Parameters.union
      and measure =
        flag "-measure" no_arg ~doc:"Produce a CSV of data to stdout"
      and print_res = flag "-p" no_arg ~doc:"Print synthesized program" in
      fun () ->
        let data =
          Option.value_exn data_opt
            ~message:"Data must be passed in for synthesis"
        in
        let prob = mk_prob () in
        let edit_to_string =
          if params.thrift_mode then Edit.to_bmv2_string (Problem.phys prob)
          else Edit.to_string
        in
        if Option.is_some params.read_ecache then Avenir.EAbstr.make ~filename:params.read_ecache ();
        if measure then
          match Benchmark.measure params None mk_prob data with
          | None -> Core.Printf.printf "No solution could be found \n%!"
          | Some soln when print_res ->
              if Option.is_some params.write_ecache then
                Avenir.EAbstr.dump_yojson (Option.value_exn params.write_ecache);
              Core.Printf.printf "EDITS:\n%!" ;
              List.iter soln ~f:(fun e ->
                  Core.Printf.printf "%s\n%!" (edit_to_string e))
          | _ -> if Option.is_some params.write_ecache then
            Avenir.EAbstr.dump_yojson (Option.value_exn params.write_ecache); ()
        else
          let data = List.join data in
          let mk_prob =
            let open Problem in
            make ~log:(log prob) ~phys:(phys prob) ~log_inst:(log_inst prob)
              ~phys_inst:(phys_inst prob) ~log_edits:data ~fvs:(fvs prob)
              ~phys_drop_spec:(phys_drop_spec prob)
          in
          let prob = mk_prob () in
          Avenir.Log.debug
          @@ lazy
               (Core.Printf.sprintf "PROBLEM: %s \n"
                  (Problem.to_string params prob) ) ;
          match
            Synthesis.cegis_math_sequence params (ProfData.zero ()) mk_prob
          with
          | None -> failwith "failed"
          | Some (_, phys_edits) ->
              if Option.is_some params.write_ecache then Avenir.EAbstr.dump_yojson (Option.value_exn params.write_ecache);
              if print_res then (
                Core.Printf.printf "Target operations:\n%!" ;
                List.iter phys_edits ~f:(fun e ->
                    Core.Printf.printf "%s\n%!" (edit_to_string e) ) )
              else ()]

let encode_cmd : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Convert P4 programs into their GCL-While interpretation"
    [%map_open
      let includes =
        flag "-I" (listed string)
          ~doc:"<dir> add directory to include search path"
      and verbose = flag "-v" no_arg ~doc:"verbose mode"
      and p4_file = anon ("p4_file" %: string) in
      fun () ->
        Encode.encode_from_p4 includes p4_file verbose
        |> Cmd.to_string |> Core.Printf.printf "%s"]

let benchmark =
  let open Command.Let_syntax in
  Command.basic ~summary:"Run benchmarks"
    [%map_open
      let opts = opt_params
      and mng = mng_params
      and varsize = anon ("varsize" %: int)
      and num_tables = anon ("num_tables" %: int)
      and max_inserts = anon ("max_inserts" %: int) in
      fun () ->
        let params = Parameters.union opts mng in
        ignore
          ( Benchmark.reorder_benchmark varsize num_tables max_inserts params
            : Edit.t list option )]

let onf_real : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"Run the onf benchmark on the real p4 programs"
    [%map_open
      let opts = opt_params
      and mng = mng_params
      and data = flag "-data" (required string) ~doc:"the input log"
      and print = flag "-p" no_arg ~doc:"show_result_at_end"
      and logical_p4 = anon ("p4file1" %: string)
      and physical_p4 = anon ("p4file1" %: string)
      and log_edits = anon ("p4file2" %: string)
      and phys_edits = anon ("phys_edits" %: string)
      and fvs = anon ("fvs" %: string)
      and assume = anon ("assume" %: string)
      and logical_inc =
        flag "-I1" (listed string)
          ~doc:"<dir> add directory to include search path for logical file"
      and physical_inc =
        flag "-I2" (listed string)
          ~doc:"<dir> add directory to include search path for physical file"
      in
      fun () ->
        let params = Parameters.union opts mng in
        let res =
          Benchmark.basic_onf_ipv4_real params data logical_p4 physical_p4
            log_edits phys_edits fvs assume logical_inc physical_inc
        in
        match res with
        | None -> Core.Printf.printf "no example could be found\n"
        | Some r when print ->
            List.iter r ~f:(fun edit ->
                Edit.to_string edit |> Core.Printf.printf "%s\n%!" )
        | Some _ -> ()]

let equality : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"Check equivalence"
    [%map_open
      let params, mk_prob, data = problem_flags <*> mng_params in
      fun () ->
        let problem = mk_prob () in
        assert (Option.is_none data) ;
        match Synthesis.implements params (ProfData.zero ()) problem with
        | None -> Core.Printf.printf "Equivalent\n%!"
        | Some (inpkt, _) ->
            let printer p i o =
              Core.Printf.printf "%s\n  in: %s\n  out: %s\n" p
                (Avenir.Packet.to_string i)
                (Avenir.Packet.to_string o)
            in
            let log_out =
              Avenir.Semantics.eval_act
                (Problem.log_gcl_program params problem)
                inpkt
            in
            let phys_out =
              Avenir.Semantics.eval_act
                (Problem.phys_gcl_program params problem)
                inpkt
            in
            Core.Printf.printf "--\n%!" ;
            printer "Log" inpkt log_out ;
            Core.Printf.printf "--\n%!" ;
            printer "Phys" inpkt phys_out ;
            Core.Printf.printf "\n\nDifferences\t\tlog\tphys\n" ;
            Problem.fvs problem
            |> List.iter ~f:(fun (fv, _) ->
                   match
                     ( Avenir.Packet.get_val_opt log_out fv
                     , Avenir.Packet.get_val_opt phys_out fv )
                   with
                   | None, None -> ()
                   | Some v, None ->
                       Core.Printf.printf "\t%s\t%s\tundefined\n" fv
                         (Value.to_string v)
                   | None, Some v ->
                       Core.Printf.printf "\t%s\tundefined\t%s\n" fv
                         (Value.to_string v)
                   | Some vl, Some vp ->
                       if not (Value.eq vl vp) then
                         Core.Printf.printf "\t%s\t%s\t%s\n" fv
                           (Value.to_string vl) (Value.to_string vp) )]

let summarize : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"Check equivalence"
    [%map_open
      let program = anon ("program_file" %: string)
      and p4 = flag "-P4" no_arg ~doc:"input full P4 programs"
      and includes =
        flag "-I1" (listed string)
          ~doc:"<dir> add directory to include search path for logical file"
      in
      fun () ->
        let cmd =
          if p4 then Encode.encode_from_p4 includes program false
          else Benchmark.parse_file program
        in
        let open Core.Printf in
        printf "In program %s\n" program ;
        printf "\t %d unique read variables\n" (Cmd.num_read_vars cmd) ;
        printf "\t %d unique assigned vars\n" (Cmd.num_assigned_vars cmd) ;
        printf "\t %d action data parameters\n"
          (Cmd.num_action_data_params cmd) ;
        printf "\t %d keys, %d unique\n" (Cmd.num_keys cmd)
          (Cmd.num_unique_keys cmd) ;
        printf "\t %d tables\n" (Cmd.num_tables cmd) ;
        printf "\t %d actions\n%!" (Cmd.num_actions cmd)]

let classbench_cmd : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"benchmarks generated insertions"
    [%map_open
      let opt = opt_params
      and mng = mng_params
      and data =
        flag "-data" (required string) ~doc:"path to classbench data"
      and expname = anon ("EXPNAME" %: string)
      and nrules = anon ("NRULES" %: int) in
      fun () ->
        let params = Parameters.union opt mng in
        if String.(uppercase expname = "IPONLY") then
          ignore (Benchmark.rep params data nrules : Edit.t list option)
        else if
          String.(uppercase expname = "TCPIP" || uppercase expname = "IPTCP")
        then
          ignore
            (Benchmark.rep_middle params data nrules : Edit.t list option)
        else if
          String.(
            uppercase expname = "FULL"
            || uppercase expname = "ETHTCPIP"
            || uppercase expname = "ETHIPTCP"
            || uppercase expname = "OF")
        then
          ignore
            (Benchmark.rep_of params false data nrules : Edit.t list option)
        else if String.(uppercase expname = "PAR") then
          ignore (Benchmark.rep_par params data nrules : Edit.t list option)
        else if String.(uppercase expname = "ALL") then
          List.iter (Parameters.all_params params) ~f:(fun params ->
              Core.Printf.printf "\n\n%s\n\n" (Parameters.to_string params) ;
              try
                ignore
                  (Benchmark.rep_par params data nrules : Edit.t list option)
              with _ -> Core.Printf.printf "well that failed" )
        else
          failwith
          @@ Core.Printf.sprintf "Unrecognized experiment parameter %s"
               expname]

let sqbench : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"run square benchmark"
    [%map_open
      let opts = opt_params
      and mng = mng_params
      and sz = anon ("SIZE" %: int)
      and ntbls = anon ("NTABLES" %: int)
      and nedits = anon ("NEDITS" %: int) in
      fun () ->
        let params = Parameters.union opts mng in
        Benchmark.square_bench params sz ntbls nedits]

let nhdrs_cmd : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"benchmarks generated insertions"
    [%map_open
      let opt = opt_params
      and mng = mng_params
      and sz = anon ("SIZE" %: int)
      and ntables = anon ("NTABLES" %: int)
      and nheaders = anon ("NHEADERS" %: int)
      and nrules = anon ("NRULES" %: int) in
      fun () ->
        let params = Parameters.union opt mng in
        Benchmark.headers params sz ntables nheaders nrules]

let metadata_cmd : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"benchmarks generated insertions"
    [%map_open
      let opt = opt_params
      and mng = mng_params
      and sz = anon ("SIZE" %: int)
      and nmeta = anon ("NMETA" %: int)
      and nrules = anon ("NRULES" %: int) in
      fun () ->
        let params = Parameters.union opt mng in
        Benchmark.metadata params sz nmeta nrules]

let ntbls_cmd : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"benchmarks generated insertions"
    [%map_open
      let opt = opt_params
      and mng = mng_params
      and sz = anon ("SIZE" %: int)
      and ntables = anon ("MAXTABLES" %: int)
      and nheaders = anon ("NHEADERS" %: int)
      and nrules = anon ("NRULES" %: int)
      and breadth =
        flag "-breadth" no_arg
          ~doc:"Breadth experiment when on, Length when not"
      in
      fun () ->
        let params = Parameters.union opt mng in
        if breadth then Benchmark.breadth params sz ntables nheaders nrules
        else Benchmark.tables params sz ntables nheaders nrules]

let random_bench : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"benchmarks random insertions into OBT -> Pipe"
    [%map_open
      let opt = opt_params
      and mng = mng_params
      and file =
        flag "-output" (required string)
          ~doc:"The file to which to write the experimental data"
      and lobits =
        flag "-lb" (required int) ~doc:"b The lowest bitwidth in the space"
      and hibits =
        flag "-hb" (optional int)
          ~doc:
            "B The highest bitwidth in the space. If omitted taken to be b"
      and lokeys =
        flag "-lk" (required int)
          ~doc:"k The lowest number of keys in the space"
      and hikeys =
        flag "-hk" (optional int)
          ~doc:
            "K The highest number of keys in the space. If omitted taken to \
             be k"
      and loouts =
        flag "-lo" (required int)
          ~doc:"o The lowest number of writable variables in the space"
      and hiouts =
        flag "-ho" (optional int)
          ~doc:
            "O The highest number of writable varaibles in the space. If \
             omitted taken to be o"
      and loedts =
        flag "-le" (required int)
          ~doc:"e The lowest number of writable edits in the space"
      and hiedts =
        flag "-he" (optional int)
          ~doc:
            "E The highest number of writable edits in the space. If \
             omitted taken to be E"
      in
      fun () ->
        let open Avenir.MicroBench in
        let params = Parameters.union opt mng in
        let lo = {bits= lobits; keys= lokeys; outs= loouts; edts= loedts} in
        let hi =
          { bits= Option.value hibits ~default:lobits
          ; keys= Option.value hikeys ~default:lokeys
          ; outs= Option.value hiouts ~default:loouts
          ; edts= Option.value hiedts ~default:loedts }
        in
        (hi.bits - lo.bits + 1)
        * (hi.keys - lo.keys + 1)
        * (hi.outs - lo.outs + 1)
        * (hi.edts - lo.edts + 1)
        |> Core.Printf.printf "Starting Experiments with %d cases\n%!" ;
        run_experiment params file lo hi ;
        Core.Printf.printf "Finished\n%!"]

let obt : Command.t =
  let open Command.Let_syntax in
  Command.basic ~summary:"Convert a program to one big table"
    [%map_open
      let prog_path = anon ("program" %: string)
      and p4 = flag "-P4" no_arg ~doc:"input file is a P4 program"
      and incl =
        flag "-I" (listed string)
          ~doc:"<dir> add director to include search path for logical file"
      in
      fun () ->
        let prog =
          if p4 then Encode.encode_from_p4 incl prog_path false
          else Benchmark.parse_file prog_path
        in
        let obt = OneBigTable.mk_one_big_table prog in
        Core.printf "%s\n" (Cmd.to_string obt)]

let server_cmd : Async_command.t =
  let open Command.Let_syntax in
  Command.async ~summary:"Invoke Avenir Server"
    [%map_open
      let params, mk_prob, data =
        problem_flags <*> map2 opt_params mng_params ~f:Parameters.union
      in
      fun () ->
        assert (Option.is_none data) ;
        Server.runserver params (mk_prob ()) ()]

let main : Command.t =
  Command.group ~summary:"Invokes the specified Avenir Command"
    [ ("synth", synthesize)
    ; ("server", server_cmd)
    ; ("encode-p4", encode_cmd)
    ; ("random", random_bench)
    ; ("bench", benchmark)
    ; ("square", sqbench)
    ; ("headers", nhdrs_cmd)
    ; ("tables", ntbls_cmd)
    ; ("metadata", metadata_cmd)
    ; ("classbench", classbench_cmd)
    ; ("onf-real", onf_real)
    ; ("obt", obt)
    ; ("eq", equality)
    ; ("summarize", summarize) ]

let () = Command.run main
