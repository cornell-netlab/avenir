open Core
open Async
open Async_command
module Ast = Avenir.Ast
module Parser = Avenir.Parser
module Lexer = Avenir.Lexer
module Prover = Avenir.Prover
module Synthesis = Avenir.Synthesis
module Encode = Avenir.Encode
module Manip = Avenir.Manip
module Benchmark = Avenir.Benchmark
module Classbenching = Avenir.Classbenching
module Parameters = Avenir.Parameters
module ProfData = Avenir.ProfData
module Problem = Avenir.Problem
module Tables = Avenir.Tables
module Instance = Avenir.Instance
module Runtime = Avenir.Runtime
module Server = Avenir.Server


let opt_flags =
  Command.Spec.(
    empty
    +> flag "-w" no_arg ~doc:"Do widening"
    +> flag "-s" no_arg ~doc:"Do slicing optimization"
    +> flag "-e" (required int) ~doc:"E maximum number of physical edits"
    +> flag "-b" (required int) ~doc:"B maximum number of attempts per CE"
    +> flag "-m" no_arg ~doc:"Prune rows with no holes"
    +> flag "--inj" no_arg ~doc:"Try injection optimization"
    +> flag "--fastcx" no_arg ~doc:"Generate counterexample quickly"
    +> flag "--cache-queries" no_arg ~doc:"Disable query and edit caching"
    +> flag "--cache-edits" no_arg ~doc:"Disable query and edit caching"
    +> flag "--shortening" no_arg ~doc:"shorten queries"
    +> flag "--above" no_arg ~doc:"synthesize new edits above existing instance, not below"
    +> flag "--min" no_arg ~doc:"try and eliminate each edit in turn"
    +> flag "--hints" no_arg ~doc:"Use syntactic hints"
    +> flag "--holes" no_arg ~doc:"Holes only"
    +> flag "--annot" no_arg ~doc:"Use hard-coded edits"
    +> flag "--nlp" no_arg ~doc:"variable name based domain restrictions"
    +> flag "--unique-edits" no_arg ~doc:"Only one edit allowed per table"
    +> flag "--domain-restrict" no_arg ~doc:"Restrict allowed values to those that occur in the programs"
    +> flag "--restrict-masks" no_arg ~doc:"Restrict masks"
    +> flag "--no-defaults" no_arg ~doc:"Prefer solutions that don't rely on default actions ")



module Solver = struct
  let spec = Command.Spec.(
      (empty
       +> anon ("logical" %: string)
       +> anon ("real" %: string)
       +> anon ("logical_edits" %: string)
       +> anon ("physical_edits" %: string)
       +> anon ("fvs" %: string)
       +> flag "-P4" no_arg ~doc:"input full P4 programs"
       +> flag "-I1" (listed string) ~doc:"<dir> add directory to include search path for logical file"
       +> flag "-I2" (listed string) ~doc:"<dir> add directory to include search path for physical file")

      +> flag "-data" (required string) ~doc:"The logical experiment to run"
      +> flag "-DEBUG" no_arg ~doc:"Print Debugging commands"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "-p" no_arg ~doc:"Print synthesized program"
      +> flag "-measure" no_arg ~doc:"Produce a CSV of data to stdout"
      +> flag "-onos" no_arg ~doc:"Parse logical edits as onos insertions"

      ++ opt_flags)

  let run
        logical
        real
        logical_edits
        physical_edits
        fvs
        p4
        log_incl
        phys_incl
        data
        debug
        interactive
        print_res
        measure
        onos
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults () =
    let params = Parameters.({widening;
                              do_slice;
                              edits_depth;
                              search_width;
                              debug;
                              monotonic;
                              interactive;
                              injection;
                              fastcx;
                              vcache;
                              ecache;
                              shortening;
                              above;
                              minimize;
                              hints;
                              only_holes;
                              allow_annotations;
                              nlp;
                              unique_edits;
                              domain;
                              restrict_mask;
                              no_defaults;
                              timeout = None
                 }) in
    let var_mapping = Benchmark.parse_fvs fvs in
    let fvs = List.map var_mapping ~f:snd in
    let log = if p4
              then Encode.encode_from_p4 log_incl logical false
                   |> Benchmark.zero_init fvs |> Benchmark.drop_handle fvs
              else Benchmark.parse_file logical in
    let phys = if p4
               then Encode.encode_from_p4 phys_incl real false
                    |> Benchmark.zero_init fvs |> Benchmark.drop_handle fvs
               else Benchmark.parse_file real in
    let log_inst = Runtime.parse logical_edits |> Instance.(update_list params empty) in
    let log_edits = if onos
                    then Benchmark.onos_to_edits data "ipv6"
                    else Runtime.parse data |> List.(map ~f:return)  in
    let phys_inst = Runtime.parse physical_edits |> Instance.(update_list params empty) in
    let phys_drop_spec = None in
    if measure then
      let open Avenir.Instance in
      let problem = Problem.make ~log ~phys ~log_inst ~phys_inst ~log_edits:[] ~fvs ~phys_drop_spec () in
      match Benchmark.measure params None problem log_edits with
      |  None -> Core.Printf.printf "No solution could be found \n%!"
      | Some soln when print_res ->
         Core.Printf.printf "EDITS:\n%!";
         List.iter soln ~f:(fun e -> Core.Printf.printf "%s\n%!" (Tables.Edit.to_string e))
      | _ -> ()
    else
      let log_edits = List.join log_edits in
      let problem = Problem.make ~log ~phys ~log_inst ~phys_inst ~log_edits ~fvs ~phys_drop_spec () in
      Core.Printf.printf "PROBLEM: %s \n" (Problem.to_string params problem);
      match Synthesis.cegis_math_sequence params (ProfData.zero ()) problem with
      | None -> failwith "failed"
      | Some (solution, phys_edits) ->
         if print_res
         then
           begin
             Core.Printf.printf "Target operations:\n%!";
             List.iter phys_edits ~f:(fun e -> Core.Printf.printf "%s\n%!" (Tables.Edit.to_string e))
           end
         else ()
end


let synthesize_cmd : Command.t =
  Command.basic_spec
    ~summary:"Compute modifications to real program to implement logical program "
    Solver.spec
    Solver.run

module Encoder = struct
  let spec = Command.Spec.(
      empty
      +> flag "-I" (listed string) ~doc:"<dir> add directory to include search path"
      +> flag "-v" no_arg ~doc:"verbose mode"
      +> anon ("p4_file" %: string))

  let run include_dirs verbose p4_file () =
    Encode.encode_from_p4 include_dirs p4_file verbose
    |> Ast.string_of_cmd
    |> Core.Printf.printf "%s"
end

let encode_cmd : Command.t =
  Command.basic_spec
    ~summary:"Convert P4 programs into their GCL-While interpretation"
    Encoder.spec
    Encoder.run

module RunTest = struct
  let spec = Command.Spec.(
      empty
      +> anon ("test_file" %: string)
      +> flag "-DEBUG" no_arg ~doc:"DEBUG"
      +> flag "-i" no_arg ~doc:"Interactive mode"
      ++ opt_flags)

  let run test_file
        debug
        interactive
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults () =
    In_channel.read_lines test_file
    |> List.iter
         ~f:(fun line ->
           match String.split line ~on:',' with
           | [log_str;phys_str;edits_str] ->
              let log = Benchmark.parse_file log_str in
              let phys = Benchmark.parse_file phys_str in
              let log_edits = Runtime.parse edits_str in
              let params = Parameters.({
                              widening;
                              do_slice;
                              edits_depth;
                              search_width;
                              debug;
                              monotonic;
                              interactive;
                              injection;
                              fastcx;
                              vcache;
                              ecache;
                              shortening;
                              above;
                              minimize;
                              hints;
                              only_holes;
                              allow_annotations;
                              nlp;
                              unique_edits;
                              domain;
                              no_defaults;
                              restrict_mask;
                              timeout = None;
                           }) in
              let problem = Problem.make ~log ~phys ~log_edits
                              ~log_inst:Instance.empty
                              ~phys_inst:Instance.empty
                              ~fvs:(List.dedup_and_sort ~compare:Stdlib.compare
                                      Ast.(free_of_cmd `Var log @ free_of_cmd `Var phys)) () in
              let data = ProfData.zero () in
              begin
                try
                     match Synthesis.cegis_math_sequence params data problem with
                     | Some _  ->
                        Core.Printf.printf " ✔✔ %s\n%!" line
                     | None ->
                        Core.Printf.printf " ✗✗ %s\n%!" line
                with _ ->
                  Core.Printf.printf " ✗✗  %s |---> Threw an exception\n%!" line


              end
           | _ ->
              Core.Printf.sprintf "Cannot recognize string %s" line
              |> failwith
         )

end

let runtest_cmd : Command.t =
  Command.basic_spec
    ~summary:"Run the tests "
    RunTest.spec
    RunTest.run




module Bench = struct
  let spec = Command.Spec.(
      empty
      +> anon ("varsize" %: int)
      +> anon ("num_tables" %: int)
      +> anon ("max_inserts" %: int)
      +> flag "-DEBUG" no_arg ~doc:"print debugging statements"
      +> flag "-i" no_arg ~doc:"interactive mode"
      ++ opt_flags )

  let run varsize num_tables max_inserts
        debug
        interactive
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults () =
    let params =
      Parameters.(
        {
          widening;
          do_slice;
          edits_depth;
          search_width;
          debug;
          monotonic;
          interactive;
          injection;
          fastcx;
          vcache;
          ecache;
          shortening;
          above;
          minimize;
          hints;
          only_holes;
          allow_annotations;
          nlp;
          unique_edits;
          domain;
          restrict_mask;
          no_defaults;
          timeout = None;
        })
    in
    ignore(Benchmark.reorder_benchmark varsize num_tables max_inserts params : Tables.Edit.t list option)
end


let benchmark : Command.t =
  Command.basic_spec
    ~summary: "Run some benchmarks"
    Bench.spec
    Bench.run


module ONF = struct
  let spec = Command.Spec.(
      empty
      +> flag "-DEBUG" no_arg ~doc:"print debugging statements"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "-data" (required string) ~doc:"the input log"
      +> flag "-p" no_arg ~doc:"show_result_at_end"
      ++ opt_flags )


  let run debug interactive data print
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults
    () =
    let res = Benchmark.basic_onf_ipv4
              Parameters.({
          widening;
          do_slice;
          edits_depth;
          search_width;
          debug;
          monotonic;
          interactive;
          injection;
          fastcx;
          vcache;
          ecache;
          shortening;
          above;
          minimize;
          hints;
          only_holes;
          allow_annotations;
          nlp;
          unique_edits;
          domain;
          restrict_mask;
          no_defaults;
          timeout = None})
              data
    in
    match res with
    | None -> Core.Printf.printf "no example could be found\n"
    | Some r when print ->
       List.iter r ~f:(fun edit ->
           Tables.Edit.to_string edit
           |> Core.Printf.printf "%s\n%!"
         )
    | _ -> ()
end

let onf : Command.t =
  Command.basic_spec
    ~summary: "Run the onf benchmark"
    ONF.spec
    ONF.run


module ONFReal = struct
  let spec = Command.Spec.(
      empty
      +> flag "-DEBUG" no_arg ~doc:"print debugging statements"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "-data" (required string) ~doc:"the input log"
      +> flag "-p" no_arg ~doc:"show_result_at_end"
      +> anon ("p4file1" %: string)
      +> anon ("p4file2" %: string)
      +> anon ("log_edits" %: string)
      +> anon ("phys_edits" %: string)
      +> anon ("fvs" %: string)
      +> anon ("assume" %: string)
      +> flag "-I1" (listed string) ~doc:"<dir> add directory to include search path for logical file"
      +> flag "-I2" (listed string) ~doc:"<dir> add directory to include search path for physical file"
      ++ opt_flags)




  let run debug interactive data print
        logical_p4 physical_p4 log_edits phys_edits fvs assume logical_inc physical_inc
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults
    () =
    let res = Benchmark.basic_onf_ipv4_real
              Parameters.({
          widening;
          do_slice;
          edits_depth;
          search_width;
          debug;
          monotonic;
          interactive;
          injection;
          fastcx;
          vcache;
          ecache;
          shortening;
          above;
          minimize;
          hints;
          only_holes;
          allow_annotations;
          nlp;
          unique_edits;
          domain;
          restrict_mask;
          no_defaults;
          timeout = None})
              data logical_p4 physical_p4 log_edits phys_edits fvs assume logical_inc physical_inc
    in
    match res with
    | None -> Core.Printf.printf "no example could be found\n"
    | Some r when print ->
       List.iter r ~f:(fun edit ->
           Tables.Edit.to_string edit
           |> Core.Printf.printf "%s\n%!"
         )
    | Some _ -> ()

end

let onf_real : Command.t =
  Command.basic_spec
    ~summary: "Run the onf benchmark on the real p4 programs"
    ONFReal.spec
    ONFReal.run

module Equality = struct
  let spec = Command.Spec.(
      empty
      +> anon ("log" %: string)
      +> anon ("phys" %: string)
      +> anon ("log_edits" %: string)
      +> anon ("phys_edits" %: string)
      +> anon ("fvs" %: string)
      +> flag "-DEBUG" no_arg ~doc:"Debugging messages" )


  let run log phys log_edits phys_edits fvs_fp debug () =
    let log = Benchmark.parse_file log in
    let phys = Benchmark.parse_file phys in
    let log_edits = Runtime.parse log_edits in
    let phys_edits = Runtime.parse phys_edits in
    let params = Parameters.(
        { default with
          debug;
          ecache = true;
          vcache = true}) in
    let data = ProfData.zero () in
    let log_inst = Instance.empty in
    let phys_inst = Instance.empty in
    let fvs = Benchmark.parse_fvs fvs_fp |> List.map ~f:snd in
    let problem =
      Problem.make ~log ~phys ~fvs
        ~log_inst ~phys_inst
        ~log_edits ()
      |> Avenir.Util.flip Problem.replace_phys_edits phys_edits
    in
    match Synthesis.implements params data problem with
    | `Yes -> Core.Printf.printf "Equivalent\n%!"
    | `NoAndCE (inpkt,_) ->
       let printer p i o =
         Core.Printf.printf "%s\n  in: %s\n  out: %s\n" p
           (Avenir.Packet.string__packet i)
           (Avenir.Packet.string__packet o)
       in
       let log_out = Avenir.Semantics.eval_act (Problem.log_gcl_program params problem) inpkt in
       let phys_out = Avenir.Semantics.eval_act (Problem.phys_gcl_program params problem) inpkt in
       Core.Printf.printf "--\n%!";
       printer "Log" inpkt log_out;
       Core.Printf.printf "--\n%!";
       printer "Phys" inpkt phys_out;
       Core.Printf.printf "\n\nDifferences\t\tlog\tphys\n";
       List.iter fvs
         ~f:(fun (fv,_) ->
           match Avenir.Util.StringMap.find log_out fv
               , Avenir.Util.StringMap.find phys_out fv with
           | None, None -> ()
           | Some (Int(v,_)), None -> Core.Printf.printf "\t%s\t%s\tundefined\n"
                                         fv (Bigint.Hex.to_string v)
           | None, Some (Int(v,_)) -> Core.Printf.printf "\t%s\tundefined\t%s\n"
                                         fv (Bigint.Hex.to_string v)
           | Some (Int(vl,_)), Some(Int(vp,_)) ->
              if Bigint.(vl <> vp) then
                Core.Printf.printf "\t%s\t%s\t%s\n"
                  fv (Bigint.Hex.to_string vl) (Bigint.Hex.to_string vp)
         )

end

let equality : Command.t =
  Command.basic_spec
    ~summary: "Check equivalence"
    Equality.spec
    Equality.run


module EqualityReal = struct
  let spec = Command.Spec.(
      empty
      +> anon ("log" %: string)
      +> anon ("phys" %: string)
      +> flag "-I1" (listed string) ~doc:"<dir> add directory to include search path for logical file"
      +> flag "-I2" (listed string) ~doc:"<dir> add directory to include search path for physical file"
      +> anon ("log_edits" %: string)
      +> anon ("phys_edits" %: string)
      +> anon ("fvs" %: string)
      +> anon ("assume" %: string)
      +> flag "-DEBUG" no_arg ~doc:"Debugging messages" )


  let run log_p4 phys_p4  log_incs phys_incs log_edits phys_edits fvs_fp assume_fp debug () =
    let fvs = Benchmark.parse_fvs fvs_fp |> List.map ~f:snd in
    let assume = Benchmark.parse_file assume_fp in

    let () = printf "fvs = %s" (Sexp.to_string ([%sexp_of: (string * int) list] fvs)) in

    let open Ast in
    let log = (assume %:% Encode.encode_from_p4 log_incs log_p4 false) |> Benchmark.zero_init fvs |> Benchmark.drop_handle fvs in
    let phys = (assume %:% Encode.encode_from_p4 phys_incs phys_p4 false) |> Benchmark.zero_init fvs |> Benchmark.drop_handle fvs in

    Format.printf "Log Encoded Program: \n%!\n %s%! \n%!" (string_of_cmd log);
    Format.printf "Phys Encoded Program: \n%!\n %s%! \n%!" (string_of_cmd phys);

    (* let log = Encode.encode_from_p4 log_incs log false in
    let phys = Encode.encode_from_p4 phys_incs phys false in
    *)
    let log_edits = Runtime.parse log_edits in
    let phys_edits = Runtime.parse phys_edits in
    let params = Parameters.(
        { default with
          debug;
          ecache = true;
          vcache = true}) in
    let data = ProfData.zero () in
    let log_inst = Instance.empty in
    let phys_inst = Instance.empty in
    (*let fvs = Benchmark.parse_fvs fvs_fp in *)
    let problem =
      Problem.make ~log ~phys ~fvs
        ~log_inst ~phys_inst
        ~log_edits ()
      |> Avenir.Util.flip Problem.replace_phys_edits phys_edits
    in
    match Synthesis.implements params data problem with
    | `Yes -> Core.Printf.printf "Equivalent\n%!"
    | `NoAndCE (inpkt,_) ->
       let printer p i o =
         Core.Printf.printf "%s\n  in: %s\n  out: %s\n" p
           (Avenir.Packet.string__packet i)
           (Avenir.Packet.string__packet o)
       in
       let log_out = Avenir.Semantics.eval_act (Problem.log_gcl_program params problem) inpkt in
       let phys_out = Avenir.Semantics.eval_act (Problem.phys_gcl_program params problem) inpkt in
       Core.Printf.printf "--\n%!";
       printer "Log" inpkt log_out;
       Core.Printf.printf "--\n%!";
       printer "Phys" inpkt phys_out



end



let equality_real : Command.t =
  Command.basic_spec
    ~summary: "Check equivalence"
    EqualityReal.spec
    EqualityReal.run


module WeakestPrecondition = struct
  let spec = Command.Spec.(
      empty
      +> anon ("file" %: string)
      +> flag "-z3" no_arg ~doc: "z3-ready output")

  let run file z3 () =
    let cmd = Benchmark.parse_file file in
    let _ : unit = Core.Printf.printf "PROGRAM: %s \n%!" (Ast.string_of_cmd cmd) in
    let wp = Synthesis.symb_wp cmd in
    if z3 then
      Core.Printf.printf "wp: %s" (Ast.string_of_test wp)
    else
      Core.Printf.printf "%s" (Prover.toZ3String wp)
end

let wp_cmd : Command.t =
  Command.basic_spec
    ~summary:"Convert P4 programs into their GCL-While interpretation"
    WeakestPrecondition.spec
    WeakestPrecondition.run

module Classbench = struct
  let spec = Command.Spec.(
      empty
      +> anon ("EXPNAME" %: string)
      +> anon ("NRULES" %: int)
      +> flag "-data" (required string) ~doc:"path to classbench data"
      +> flag "-DEBUG" no_arg ~doc:"debug"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "--timeout" (optional float) ~doc:"Optional timeout in seconds"
      ++ opt_flags)

  let run
        expname
        nrules
        data
        debug
        interactive
        timeout
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults
        () =
    let params =
      Parameters.({
                     widening;
                     do_slice;
                     edits_depth;
                     search_width;
                     debug;
                     monotonic;
                     interactive;
                     injection;
                     fastcx;
                     vcache;
                     ecache;
                     shortening;
                     above;
                     minimize;
                     hints;
                     only_holes;
                     allow_annotations;
                     nlp;
                     unique_edits;
                     domain;
                     restrict_mask;
                     no_defaults;
                     timeout = Option.(timeout >>= fun s -> Some(Time.now(), (Time.Span.of_sec s)))
      })
    in
    if String.(uppercase expname = "IPONLY") then
      ignore(Benchmark.rep params data nrules : Tables.Edit.t list option)
    else if String.(uppercase expname = "TCPIP" || uppercase expname = "IPTCP") then
      ignore(Benchmark.rep_middle params data nrules : Tables.Edit.t list option)
    else if String.(uppercase expname = "FULL"
                    || uppercase expname = "ETHTCPIP"
                    || uppercase expname = "ETHIPTCP"
                    || uppercase expname = "OF") then
      ignore(Benchmark.rep_of params false data nrules : Tables.Edit.t list option)
    else if String.(uppercase expname = "PAR") then
      ignore(Benchmark.rep_par params data nrules : Tables.Edit.t list option)
    else if String.(uppercase expname = "ALL") then
      List.iter (Parameters.all_params params)
        ~f:(fun params ->
          Core.Printf.printf "\n\n%s\n\n" (Parameters.to_string params);
          try
            ignore (Benchmark.rep_par params data nrules : Tables.Edit.t list option)
          with _ ->
            Core.Printf.printf "well that failed"
        )
    else
      failwith @@ Core.Printf.sprintf "Unrecognized experiment parameter %s" expname


end

let classbench_cmd : Command.t =
  Command.basic_spec
    ~summary:"benchmarks generated insertions"
    Classbench.spec
    Classbench.run

module SqBench = struct
  let spec = Command.Spec.(
      empty
      +> anon ("SIZE" %: int)
      +> anon ("NTABLES" %: int)
      +> anon ("NEDITS" %: int)
      +> flag "-DEBUG" no_arg ~doc:"debug"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "--timeout" (optional float) ~doc:"Optional timeout in seconds"
      ++ opt_flags)
  let run
        sz
        ntbls
        nedits
        debug
        interactive
        timeout
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults
        ()
    =    let params =
      Parameters.(
        {
          widening;
          do_slice;
          edits_depth;
          search_width;
          debug;
          monotonic;
          interactive;
          injection;
          fastcx;
          vcache;
          ecache;
          shortening;
          above;
          minimize;
          hints;
          only_holes;
          allow_annotations;
          nlp;
          unique_edits;
          domain;
          restrict_mask;
          no_defaults;
          timeout = Option.(timeout >>= fun s -> Some(Time.now(), (Time.Span.of_sec s)))
        })
    in
    Benchmark.square_bench params sz ntbls nedits

end

let sqbench : Command.t =
  Command.basic_spec
    ~summary:"run square benchmark"
    SqBench.spec
    SqBench.run



module NumHdrs = struct
  let spec = Command.Spec.(
      empty
      +> anon ("SIZE" %: int)
      +> anon ("NTABLES" %: int)
      +> anon ("NHEADERS" %: int)
      +> anon ("NRULES" %: int)
      +> flag "-DEBUG" no_arg ~doc:"debug"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "--timeout" (optional float) ~doc:"Optional timeout in seconds"
      ++ opt_flags)

  let run
        sz
        ntables
        nheaders
        nrules
        debug
        interactive
        timeout
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults
        () =
    let params =
      Parameters.({
                     widening;
                     do_slice;
                     edits_depth;
                     search_width;
                     debug;
                     monotonic;
                     interactive;
                     injection;
                     fastcx;
                     vcache;
                     ecache;
                     shortening;
                     above;
                     minimize;
                     hints;
                     only_holes;
                     allow_annotations;
                     nlp;
                     unique_edits;
                     domain;
                     restrict_mask;
                     no_defaults;
                     timeout = Option.(timeout >>= fun s -> Some(Time.now(), (Time.Span.of_sec s)))
      })
    in
    Benchmark.headers params sz ntables nheaders nrules


end

let nhdrs_cmd : Command.t =
  Command.basic_spec
    ~summary:"benchmarks generated insertions"
    NumHdrs.spec
    NumHdrs.run




module MetadataBench = struct
  let spec = Command.Spec.(
      empty
      +> anon ("SIZE" %: int)
      +> anon ("NMETA" %: int)
      +> anon ("NRULES" %: int)
      +> flag "-DEBUG" no_arg ~doc:"debug"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "--timeout" (optional float) ~doc:"Optional timeout in seconds"
      ++ opt_flags)

  let run
        sz
        nmeta
        nrules
        debug
        interactive
        timeout
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults
        () =
    let params =
      Parameters.({
                     widening;
                     do_slice;
                     edits_depth;
                     search_width;
                     debug;
                     monotonic;
                     interactive;
                     injection;
                     fastcx;
                     vcache;
                     ecache;
                     shortening;
                     above;
                     minimize;
                     hints;
                     only_holes;
                     allow_annotations;
                     nlp;
                     unique_edits;
                     domain;
                     restrict_mask;
                     no_defaults;
                     timeout = Option.(timeout >>= fun s -> Some(Time.now(), (Time.Span.of_sec s)))
      })
    in
    Benchmark.metadata params sz nmeta nrules


end

let metadata_cmd : Command.t =
  Command.basic_spec
    ~summary:"benchmarks generated insertions"
    MetadataBench.spec
    MetadataBench.run


module NumTbls = struct
  let spec = Command.Spec.(
      empty
      +> anon ("SIZE" %: int)
      +> anon ("MAXTABLES" %: int)
      +> anon ("NHEADERS" %: int)
      +> anon ("NRULES" %: int)
      +> flag "-breadth" no_arg ~doc:"Breadth experiment when on, Length when not"
      +> flag "-DEBUG" no_arg ~doc:"debug"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "--timeout" (optional float) ~doc:"Optional timeout in seconds"
      ++ opt_flags)

  let run
        sz
        ntables
        nheaders
        nrules
        breadth
        debug
        interactive
        timeout
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults
        () =
    let params =
      Parameters.({
                     widening;
                     do_slice;
                     edits_depth;
                     search_width;
                     debug;
                     monotonic;
                     interactive;
                     injection;
                     fastcx;
                     vcache;
                     ecache;
                     shortening;
                     above;
                     minimize;
                     hints;
                     only_holes;
                     allow_annotations;
                     nlp;
                     unique_edits;
                     domain;
                     restrict_mask;
                     no_defaults;
                     timeout = Option.(timeout >>= fun s -> Some(Time.now(), (Time.Span.of_sec s)))
      })
    in
    if breadth then
      Benchmark.breadth params sz ntables nheaders nrules
    else
      Benchmark.tables params sz ntables nheaders nrules


end

let ntbls_cmd : Command.t =
  Command.basic_spec
    ~summary:"benchmarks generated insertions"
    NumTbls.spec
    NumTbls.run



module ServerCmd = struct
  let spec = Async_command.Spec.(
      empty
      +> anon ("logical" %: string)
      +> anon ("real" %: string)
      +> anon ("logical_edits" %: string)
      +> anon ("physical_edits" %: string)
      +> anon ("fvs" %: string)
      +> flag "-P4" no_arg ~doc:"input full P4 programs"
      +> flag "-I1" (listed string) ~doc:"<dir> add directory to include search path for logical file"
      +> flag "-I2" (listed string) ~doc:"<dir> add directory to include search path for physical file"
      +> flag "-DEBUG" no_arg ~doc:"Print Debugging commands"
      +> flag "-p" no_arg ~doc:"Print synthesized program"
      +> flag "-measure" no_arg ~doc:"Produce a CSV of data to stdout"
      +> flag "-onos" no_arg ~doc:"Parse logical edits as onos insertions"
      +> flag "-w" no_arg ~doc:"Do widening"
      +> flag "-s" no_arg ~doc:"Do slicing optimization"
      +> flag "-e" (required int) ~doc:"maximum number of physical edits"
      +> flag "-b" (required int) ~doc:"maximum number of attempts per CE"
      +> flag "-m" no_arg ~doc:"Prune rows with no holes"
      +> flag "--inj" no_arg ~doc:"Try injection optimization"
      +> flag "--fastcx" no_arg ~doc:"Generate counterexample quickly"
      +> flag "--cache-queries" no_arg ~doc:"Disable query and edit caching"
      +> flag "--cache-edits" no_arg ~doc:"Disable query and edit caching"
      +> flag "--shortening" no_arg ~doc:"shorten queries"
      +> flag "--above" no_arg ~doc:"synthesize new edits above existing instance, not below"
      +> flag "--min" no_arg ~doc:"try and eliminate each edit in turn"
      +> flag "--hints" no_arg ~doc:"Use syntactic hints"
      +> flag "--holes" no_arg ~doc:"Holes only"
      +> flag "--annot" no_arg ~doc:"Use hard-coded edits"
      +> flag "--nlp" no_arg ~doc:"variable name based domain restrictions"
      +> flag "--unique-edits" no_arg ~doc:"Only one edit allowed per table"
      +> flag "--domain-restrict" no_arg ~doc:"Restrict allowed values to those that occur in the programs"
      +> flag "--restrict-masks" no_arg ~doc:"Restrict masks"
      +> flag "--no-defaults" no_arg ~doc:"Prefer solutions that don't rely on default actions ")

  let run
        logical
        real
        logical_edits
        physical_edits
        fvs
        p4
        log_incl
        phys_incl
        debug
        print_res
        measure
        onos
        widening
        do_slice
        edits_depth
        search_width
        monotonic
        injection
        fastcx
        vcache
        ecache
        shortening
        above
        minimize
        hints
        only_holes
        allow_annotations
        nlp
        unique_edits
        domain
        restrict_mask
        no_defaults () =
    let params = Parameters.({widening;
                              do_slice;
                              edits_depth;
                              search_width;
                              debug;
                              monotonic;
                              interactive = false;
                              injection;
                              fastcx;
                              vcache;
                              ecache;
                              shortening;
                              above;
                              minimize;
                              hints;
                              only_holes;
                              allow_annotations;
                              nlp;
                              unique_edits;
                              domain;
                              restrict_mask;
                              no_defaults;
                              timeout = None
                 }) in
    let mapping = Benchmark.parse_fvs fvs in
    let fvs = List.map mapping ~f:snd in
    let log = if p4
              then Encode.encode_from_p4 log_incl logical false
                   |> Encode.unify_names mapping
                   |> Benchmark.zero_init fvs
                   |> Benchmark.drop_handle fvs
              else Benchmark.parse_file logical in
    let phys = if p4
               then Encode.encode_from_p4 phys_incl real false
                    |> Benchmark.zero_init fvs |> Benchmark.drop_handle fvs
               else Benchmark.parse_file real in
    let log_inst = Runtime.parse logical_edits |> Instance.(update_list params empty) in
    let phys_inst = Runtime.parse physical_edits |> Instance.(update_list params empty) in
    let phys_drop_spec = None in
    let problem = Problem.make ~log ~phys ~log_inst ~phys_inst ~log_edits:[] ~fvs ~phys_drop_spec () in
    Server.runserver params problem ()
end


let server_cmd : Async_command.t =
    Command.async_spec
      ~summary:"Invoke Avenir Server"
      ServerCmd.spec
      ServerCmd.run

let main : Command.t =
  Command.group
    ~summary:"Invokes the specified Avenir Command"
    [ ("synth", synthesize_cmd)
    ; ("server", server_cmd)
    ; ("encode-p4", encode_cmd)
    ; ("runtest", runtest_cmd)
    ; ("bench", benchmark)
    ; ("square", sqbench)
    ; ("headers", nhdrs_cmd)
    ; ("tables", ntbls_cmd)
    ; ("metadata", metadata_cmd)
    ; ("classbench", classbench_cmd)
    ; ("onf", onf)
    ; ("onf-real", onf_real)
    ; ("eq", equality)
    ; ("eq-real", equality_real)
    ; ("wp", wp_cmd)]

let () = Command.run main
