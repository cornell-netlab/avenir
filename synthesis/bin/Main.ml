open Core
module Ast = Motley.Ast
module Parser = Motley.Parser
module Lexer = Motley.Lexer
module Prover = Motley.Prover
module Synthesis = Motley.Synthesis
module Encode = Motley.Encode
module Manip = Motley.Manip
module Benchmark = Motley.Benchmark
module CheckAndSet = Motley.CheckAndSet
module Parameters = Motley.Parameters
module ProfData = Motley.ProfData
module Problem = Motley.Problem
module Tables = Motley.Tables
module Instance = Motley.Instance
module Runtime = Motley.Runtime


let parse_file (filename : string) : Ast.cmd =
  let cts = In_channel.read_all filename in
  let lexbuf = Lexing.from_string cts in
  Parser.main Lexer.tokens lexbuf

let parse_fvs fp =
  In_channel.read_lines fp
  |> List.map ~f:(fun line ->
         match String.lsplit2 line ~on:'#' with
         | None -> Printf.sprintf "Malformed FV line %s" line
                   |> failwith
         | Some (x, sz) -> (x, int_of_string sz)
       )



module Solver = struct
  let spec = Command.Spec.(
      empty
      +> anon ("logical" %: string)
      +> anon ("real" %: string)
      +> anon ("logical_edits" %: string)
      +> anon ("physical_edits" %: string)
      +> anon ("fvs" %: string)
      +> flag "-data" (required string) ~doc:"The logical experiment to run"
      +> flag "-p" no_arg ~doc:"Print synthesized program"
      +> flag "-w" no_arg ~doc:"Do widening"
      +> flag "-s" no_arg ~doc:"Do slicing optimization"
      +> flag "-m" no_arg ~doc:"Prune rows with no holes"
      +> flag "-inj" no_arg ~doc:"Try injection optimization"
      +> flag "-g" (required int) ~doc:"max number of CEGIS reps"
      +> flag "-DEBUG" no_arg ~doc:"Print Debugging commands"
      +> flag "-i" no_arg ~doc:"Interactive Mode"
      +> flag "-fastcx" no_arg ~doc:"Generate counterexample quickly"
      +> flag "-measure" no_arg ~doc:"Produce a CSV of data to stdout"
      +> flag "-onos" no_arg ~doc:"Parse logical edits as onos insertions"
      +> flag "--del-pushdown" no_arg ~doc:"interpret deletions as pushdowns when possible"
      +> flag "--phys-drop-spec" (optional string) ~doc:"fast counter-examples are restricted to admitted packets"
      +> flag "--above" no_arg ~doc:"synthesize new edits above existing instance, not below")

  let run logical real logical_edits physical_edits fvs data print_res widening do_slice monotonic injection gas debug interactive fastcx measure onos del_pushdown p_drop_spec above () =
    let params = Parameters.({widening;
                              do_slice;
                              gas;
                              debug;
                              monotonic;
                              injection;
                              interactive;
                              fastcx;
                              del_pushdown;
                              above}) in
    let log = parse_file logical in
    let phys = parse_file real in
    let log_inst = Runtime.parse logical_edits |> Instance.(update_list params empty) in
    let log_edits = if onos
                    then Benchmark.onos_to_edits data
                    else Runtime.parse data |> List.(map ~f:return)  in
    let phys_inst = Runtime.parse physical_edits |> Instance.(update_list params empty) in
    let fvs = parse_fvs fvs in
    let phys_drop_spec = Option.(p_drop_spec >>| fun dp -> parse_file dp |> Ast.get_test_from_assume)in
    if measure then
      let open Motley.Instance in
      let problem = Problem.make ~log ~phys ~log_inst ~phys_inst ~log_edits:[] ~fvs ~phys_drop_spec () in
      assert params.do_slice;
      let soln = Benchmark.measure params None problem log_edits in
      if print_res then
        List.iter soln ~f:(fun e -> Printf.printf "%s\n%!" (Tables.Edit.to_string e))
      else ()
    else
      let log_edits = List.join log_edits in
      let problem = Problem.make ~log ~phys ~log_inst ~phys_inst ~log_edits ~fvs ~phys_drop_spec () in
      match Synthesis.cegis_math_sequence params (ProfData.zero ()) problem with
      | None -> failwith "failed"
      | Some (solution, phys_edits) ->
         if print_res
         then
           begin
             Printf.printf "Synthesized Program (%d edits made)\n%!" (List.length phys_edits);
             Printf.printf "%s\n%!" (Problem.phys_gcl_program params solution |> Ast.string_of_cmd)
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
    ignore(Encode.encode_from_p4 include_dirs p4_file verbose : Ast.cmd)
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
      +> flag "-w" no_arg ~doc:"Do widening"
      +> flag "-s" no_arg ~doc:"Do slicing optimization"
      +> flag "-m" no_arg ~doc:"Prune rows with no holes"
      +> flag "-inj" no_arg ~doc:"Attempt an injection"
      +> flag "-g" (required int) ~doc:"max number of CEGIS reps"
      +> flag "-fastcx" no_arg ~doc:"Generate counterexample quickly")


  let run test_file widening do_slice monotonic injection gas fastcx () =
    In_channel.read_lines test_file
    |> List.iter
         ~f:(fun line ->
           match String.split line ~on:',' with
           | [log_str;phys_str;edits_str] ->
              let log = parse_file log_str in
              let phys = parse_file phys_str in
              let log_edits = Runtime.parse edits_str in
              let params = Parameters.({widening;
                                        do_slice;
                                        gas;
                                        monotonic;
                                        fastcx;
                                        injection;
                                        debug = false;
                                        interactive = false;
                                        del_pushdown = false;
                                        above = false;
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
                        Printf.printf " ✔✔ %s\n%!" line
                     | None ->
                        Printf.printf " ✗✗ %s\n%!" line
                with _ ->
                  Printf.printf " ✗✗  %s |---> Threw an exception\n%!" line


              end
           | _ ->
              Printf.sprintf "Cannot recognize string %s" line
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
      +> flag "-w" no_arg ~doc:"perform widening"
      +> flag "-s" no_arg ~doc:"perform slicing optimization"
      +> flag "-m" no_arg ~doc:"eliminate non-hole branches"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "-inj" no_arg ~doc:"try injection optimization"
      +> flag "-DEBUG" no_arg ~doc:"print debugging statements"
      +> flag "-fastcx" no_arg ~doc:"Generate counterexample quickly"
      +> flag "--del-pushdown" no_arg ~doc:"interpret deletions as pushdowns when possible"
      +> flag "--above" no_arg ~doc:"insert new edits above old instance instead of below")

  let run varsize num_tables max_inserts widening do_slice monotonic interactive injection debug fastcx del_pushdown above () =
    let params =
      Parameters.(
        { widening;
          do_slice;
          monotonic;
          interactive;
          injection;
          debug;
          fastcx;
          gas = 10;
          del_pushdown;
          above;
      })
    in
    ignore(Benchmark.reorder_benchmark varsize num_tables max_inserts params : Tables.Edit.t list)
end


let benchmark : Command.t =
  Command.basic_spec
    ~summary: "Run some benchmarks"
    Bench.spec
    Bench.run


module ONF = struct
  let spec = Command.Spec.(
      empty
      +> flag "-gas" (required int) ~doc:"how many cegis iterations?"
      +> flag "-w" no_arg ~doc:"perform widening"
      +> flag "-s" no_arg ~doc:"perform slicing optimization"
      +> flag "-m" no_arg ~doc:"eliminate non-hole branches"
      +> flag "-i" no_arg ~doc:"interactive mode"
      +> flag "-inj" no_arg ~doc:"try injection optimization"
      +> flag "-p" no_arg ~doc:"show_result_at_end"
      +> flag "-DEBUG" no_arg ~doc:"print debugging statements"
      +> flag "-data" (required string) ~doc:"the input log"
      +> flag "-fastcx" no_arg ~doc:"Generate counterexample quickly"
      +> flag "--del-pushdown" no_arg ~doc:"interpret deletions as pushdowns when possible"
      +> flag "--above" no_arg ~doc:"insert new edits above instance, not below")


  let run gas widening do_slice monotonic interactive injection print debug data_fp fastcx del_pushdown above() =
    let res = Benchmark.basic_onf_ipv4
              Parameters.({widening;do_slice;gas;monotonic;injection;interactive;debug;fastcx;del_pushdown;above})
              data_fp
    in
    if print then
      List.iter res ~f:(fun edit ->
          Tables.Edit.to_string edit
          |> Printf.printf "%s\n%!"
        )
end


let onf : Command.t =
  Command.basic_spec
    ~summary: "Run the onf benchmark"
    ONF.spec
    ONF.run

module RunningExample = struct
  let spec = Command.Spec.(
      empty
      +> flag "-gas" (required int) ~doc:"how many cegis iterations?"
      +> flag "-w" no_arg ~doc:"perform widening")


  let run gas widening () =
    ignore (Benchmark.running_example gas widening : Motley.Tables.Edit.t list)
end


let running_example : Command.t =
  Command.basic_spec
    ~summary: "Run the onf benchmark"
    RunningExample.spec
    RunningExample.run

let onf : Command.t =
  Command.basic_spec
    ~summary: "Run the onf benchmark"
    ONF.spec
    ONF.run

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
    let log = parse_file log in
    let phys = parse_file phys in
    let log_edits = Runtime.parse log_edits in
    let phys_edits = Runtime.parse phys_edits in
    let params = Parameters.(
        { debug;
          interactive = false;
          do_slice = false;
          widening = false;
          gas = 1;
          monotonic = false;
          fastcx = false;
          injection = false;
          del_pushdown = false;
          above = false; }) in
    let data = ProfData.zero () in
    let log_inst = Instance.empty in
    let phys_inst = Instance.empty in
    let fvs = parse_fvs fvs_fp in
    let problem =
      Problem.make ~log ~phys ~fvs
        ~log_inst ~phys_inst
        ~log_edits ()
      |> Motley.Util.flip Problem.replace_phys_edits phys_edits
    in
    match Synthesis.implements params data problem with
    | `Yes -> Printf.printf "Equivalent\n%!"
    | `NoAndCE (inpkt,_) ->
       let printer p i o =
         Printf.printf "%s\n  in: %s\n  out: %s\n" p
           (Motley.Packet.string__packet i)
           (Motley.Packet.string__packet o)
       in
       let log_out = Motley.Semantics.eval_act (Problem.log_gcl_program params problem) inpkt in
       let phys_out = Motley.Semantics.eval_act (Problem.phys_gcl_program params problem) inpkt in
       Printf.printf "--\n%!";
       printer "Log" inpkt log_out;
       Printf.printf "--\n%!";
       printer "Phys" inpkt phys_out



end


let equality : Command.t =
  Command.basic_spec
    ~summary: "Check equivalence"
    Equality.spec
    Equality.run


module WeakestPrecondition = struct
  let spec = Command.Spec.(
      empty
      +> anon ("file" %: string)
      +> flag "-z3" no_arg ~doc: "z3-ready output")

  let run file z3 () =
    let cmd = parse_file file in
    let _ : unit = Printf.printf "PROGRAM: %s \n%!" (Ast.string_of_cmd cmd) in
    let wp = Synthesis.symb_wp cmd in
    if z3 then
      Printf.printf "wp: %s" (Ast.string_of_test wp)
    else
      Printf.printf "%s" (Prover.toZ3String wp)
end

let wp_cmd : Command.t =
  Command.basic_spec
    ~summary:"Convert P4 programs into their GCL-While interpretation"
    WeakestPrecondition.spec
    WeakestPrecondition.run


module OFBench = struct
  let spec = Command.Spec.(
      empty
      +> anon ("file" %: string)
      +> flag "-w" no_arg ~doc:"do widening"
      +> flag "-gas" (optional int) ~doc:"how many cegis iterations?")

  let run classbench_file widening gas () =
    ignore(Benchmark.of_to_pipe1 widening gas classbench_file () : Tables.Edit.t list)
end


let of_bench : Command.t =
  Command.basic_spec
    ~summary:"benchmarks against of tables"
    OFBench.spec
    OFBench.run

module Meta = struct
  let spec = Command.Spec.(empty)
  let run = CheckAndSet.run
end

let meta : Command.t =
  Command.basic_spec
    ~summary:"run CheckAndSet test"
    Meta.spec
    Meta.run

let main : Command.t =
  Command.group
    ~summary:"Invokes the specified Motley Command"
    [ ("synth", synthesize_cmd)
    ; ("encode-p4", encode_cmd)
    ; ("runtest", runtest_cmd)
    ; ("bench", benchmark)
    ; ("onf", onf)
    ; ("eq", equality)
    ; ("ex", running_example)
    ; ("meta", meta)
    ; ("wp", wp_cmd)]

let () = Command.run main
