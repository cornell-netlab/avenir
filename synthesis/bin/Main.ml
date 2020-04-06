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
module Runtime = Motley.Runtime


let parse_file (filename : string) : Ast.cmd =
  let cts = In_channel.read_all filename in
  let lexbuf = Lexing.from_string cts in
  Parser.main Lexer.tokens lexbuf


module Solver = struct
  let spec = Command.Spec.(
      empty
      +> anon ("logical" %: string)
      +> anon ("real" %: string)
      +> anon ("logical_edits" %: string)
      +> flag "-p" no_arg ~doc:"Print synthesized program"
      +> flag "-w" no_arg ~doc:"Do widening"
      +> flag "-s" no_arg ~doc:"Do slicing optimization"
      +> flag "-m" no_arg ~doc:"Prune rows with no holes"
      +> flag "-g" (required int) ~doc:"max number of CEGIS reps"
      +> flag "-DEBUG" no_arg ~doc:"Print Debugging commands"
      +> flag "-i" no_arg ~doc:"Interactive Mode"
      +> flag "-fastcx" no_arg ~doc:"Generate counterexample quickly")

  let run logical real logical_edits print_res widening do_slice monotonic gas debug interactive fastcx () =
    let log = parse_file logical in
    let phys = parse_file real in
    let log_edits = Runtime.parse logical_edits in
    let phys_edits =
      Synthesis.cegis_math
        Parameters.({widening;
                     do_slice;
                     gas;
                     debug;
                     monotonic;
                     interactive;
                     fastcx})
        (ProfData.zero ())
        Problem.({log; phys; log_inst = Motley.Tables.Instance.empty;
                  phys_inst = Motley.Tables.Instance.empty;
                  log_edits = log_edits;
                  phys_edits = [];
                  cexs = [];
                  model_space = True;
                  fvs =
                    List.dedup_and_sort ~compare:Stdlib.compare
                    Ast.(free_of_cmd `Var log @ free_of_cmd `Var phys)})
    in
    match phys_edits with
    | None -> Printf.printf "Failed\n%!"
    | Some phys_edits ->
       if not debug && not interactive && print_res
       then
         begin
           let synth_inst = Tables.Instance.(update_list empty phys_edits) in
           Printf.printf "Synthesized Program (%d edits made)\n%!" (List.length phys_edits);
           Printf.printf "%s\n%!" (Tables.Instance.apply `NoHoles `Exact synth_inst phys |> fst |> Ast.string_of_cmd)
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


module EditCheck = struct
  let spec = Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:"dry-run-mode: Output the instrumented programs"
      +> flag "-n" (required int) ~doc:"The number of concrete edits"
      +> flag "-t" (required string) ~doc:"The logical table to edit"
      +> anon ("logical" %: string)
      +> anon ("concrete" %: string))


  let run (_:bool) _ _ _ _ () =
    Printf.printf "deprecated"
    (* let log_cmd = parse_file logical_fp in
     * let real_cmd = parse_file concrete_fp in
     * if d then begin
     *     Printf.printf "Logical: \n %s \n%!" (Ast.string_of_cmd (Synthesis.base_translation log_cmd));
     *     Printf.printf "Real : \n %s \n %!" (Ast.string_of_cmd (Synthesis.base_translation real_cmd));
     *     Printf.printf "ADD1 to Logical:\n %s\n%!" (Ast.string_of_cmd (snd (Synthesis.add_symbolic_row name log_cmd)));
     *     Printf.printf "ADD<N to Concrete:\n %s \n%!" (Ast.string_of_cmd (Synthesis.concretely_instrument n real_cmd))
     *   end else ignore(Synthesis.check_add n name log_cmd real_cmd) *)
end

let editCheck : Command.t =
  Command.basic_spec
    ~summary: "Check whether there exist `n` outputs that implement an edit"
    EditCheck.spec
    EditCheck.run

module EditSynth = struct
  let spec = Command.Spec.(
      empty
      +> flag "-d" no_arg ~doc:"dry-run-mode: Output the Z3 query"
      +> flag "-n" (required int) ~doc:"The number of functions"
      +> flag "-t" (required string) ~doc:"The logical table to edit"
      +> anon ("logical" %: string)
      +> anon ("concrete" %: string))


  let run (_:bool) _ _ _ _ () =
    Printf.printf "deprecated"
    (* let log_cmd = parse_file logical_fp in
     * let real_cmd = parse_file concrete_fp in
     * if d then begin
     *     let open Ast in
     *     let open Synthesis in
     *     let (_, logAdd1_cmd) = add_symbolic_row name log_cmd in
     *     Printf.printf "Logical: \n %s \n%!" (string_of_cmd (base_translation log_cmd));
     *     Printf.printf "Real : \n %s \n %!" (string_of_cmd (base_translation real_cmd));
     *     Printf.printf "ADD1 to Logical:\n %s\n%!" (string_of_cmd logAdd1_cmd);
     *     Printf.printf "ADD<N to Concrete:\n %s \n%!"
     *       (string_of_cmd (edit_synth_real_inst ~numFs ~setSize:1 real_cmd))
     *   end
     * else
     *   ignore(Synthesis.synth_add numFs name log_cmd real_cmd) *)
end

let editSynth : Command.t =
  Command.basic_spec
    ~summary: "Produce a function that transforms edits"
    EditSynth.spec
    EditSynth.run



module Bench = struct
  let spec = Command.Spec.(
      empty
      +> anon ("varsize" %: int)
      +> anon ("num_tables" %: int)
      +> anon ("max_inserts" %: int)
      +> flag "-w" no_arg ~doc:"perform widening")


  let run varsize num_tables max_inserts widening () =
    ignore(Benchmark.reorder_benchmark varsize num_tables max_inserts widening : Tables.Edit.t list)
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
      +> flag "-p" no_arg ~doc:"show_result_at_end"
      +> flag "-DEBUG" no_arg ~doc:"print debugging statements"
      +> flag "-data" (required string) ~doc:"the input log"
      +> flag "-fastcx" no_arg ~doc:"Generate counterexample quickly")


  let run gas widening do_slice monotonic interactive print debug data_fp fastcx () =
    let res = Benchmark.basic_onf_ipv4
              Parameters.({widening;do_slice;gas;monotonic;interactive;debug;fastcx})
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
    ; ("edit-check", editCheck)
    ; ("edit-synth", editSynth)
    ; ("bench", benchmark)
    ; ("onf", onf)
    ; ("of", of_bench)
    ; ("ex", running_example)
    ; ("meta", meta)
    ; ("wp", wp_cmd)]

let () = Command.run main
