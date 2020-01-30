open Core
module Ast = Motley.Ast
module Parser = Motley.Parser
module Lexer = Motley.Lexer
module Prover = Motley.Prover
module Synthesis = Motley.Synthesis
module Encode = Motley.Encode
module Manip = Motley.Manip
module Benchmark = Motley.Benchmark


let parse_file (filename : string) : Ast.cmd =
  let cts = In_channel.read_all filename in
  let lexbuf = Lexing.from_string cts in  
  Parser.main Lexer.tokens lexbuf

  
(* module Solver = struct
 *   let spec = Command.Spec.(
 *       empty
 *       +> flag "-M" (optional string) ~doc: "<file> model for logical program"
 *       +> anon ("logical" %: string)
 *       +> anon ("real" %: string))
 * 
 *   let run model logical real () =
 *     let log_cmd = parse_file logical in
 *     let real_cmd = parse_file real in
 *     match model with
 *     | None -> Synthesis.synthesize log_cmd real_cmd |> ignore
 *     | Some m ->
 *        let log_cmd = Encode.apply_model_from_file log_cmd m in
 *        Synthesis.synthesize (Encode.apply_model_from_file log_cmd m) real_cmd |> ignore
 * end
 * 
 * 
 * let synthesize_cmd : Command.t = 
 *   Command.basic_spec
 *     ~summary:"Compute modifications to real program to implement logical program "
 *     Solver.spec
 *     Solver.run *)


module Encoder = struct
  let spec = Command.Spec.(
      empty
      +> flag "-I" (listed string) ~doc:"<dir> add directory to include search path"
      +> flag "-v" no_arg ~doc:"verbose mode"
      +> anon ("p4_file" %: string))

  let run include_dirs verbose p4_file () =    
    ignore(Encode.encode_from_p4 include_dirs p4_file verbose)
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
      +> anon ("max_inserts" %: int))
   

  let run varsize num_tables max_inserts () =
    Benchmark.reorder_benchmark varsize num_tables max_inserts
end
    

let benchmark : Command.t =
  Command.basic_spec
    ~summary: "Run some benchmarks"
    Bench.spec
    Bench.run             


module ONF = struct
  let spec = Command.Spec.(empty)
  

  let run () =
    Benchmark.basic_onf_ipv4 () |> ignore
end
    

let onf : Command.t =
  Command.basic_spec
    ~summary: "Run the onf benchmark"
    ONF.spec
    ONF.run

module RunningExample = struct
  let spec = Command.Spec.(empty)
  

  let run () =
    Benchmark.running_example () |> ignore
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
    let _ = Printf.printf "PROGRAM: %s \n%!" (Ast.string_of_cmd cmd) in
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
  
let main : Command.t =
  Command.group
    ~summary:"Invokes the specified Motley Command"
    [ (*("inst-synth", synthesize_cmd)
    ;*) ("encode-p4", encode_cmd)
    ; ("edit-check", editCheck)
    ; ("edit-synth", editSynth)
    ; ("bench", benchmark)
    ; ("onf", onf)
    ; ("ex", running_example)
    ; ("wp", wp_cmd)]
    
let () = Command.run main


	
