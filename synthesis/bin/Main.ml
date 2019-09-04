open Core
module Ast = Motley.Ast
module Parser = Motley.Parser
module Lexer = Motley.Lexer
module Prover = Motley.Prover
module Synthesis = Motley.Synthesis
module Encode = Motley.Encode
module Manip = Motley.Manip


let parse_file (filename : string) : Ast.cmd =
  let cts = In_channel.read_all filename in
  let lexbuf = Lexing.from_string cts in  
  Parser.main Lexer.tokens lexbuf

  
module Solver = struct
  let spec = Command.Spec.(
      empty
      +> flag "-M" (optional string) ~doc: "<file> model for logical program"
      +> anon ("logical" %: string)
      +> anon ("real" %: string))

  let run model logical real () =
    let log_cmd = parse_file logical in
    let real_cmd = parse_file real in
    match model with
    | None -> Synthesis.synthesize log_cmd real_cmd
    | Some m ->
       let log_cmd = Encode.apply_model_from_file log_cmd m in
       Synthesis.synthesize (Encode.apply_model_from_file log_cmd m) real_cmd        
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
    ignore(Encode.encode_from_p4 include_dirs p4_file verbose)
end

let encode_cmd : Command.t =
  Command.basic_spec
    ~summary:"Convert P4 programs into their GCL-While interpretation"
    Encoder.spec
    Encoder.run

let main : Command.t =
  Command.group
    ~summary:"Invokes the specified Motley Command"
    [ ("synthesize", synthesize_cmd)
    ; ("encodep4", encode_cmd) ]
    
let () = Command.run main


	
