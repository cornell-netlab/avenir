open Core
module Ast = Motley.Ast
module Parser = Motley.Parser
module Lexer = Motley.Lexer
module Prover = Motley.Prover
module Synthesis = Motley.Synthesis


let parse_file (filename : string) : Ast.cmd =
  let cts = In_channel.read_all filename in
  let lexbuf = Lexing.from_string cts in  
  Parser.main Lexer.tokens lexbuf

  
module Solver = struct
  let spec = Command.Spec.(
      empty
      +> anon ("logical" %: string)
      +> anon ("real" %: string))

  let run logical real () =
    let log_cmd = parse_file logical in
    let real_cmd = parse_file real in
    Synthesis.synthesize log_cmd real_cmd
           
end
   
let command =
  Command.basic_spec
    ~summary:"Compute modifications to real program to implement logical program "
    Solver.spec
    Solver.run
    
let () = Command.run command


	
