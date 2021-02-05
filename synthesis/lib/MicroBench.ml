open Core
open RandomGen

let counter = ref 0

type args =
  { bits: int
  ; keys: int
  ; tbls: int
  ; vars: int
  ; data: int
  ; acts: int  
  ; edts: int }

let args_to_string args =
  Printf.sprintf "%d,%d,%d,%d,%d,%d,%d" args.bits args.keys args.tbls args.vars args.data args.acts args.edts

let time_synthesizer params fvs log phys log_edits =
  let data = ProfData.zero () in
  let problem =
    Problem.make ~log ~phys ~log_edits ~fvs ~log_inst:Instance.empty
      ~phys_inst:Instance.empty
  in
  let st = Time.now () in
  let res =
    try
      Synthesis.cegis_math_sequence params data problem
    with
    | _ -> None in 
  let time = Time.(Span.(diff (now ()) st |> to_ms)) in
  (res, time)

let append_line file string =
  let outc = Out_channel.create ~append:true file in
  Out_channel.output_string outc string ;
  Out_channel.newline outc ;
  Out_channel.flush outc ;
  Out_channel.close outc

let log_header file = "bits,keys,tables,vars,data,acts,edits,succeed,time" |> append_line file

let log_row total file args (res, time) =
  Printf.printf "Finished case %d of %d\n%!" !counter total ;
  counter := !counter + 1 ;
  Printf.sprintf "%s,%b,%f" (args_to_string args) (Option.is_some res) time
  |> append_line file

let run_experiment_case total params file args =
  if false then
    let obt, fvs = Obt.gen false args.bits args.keys args.vars in
    let pip = Pipe.gen false args.bits args.keys args.vars in
    let edits =
      Obt.rand_edits false args.bits args.keys args.vars args.edts
    in
    time_synthesizer params fvs obt pip edits |> log_row total file args
  else
    let obt, fvs, acts =
      Obt.gen_big args.bits args.keys args.vars args.data args.acts
    in
    let pip =
      Pipe.gen_big args.bits args.keys args.tbls args.vars args.data args.acts
    in
    let edits = Obt.rand_big_edits acts args.bits args.keys args.edts in
    time_synthesizer params fvs obt pip edits |> log_row total file args

let product bs ks vs ts ds as_ es =
  let open List.Let_syntax in
  let%bind bits = bs in
  let%bind keys = ks in
  let%bind vars = vs in
  let%bind tbls = ts in
  let%bind data = ds in
  let%bind acts = as_ in
  let%bind edts = es in
  if acts < tbls then []
  else
    return { bits
           ; keys
           ; vars
           ; tbls
           ; data
           ; acts
           ; edts }

let run_experiment params file (lo : args) (hi : args) =
  Random.init 4153 ;
  log_header file ;
  (* compute inclusive ranges for all arguments*)
  let bitrange = Util.range_inc lo.bits hi.bits in
  let keyrange = Util.range_inc lo.keys hi.keys in
  let varrange = Util.range_inc lo.vars hi.vars in
  let tblrange = Util.range_inc lo.tbls hi.tbls in
  let dtarange = Util.range_inc lo.data hi.data in
  let actrange = Util.range_inc lo.acts hi.acts in
  let edtrange = Util.range_inc lo.edts hi.edts in
  let space = product bitrange keyrange varrange tblrange dtarange actrange edtrange in
  List.iter space ~f:(fun args ->
      EAbstr.make () ;
      run_experiment_case (List.length space) (Parameters.restart_timer params) file args)
