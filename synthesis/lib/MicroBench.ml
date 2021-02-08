open Core
open RandomGen

let counter = ref 0

type args = {bits: int; keys: int; outs: int; edts: int}

let args_to_string args =
  Printf.sprintf "%d,%d,%d,%d" args.bits args.keys args.outs args.edts

let time_synthesizer params fvs log phys log_edits =
  let data = ProfData.zero () in
  let problem =
    Problem.make ~log ~phys ~log_edits ~fvs ~log_inst:Instance.empty
      ~phys_inst:Instance.empty
  in
  let st = Time.now () in
  let res = Synthesis.cegis_math_sequence params data problem in
  let time = Time.(Span.(diff (now ()) st |> to_ms)) in
  (res, time)

let append_line file string =
  let outc = Out_channel.create ~append:true file in
  Out_channel.output_string outc string ;
  Out_channel.newline outc ;
  Out_channel.flush outc

let log_header file = "bits,keys,outs,edits,succeed,time" |> append_line file

let log_row file args (res, time) =
  Printf.printf "Finished case %d\n%!" !counter ;
  counter := !counter + 1 ;
  Printf.sprintf "%s,%b,%f" (args_to_string args) (Option.is_some res) time
  |> append_line file

let run_experiment_case params file args =
  let obt, fvs = Obt.gen args.bits args.keys args.outs in
  let pip = Pipe.gen args.bits args.keys args.outs in
  let edits = Obt.rand_edits args.bits args.keys args.outs args.edts in
  time_synthesizer params fvs obt pip edits |> log_row file args

let product bs ks os es =
  let open List.Let_syntax in
  let%bind bits = bs in
  let%bind keys = ks in
  let%bind outs = os in
  let%map edts = es in
  {bits; keys; outs; edts}

let run_experiment params file (lo : args) (hi : args) =
  Random.init 4153 ;
  log_header file ;
  (* compute inclusive ranges for all arguments*)
  let bitrange = Util.range_inc lo.bits hi.bits in
  let keyrange = Util.range_inc lo.keys hi.keys in
  let outrange = Util.range_inc lo.outs hi.outs in
  let edtrange = Util.range_inc lo.edts hi.edts in
  let space = product bitrange keyrange outrange edtrange in
  List.iter space ~f:(fun args ->
      EAbstr.make () ;
      run_experiment_case (Parameters.restart_timer params) file args)
