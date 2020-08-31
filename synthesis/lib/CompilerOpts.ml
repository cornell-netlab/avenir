
let optimize fvs cmd =
  let open Ast in
  let i = ref 0 in
  let f cmd =
    let debug = false in
    Interactive.pause debug
      ~prompt:(Printf.sprintf "after %d optimizing passes: \n %s\n--/%d iter, size is %s--\n%!" (!i) (string_of_cmd cmd) (!i) (Bigint.to_string @@ num_paths cmd));

    let cmd' = ConstantProp.propogate cmd in
    Interactive.pause debug
      ~prompt:(Printf.sprintf "After Constant Propogation\n %s \n===%d iters, size is %s\n%!" (string_of_cmd cmd') (!i) (Bigint.to_string @@ num_paths cmd'));

    let cmd'' = DeadCode.elim_vars fvs cmd' in
    Interactive.pause debug
      ~prompt:(Printf.sprintf "After Dead Code\n %s \n+++%d iters, size is %s+++\n%!"(string_of_cmd cmd'') (!i) (Bigint.to_string @@ num_paths cmd''));
    i := !i +1;

    StaticSlicing.static_slice fvs cmd''
  in
  let rec fix cmd_old cmd =
    if Stdlib.(cmd_old = cmd) then
      cmd
    else fix cmd (f cmd)
  in
  fix Skip cmd




let passive_optimize out_pkt passive_cmd =
  ConstantProp.passive_propogate_fix out_pkt passive_cmd