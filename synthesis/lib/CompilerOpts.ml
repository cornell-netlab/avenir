let optimize fvs cmd =
  let f cmd =
    cmd
    |> ConstantProp.propogate
    |> DeadCode.elim_vars
    |> StaticSlicing.static_slice fvs
  in
  let rec fix cmd_old cmd =
    if Stdlib.(cmd_old = cmd) then
      cmd
    else fix cmd (f cmd)
  in
  fix Skip cmd
