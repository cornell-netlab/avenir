open Core

let pause ?(prompt = "") b =
  if b then (
    Printf.printf "%s [Press Enter to Continue]\n%!" prompt ;
    ignore (Stdio.In_channel.(input_char stdin) : char option) )
