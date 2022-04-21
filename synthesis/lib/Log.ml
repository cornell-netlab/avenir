open Core

type t = {warn: bool; info: bool; debug: bool; z3: bool; ecache: bool}

let level =
  ref {warn= false; info= false; debug= false; z3= false; ecache= false}

let set_warn _ = level := {!level with warn= true}

let set_info _ = level := {!level with info= true}

let set_debug _ = level := {!level with debug= true}

let set_z3 _ = level := {!level with z3= true}

let set_ecache _ = level := {!level with ecache= true}

let keyword strs s f = if List.exists strs ~f:(String.( = ) s) then f ()

let set_level strs =
  keyword strs "debug" set_debug ;
  keyword strs "warn" set_warn ;
  keyword strs "info" set_info ;
  keyword strs "z3" set_info ;
  keyword strs "ecache" set_ecache ;
  if List.length strs = 1 then
    String.iter (List.hd_exn strs) ~f:(fun c ->
        let open Char in
        if c = 'd' || c = 'D' then set_debug () ;
        if c = 'w' || c = 'W' then set_warn () ;
        if c = 'i' || c = 'I' then set_info () ;
        if c = 'z' || c = 'Z' then set_z3 () ;
        if c = 'e' || c = 'E' then set_ecache () )

let print msg s = Format.printf "%s%s\n%!" msg (Lazy.force s)

let colorize colors s = ANSITerminal.sprintf colors "%s" s

let blue = colorize [ANSITerminal.blue]

let red = colorize [ANSITerminal.red]

let yellow = colorize [ANSITerminal.yellow]

let green = colorize [ANSITerminal.green]

let warn s = if !level.warn then print (yellow "[WARNING] ") s

let info s = if !level.info then print (blue "[INFO] ") s

let debug s = if !level.debug then print (red "[DEBUG] ") s

let z3 s = if !level.z3 then print (green "[Z3] ") s

let ecache s = if !level.ecache then print (green "[ECache] ") s

let id_print ~s ~p x =
  p (lazy (s x)) ;
  x

(* Demo logging *)
let abs_log_fn : string option ref = ref None
let tgt_log_fn : string option ref = ref None  
  
let abs_log_file (abs_log : string option) : unit =
  abs_log_fn := abs_log 
    
let tgt_log_file (tgt_log : string option) : unit =
  tgt_log_fn := tgt_log

let append fn ~data =
  let open Out_channel in
  let outc= create ~append:true fn in
  output_string outc data;
  newline outc;
  close outc

let log_abs data =
  match !abs_log_fn with
  | None -> ()
  | Some fn ->
     append fn ~data

let log_tgt data =
  match !tgt_log_fn with
  | None -> ()
  | Some fn ->
     append fn ~data
  
