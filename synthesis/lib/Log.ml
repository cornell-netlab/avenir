open Core

type t = {warn: bool; info: bool; debug: bool; z3: bool}

let level = ref {warn= false; info= false; debug= false; z3= false}

let set_warn _ = level := {!level with warn= true}

let set_info _ = level := {!level with info= true}

let set_debug _ = level := {!level with debug= true}

let set_z3 _ = level := {!level with z3= true}

let keyword strs s f = if List.exists strs ~f:(String.( = ) s) then f ()

let set_level strs =
  keyword strs "debug" set_debug ;
  keyword strs "warn" set_warn ;
  keyword strs "info" set_info ;
  keyword strs "z3" set_info ;
  if List.length strs = 1 then
    String.iter (List.hd_exn strs) ~f:(fun c ->
        let open Char in
        if c = 'd' || c = 'D' then set_debug () ;
        if c = 'w' || c = 'W' then set_warn () ;
        if c = 'i' || c = 'I' then set_info () ;
        if c = 'z' || c = 'Z' then set_z3 ())

let print msg s = Format.printf "%s%s\n%!" msg (Lazy.force s)

let colorize colors s = ANSITerminal.sprintf colors "%s" s

let blue = colorize [ANSITerminal.blue]

let red = colorize [ANSITerminal.red]

let yellow = colorize [ANSITerminal.yellow]

let warn s = if !level.warn then print (yellow "[WARNING] ") s

let info s = if !level.info then print (blue "[INFO] ") s

let debug s = if !level.debug then print (red "[DEBUG] ") s

let z3 s = if !level.z3 then print (blue "[Z3] ") s
