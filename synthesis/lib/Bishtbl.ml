open Core

type t = {fwd: (string, string) Hashtbl.t; bck: (string, string) Hashtbl.t}

let make () : t =
  let size = 1000 in
  { fwd= Hashtbl.create ~growth_allowed:true ~size (module String)
  ; bck= Hashtbl.create ~growth_allowed:true ~size (module String) }

let add bht key data : unit =
  Hashtbl.add_exn bht.fwd ~key ~data ;
  Hashtbl.add_exn bht.bck ~key:data ~data:key

let get bht ~key ~default : string =
  match Hashtbl.find bht.fwd key with
  | Some s -> s
  | None ->
      let tgt = default () in
      add bht key tgt ; tgt

let get_back bht ~key : string = Hashtbl.find_exn bht.bck key
