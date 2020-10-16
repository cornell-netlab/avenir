open Core

type t = (Time.t * Time.Span.t) option


let create dur = (Time.now (), dur)

let start_secs dur =
  create (Time.Span.of_sec dur)

let start =
  Fn.flip Option.(>>|) start_secs

let timed_out =
  let open Time in
  let open Span in
  function
  | Some (st,dur) -> dur < diff (now()) st
  | _ -> false

let restart (t : t) : t =
  Option.(t >>| fun (_,dur) -> create dur)


let union t1 t2 =
  match t1, t2 with
  | None, None ->
     None
  | Some t, None | None, Some t ->
     Some t
  | Some (st1,dur1), Some (st2,dur2) ->
     Some(Time.min st1 st2, Time.Span.max dur1 dur2)
