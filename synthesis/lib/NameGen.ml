open Core

type t = string ref

let make () = ref "a"

let incr c = Char.to_int c + 1 |> Char.of_int_exn

let rec get_fresh_name (gen : t) () =
  let freshname = !gen in
  if String.(freshname = "") then begin
      gen := "a";
      get_fresh_name gen ()
    end
  else
    let c = String.prefix !gen 1 |> Char.of_string in
    if Char.(c = 'z') then
      gen := String.make (String.length freshname + 1) 'a'
    else
      gen := Printf.sprintf "%c%s" (incr c) (String.drop_prefix freshname 1);
    freshname
