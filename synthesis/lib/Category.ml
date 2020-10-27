type 'a t =
  | Accept of 'a
  | Reject of 'a
  | Unknown of 'a

let keep (cat : 'a t) (x : 'b) : 'b t =
  match cat with
  | Accept _ -> Accept x
  | Reject _ -> Reject x
  | Unknown _ -> Unknown x

let get (cat : 'a t) : 'a =
  match cat with
  | Accept x
    | Reject x
    | Unknown x ->
     x

let map ~f cat =
  keep cat (f (get cat))

let to_string ~to_string cat =
  let to_s c s = Printf.sprintf "%s %s" c s in
  match map ~f:to_string cat with
  | Accept s -> to_s "Accept" s
  | Reject s -> to_s "Reject" s
  | Unknown s -> to_s "??????" s

let equal ~equal c1 c2 =
  match c1,c2 with
  | Accept x1, Accept x2
    | Reject x1, Reject x2
    | Unknown x1, Unknown x2 ->
     equal x1 x2
  | _, _ -> false
