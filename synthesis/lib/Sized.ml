type 'a t = 'a * int

let make x size = (x, size)

let get (x, _) = x

let size (_, size) = size

let to_string ~f (x, sz) = Printf.sprintf "%s#%d" (f x) sz

let map ~f (x, _) = f x

let maps ~f (x, sz) = f x sz

let fmap ~f (x, sz) = make (f x) sz

let fmaps ~f (x, sz) = make (f x sz) sz

let map2 ~f (x1, sz1) (x2, sz2) =
  if sz1 = sz2 then f x1 x2
  else
    Printf.sprintf "[combine] Different sizes: %d and %d" sz1 sz2 |> failwith

let map2s2 ~f (x1, sz1) (x2, sz2) = f x1 sz1 x2 sz2

let fmap2 ~f (x1, sz1) (x2, sz2) =
  if sz1 = sz2 then make (f x1 x2) sz1
  else
    Printf.sprintf "[fmap2] Different sizes: %d and %d" sz1 sz2 |> failwith

let fmap2s ~f (x1, sz1) (x2, sz2) =
  if sz1 = sz2 then make (f x1 x2 sz1) sz1
  else
    Printf.sprintf "[fmap2s] Different sizes: %d and %d" sz1 sz2 |> failwith

let resize sz' (x, _) = (x, sz')
