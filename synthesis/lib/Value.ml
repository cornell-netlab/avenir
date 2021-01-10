open Util

type t = Bigint.t Sized.t

let big_make (i, sz) =
  let open Bigint in
  if i < zero then
    failwith @@ Printf.sprintf "[Value.make] Negative integers not representable, tried %s" (Hex.to_string i)
  else if of_int sz >= zero && i > max_int sz then
    failwith @@ Printf.sprintf "[Value.make] int %s larger than 2^%d-1" (Hex.to_string i) sz
  else
    Sized.make i sz

let make (i,sz) =
  big_make (Bigint.of_int_exn i,sz)

let str_make (s,sz) =
  big_make (Bigint.of_string s, sz)

let to_string v : string =
  Sized.to_string ~f:Bigint.Hex.to_string v

let to_bmv2_string v : string =
  Sized.get v
  |> Bigint.Hex.to_string

let to_sexp_string v = to_string v

let to_smt v = Z3.Smtlib.bbv (Sized.get v) (Sized.size v)

let ueq = Sized.map ~f:Bigint.(=)
let eq = Sized.map2 ~f:Bigint.(=)

let leq = Sized.map2 ~f:Bigint.(<=)

let zero sz = make (0,sz)

let get_bigint v = Sized.get v

let get_int_exn v = get_bigint v |> Bigint.to_int_exn

let size v = Sized.size v
let same_size v v' = size v = size v'


let add =
  Sized.fmap2s ~f:(fun x x' sz -> Bigint.(x + x' % max_int sz))

let sat_add =
  Sized.fmap2s ~f:(fun x x' sz -> Bigint.(min (x + x') (max_int sz)))

let multiply =
  Sized.fmap2s ~f:(fun x x' sz ->  Bigint.(x * x' % max_int sz))

let subtract =
  Sized.fmap2s ~f:(fun x x' sz -> Bigint.(x - x' % max_int sz))

let sat_subtract =
  Sized.fmap2 ~f:(fun x x' -> Bigint.(max (x - x') zero))

let mask : t -> t -> t = Sized.fmap2 ~f:Bigint.(land)

let xor = Sized.fmap2 ~f:Bigint.(lxor)

let or_ = Sized.fmap2 ~f:Bigint.(lor)

let shl =
  Sized.fmap2 ~f:(fun x x' -> Bigint.(shift_left x @@ to_int_exn x'))

(* [sized_mask sz] computes a bigint of [2^{sz} - 1]*)
let sized_mask sz =
  let open Bigint in
  (pow (of_int 2) (of_int sz)) - one

let cast w v =
  (* v & (2^w -1)#w *)
  Sized.fmap v ~f:(fun x ->
      let open Bigint in
      x land sized_mask w
    )
  |> Sized.resize w

let resize v sz = Sized.resize sz v

let slice hi lo v =
  Sized.maps v
    ~f:(fun x sz ->
      if hi > sz || lo > sz then
        failwith "index out of range"
      else
        let sz' = hi - lo in
        assert(sz' > 0);
        let x' = Bigint.((shift_right x lo) land sized_mask sz') in
        Sized.make x' sz'
    )

let concat =
  Sized.map2s2
    ~f:(fun lx lsz rx rsz ->
      Sized.make
        Bigint.((shift_left lx rsz) + rx)
        (lsz + rsz))

