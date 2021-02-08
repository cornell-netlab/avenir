open Core
open Util

let bigint_to_yojson (bi: Bigint.t) : Yojson.Safe.t = `Intlit (Bigint.to_string bi)

let bigint_of_yojson (j: Yojson.Safe.t) : Bigint.t Ppx_deriving_yojson_runtime.error_or =
  match j with
  | `Intlit s -> Result.Ok (Bigint.of_string s)
  | _ -> Result.Error "t"

type t = (Bigint.t [@to_yojson bigint_to_yojson] [@of_yojson bigint_of_yojson]) Sized.t [@@deriving yojson, sexp, compare]

let big_make (i, sz) =
  let open Bigint in
  if i < zero then
    failwith
    @@ Printf.sprintf
         "[Value.make] Negative integers not representable, tried %s"
         (Hex.to_string i)
  else if of_int sz >= zero && i > max_int sz then
    failwith
    @@ Printf.sprintf "[Value.make] int %s larger than 2^%d-1"
         (Hex.to_string i) sz
  else Sized.make i sz

let make (i, sz) = big_make (Bigint.of_int_exn i, sz)

let str_make (s, sz) = big_make (Bigint.of_string s, sz)

let unsafe_big_make (i, sz) = Sized.make i sz

let unsafe_make (i, sz) = unsafe_big_make (Bigint.of_int_exn i, sz)

let to_string v : string = Sized.to_string ~f:Bigint.Hex.to_string v

let to_bmv2_string v : string = Sized.get v |> Bigint.Hex.to_string

let to_sexp_string v = to_string v

let to_smt v = Z3.Smtlib.bbv (Sized.get v) (Sized.size v)

let big_eq = Sized.map ~f:Bigint.( = )

let eq = Sized.map2 ~f:Bigint.( = )

let neq v v' = not (eq v v')

let equals =
  Sized.map2s2 ~f:(fun x1 sz1 x2 sz2 -> Bigint.(x1 = x2) && sz1 = sz2)

let leq = Sized.map2 ~f:Bigint.( <= )

let zero sz = make (0, sz)

let get_bigint v = Sized.get v

let get_int_exn v = get_bigint v |> Bigint.to_int_exn

let size v = Sized.size v

let same_size v v' = size v = size v'

let wrap sz x = Bigint.(x % max_int sz)

let add = Sized.fmap2s ~f:(fun x x' sz -> Bigint.(x + x') |> wrap sz)

let sat_add =
  Sized.fmap2s ~f:(fun x x' sz ->
      Printf.printf "min (%s + %s = %s) and %s\n%!" (Bigint.to_string x)
        (Bigint.to_string x')
        Bigint.(to_string (x + x'))
        (Bigint.to_string (max_int sz)) ;
      Bigint.(min (x + x') (max_int sz)))

let multiply = Sized.fmap2s ~f:(fun x x' sz -> Bigint.(x * x') |> wrap sz)

let subtract = Sized.fmap2s ~f:(fun x x' sz -> Bigint.(x - x') |> wrap sz)

let sat_subtract = Sized.fmap2 ~f:(fun x x' -> Bigint.(max (x - x') zero))

let mask : t -> t -> t = Sized.fmap2 ~f:Bigint.( land )

let xor = Sized.fmap2 ~f:Bigint.( lxor )

let or_ = Sized.fmap2 ~f:Bigint.( lor )

(* [sized_mask sz] computes a bigint of [2^{sz} - 1]*)
let sized_mask sz =
  let open Bigint in
  pow (of_int 2) (of_int sz) - one

let shl =
  Sized.fmap2s ~f:(fun x x' sz ->
      Bigint.(shift_left x (to_int_exn x') land sized_mask sz))

let cast w v =
  (* v & (2^w -1)#w *)
  Sized.fmap v ~f:(fun x ->
      let open Bigint in
      x land sized_mask w)
  |> Sized.resize w

let resize v sz = Sized.resize sz v

let slice hi lo v =
  Sized.maps v ~f:(fun x sz ->
      if hi > sz || lo > sz then failwith "index out of range"
      else
        let sz' = hi - lo in
        assert (sz' > 0) ;
        let x' = Bigint.(shift_right x lo land sized_mask sz') in
        Sized.make x' sz')

let concat =
  Sized.map2s2 ~f:(fun lx lsz rx rsz ->
      Sized.make Bigint.(shift_left lx rsz + rx) (lsz + rsz))

let rec random_not_in ?(gas = 1000) exc upper =
  if gas <= 0 then failwith "Random Generation failed, out of gas"
  else
    let r = Random.int upper in
    if List.exists exc ~f:(( = ) r) then
      random_not_in ~gas:(gas - 1) exc upper
    else r

let random ?(lo = 0) ?(exc = []) sz =
  let exc_ints =
    List.filter_map exc ~f:(fun v ->
        let i = get_int_exn v in
        if i < lo then None else Some i)
  in
  if sz <= 0 then failwith @@ Printf.sprintf "Bad bitwidth %d" sz
  else
    let m = max_int sz |> Bigint.to_int_exn in
    if sz > m then
      failwith @@ Printf.sprintf "lo bound %d is to large for width %d" lo m
    else
      let r = random_not_in exc_ints (m - lo) + lo in
      make (r, sz)
