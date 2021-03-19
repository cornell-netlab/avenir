open Core
open Util

type t = Value.t StringMap.t

let to_string m =
  StringMap.fold m ~init:"" ~f:(fun ~key ~data acc ->
      Printf.sprintf "%s\n\t%s |---> %s" acc key (Value.to_string data) )
  |> Printf.sprintf "{%s\n}"

let of_smt_model = Z3ModelExtractor.of_smt_model

let extend_multi_model multi m =
  StringMap.merge multi m ~f:(fun ~key:_ -> function
    | `Left l -> Some l
    | `Right _ -> None
    | `Both (l, r) -> Some (r :: l) )

let aggregate =
  List.fold ~init:StringMap.empty ~f:(fun acc m ->
      StringMap.merge acc m ~f:(fun ~key -> function
        | `Left l -> Some l
        | `Right r -> Some r
        | `Both (l, r) when Value.eq l r -> Some l
        | `Both (l, r) ->
            failwith
            @@ Printf.sprintf
                 "[Model.aggregate] conflicting values %s is both %s and %s"
                 key (Value.to_string l) (Value.to_string r) ) )

let join m1 m2 = aggregate [m1; m2]

let diff =
  StringMap.merge ~f:(fun ~key:_ -> function
    | `Both (l, r) when Stdlib.(l <> r) -> Some (l, r)
    | _ -> None )

let right_union : t -> t -> t =
  StringMap.merge ~f:(fun ~key:_ -> function
    | `Left x | `Right x | `Both (_, x) -> Some x )

let intersect =
  StringMap.merge ~f:(fun ~key:_ -> function
    | `Both (v1, v2) when Value.equals v1 v2 -> Some v1
    | _ -> None )

let perturb = StringMap.map ~f:(fun v -> Value.(random (size v) ~exc:[v]))

let proj_packet_holes =
  StringMap.filter_keys ~f:(fun h ->
      not
        ( Hole.is_add_row_hole h || Hole.is_delete_hole h
        || Hole.is_which_act_hole h ) )

(**UTILITIES INHERITED FROM STRINGMAP**)

let fold = StringMap.fold

let set = StringMap.set

let empty = StringMap.empty

let equal = StringMap.equal Stdlib.( = )

let find = StringMap.find

let of_alist_exn = StringMap.of_alist_exn

let iteri = StringMap.iteri

let to_strmap m = m

let of_strmap m = m
