open Core
open Util
open Ast

type t = value StringMap.t

let to_string = string_of_map

let of_smt_model (lst : (Z3.Smtlib.identifier * Z3.Smtlib.term) list) =
  let name_vals : (string * value) list =
    (List.map lst ~f:(fun (Id id, x) ->
         let id = match String.index id '@' with
           | None -> id
           | Some index -> String.drop_prefix id index in
         match x with
         | Z3.Smtlib.BitVec (n, w) -> let value =
                                     Int (Bigint.of_int n, w) in id, value
         | Z3.Smtlib.BigBitVec (n, w) -> let value =
                                        Int (n, w) in id, value
         | Z3.Smtlib.Int i -> let value =
                             Int (Bigint.of_int i, Int.max_value) in id, value
         | _ -> raise (Failure "not a supported model")))
  in
  StringMap.of_alist_exn name_vals


let extend_multi_model multi m =
  StringMap.merge multi m ~f:(fun ~key:_ -> function
      | `Left l -> Some l
      | `Right _ -> None
      | `Both (l,r) -> Some (r::l)
    )
let aggregate =
  List.fold ~init:StringMap.empty
    ~f:(fun acc m ->
      StringMap.merge acc m ~f:(fun ~key -> function
          | `Left l -> Some l
          | `Right r -> Some r
          | `Both (l,r) when veq l r -> Some l
          | `Both (l,r) ->
             failwith @@
               Printf.sprintf "[Model.aggregate] conflicting values %s is both %s and %s"
                 key (string_of_value l) (string_of_value r)))

let join m1 m2 = aggregate [m1;m2]

let diff = StringMap.merge ~f:(fun ~key:_ -> function
               | `Both (l,r) when Stdlib.(l <> r) -> Some (l,r)
               | _ -> None
             )

(**UTILITIES INHERITED FROM STRINGMAP**)

let merge = StringMap.merge
let fold = StringMap.fold
let set = StringMap.set
let empty = StringMap.empty
let equal = StringMap.equal Stdlib.(=)
let find = StringMap.find
let of_alist_exn = StringMap.of_alist_exn
let iteri = StringMap.iteri
