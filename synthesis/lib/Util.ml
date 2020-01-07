open Core

(* Applies f to every element in the list and then combines them pairwise using c.
 * Roughly equivalent to [map exprs f |> fold ~init ~f:c], except that [init] is optional
 * If it is provided, then if the input list is empty, it simply returns the provided [init] value
 * However, if no [~init] is provided (or it has the value [None]), then [concatMap] 
 * assumes that there is at least one element in the input list. If there is not, then it fails.
 *)
exception EmptyList of string
let rec concatMap ?init:(init=None) ~f:(f: 'a -> 'b)  ~c:(c : 'b -> 'b -> 'b) (xs : 'a list) : 'b =
  match xs, init with
  | [], None -> failwith ("called concatMap on an empty list")
  | [], Some y -> y
  | [x], None -> f x
  | [x], Some y -> c (f x) y
  | x::xs ,_ -> c (f x) (concatMap ~init ~f ~c xs)


(** Computes a random number not in the list supplied*)
let random_int_nin domain =
  let _ = Random.init ((Time_now.nanoseconds_since_unix_epoch ())
                       |> Base.Int63.to_int_trunc)
  in
  let max_list = List.fold_left ~init:(0) ~f:(fun oldmax curr -> max oldmax curr) in
  let rec random_int_nin_rec domain = 
    let r = Random.int (max_list domain) in
    match List.findi domain ~f:(fun _ x -> x = r) with
    | None ->  r
    | Some _ -> random_int_nin_rec domain
  in
  random_int_nin_rec domain
                  
(** constructs a pair from the arguments *)
let mkPair a b = (a, b)

(** constructs a reversed pair from the arguments *)
let mkRevPair b a = (a, b)


let rec difference (xs : 'a list) (ys : 'a list) : 'a list =
  match xs,ys with
  | [],_ | _,[] -> xs
  | (x::xs'),_ ->
     if List.exists ys ~f:((=) x) then
       difference xs' ys
     else
       x :: difference xs' ys

let log2 (x : int) : int =
  int_of_float(Core.log (float_of_int x) /. Core.log (float_of_int 2))

let get_singleton_exn = function
  | [] -> failwith "Cannot get a singleton from an empty list"
  | [x] -> x
  | _ -> failwith "Cannot get a singleton from a list with more than one element"
