(* Applies f to every element in the list and then combines them pairwise using c.
 * Roughly equivalent to [map exprs f |> fold ~init ~f:c], except that [init] is optional
 * If it is provided, then if the input list is empty, it simply returns the provided [init] value
 * However, if no [~init] is provided (or it has the value [None]), then [concatMap] 
 * assumes that there is at least one element in the input list. If there is not, then it fails.
 *)
exception EmptyList of string
let rec concatMap ?init:(init=None) ~f:(f: 'a -> 'b)  ~c:(c : 'b -> 'b -> 'b) (xs : 'a list) : 'b =
  match xs, init with
  | [], None -> raise (EmptyList "called concatMap on an empty list")
  | [], Some y -> y
  | [x], None -> f x
  | [x], Some y -> c (f x) y
  | x::xs ,_ -> c (f x) (concatMap ~init ~f ~c xs)
