open Core

module StringMap = Map.Make (String)
module IntMap = Map.Make(Int)

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
  let _ : unit = Random.init ((Time_now.nanoseconds_since_unix_epoch ())
                       |> Base.Int63.to_int_trunc)
  in
  let max_list = List.fold_left domain ~init:(0) ~f:(fun oldmax curr -> max oldmax curr) in
  let rec random_int_nin_rec _ = 
    let r = if max_list = 0 then 0 else Random.int max_list in
    match List.findi domain ~f:(fun _ x -> x = r) with
    | None ->  r
    | Some _ -> random_int_nin_rec ()
  in
  random_int_nin_rec ()
                  
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

(*computes x^y*)                       
let pow (x : int) (y : int) : int =
  float_of_int(x) ** float_of_int(y) |> int_of_float
                                
let log2 (x : int) : int =
  int_of_float(Float.round_up(Core.log (float_of_int x) /. Core.log (float_of_int 2)))

let get_singleton_exn = function
  | [] -> failwith "Cannot get a singleton from an empty list"
  | [x] -> x
  | _ -> failwith "Cannot get a singleton from a list with more than one element"


let liftO2 f a_opt b_opt =
  let open Option in
  a_opt >>= fun a ->
  b_opt >>= fun b ->
  return (f a b)

let liftL2 f al bl =
  let open List in
  al >>= fun a ->
  bl >>= fun b ->
  return (f a b)

let mkCons x xs = x :: xs
          
let rec range_ex lo hi =
  if lo = hi then []
  else
  lo :: range_ex (lo + 1) hi

let inj_l x y = (y, x)
let inj_r x y = (x, y)                  

let flip f x y = f y x

let off_by_one i j =  max i j - min i j = 1                   

(*===========================================================================*)
(* Graphviz                                                                  *)
(*===========================================================================*)

let compile_dot ?(format="pdf") ?(engine="dot") ?(title=engine) data : string =
  let output_file = Filename.temp_file (title ^ "_") ("." ^ format) in
  let to_dot = Unix.open_process_out (sprintf "dot -T%s -o %s" format output_file) in
  Out_channel.output_string to_dot data;
  Out_channel.close to_dot;
  ignore (Unix.close_process_out to_dot : Core.Unix.Exit_or_signal.t);
  output_file

(* let show_dot ?format ?title ?engine data : unit =
 *   compile_dot ?format ?title ?engine data
 *   |> Open.in_default_app
 *   |> ignore
 * 
 * let show_dot_file ?format ?title ?engine file : unit =
 *   In_channel.read_all file
 *   |> show_dot ?format ?title ?engine                   *)


let lfill c n str =
  Printf.sprintf "%s%s"
    (String.make (n - String.length str) c)
    (str)
                 

(** Bytes and Stuff **)
let rec bytes_of_hex_string str : char list =
  let str = String.chop_prefix_exn str "0x" in
  if String.length str = 0 then [] else
    let first_byte = String.prefix str 2 in
    let rest = String.drop_prefix str 2 in
    match int_of_string_opt ("0x" ^ first_byte) with
    | None ->
       Printf.sprintf "Couldn't parse %s as string" first_byte |> failwith
    | Some i -> (Char.of_int_exn i) :: bytes_of_hex_string ("0x"^rest)



let bits_of_byte b =
  let bi = Char.to_int b in
  Printf.sprintf "%d%d%d%d%d%d%d%d"
    ((bi asr 7) land 1)
    ((bi asr 6) land 1)
    ((bi asr 5) land 1)
    ((bi asr 4) land 1)
    ((bi asr 3) land 1)
    ((bi asr 2) land 1)
    ((bi asr 1) land 1)
    ((bi asr 0) land 1)
    
  
let rec byte_list_to_bit_string b : string =
  match b with
    | [] -> ""
    | x::xs -> let bs = bits_of_byte x in
               bs ^ byte_list_to_bit_string xs
       

let bytes_to_bit_string b = Bytes.to_list b |> byte_list_to_bit_string
  
                                                           
let bytes_to_hex_string bytes =
  let rec char_list_to_hex_string = function
    | [] -> ""
    | b::bs ->
       Printf.sprintf "%s%s" (Char.to_int b |> Printf.sprintf "%x" |> lfill '0' 2) (char_list_to_hex_string bs)
  in
  Bytes.to_list bytes
  |> char_list_to_hex_string
  |> (^) "0x"


let bytes_to_ipv6_string bytes =
  let rec char_list_to_ipv6 = function
    | [] -> ""
    | b1::b2::bs ->
       let s1 = Printf.sprintf "%x" (Char.to_int b1) |> lfill '0' 2 in
       let s2 = Printf.sprintf "%x" (Char.to_int b2) |> lfill '0' 2 in
       Printf.sprintf "%s%s:%s" s1 s2
         (char_list_to_ipv6 bs)
    | _ -> failwith "odd number of bytes"
  in
  Bytes.to_list bytes
  |> char_list_to_ipv6
  |> String.chop_suffix_exn ~suffix:":"



let bit_string_to_decimal bs  =
  let rec euc_div x' r base bs =
    assert (r < 10);
    match bs with
    | [] -> (String.chop_prefix_exn x' ~prefix:"0", r)
    | (b::bs') ->
       let r = 2 * r + if Stdlib.(=) b '0' then 0 else 1 in
       if r >= base
       then euc_div (x' ^ "1") (r - base) base bs'
       else euc_div (x' ^ "0") r base bs'
  in
  let rec loop result bs =
    if String.equal bs "0" then result else
      let (bs',r) = euc_div "" 0 10 (String.to_list bs) in
      
      loop (Printf.sprintf "%d%s" r result) bs'
  in
  loop "" bs
       
  
    
      
let uncurry f (x,y) = f x y
