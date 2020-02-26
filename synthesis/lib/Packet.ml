open Core
open Ast
open Util

module StringMap = Map.Make (String)

type t = value StringMap.t

type located = t * (int option)

let string__packet (p : t) =
  (StringMap.fold ~f:(fun ~key:k ~data:v acc -> acc ^ k ^ "," ^ (string_of_value v) ^ ",") p ~init:"(") ^ ")"

let set_field (pkt : t) (field : string) (v : value) : t  =
  StringMap.set pkt ~key:field ~data:v
    
let get_val (pkt : t) (field : string) : value =
  match StringMap.find pkt field with
    | None -> failwith ("UseBeforeDef error " ^ field ^ " packet is " ^ string__packet pkt)
    | Some v -> v     
              
let rec set_field_of_expr (pkt : t) (field : string) (e : expr) : t =
  let binop op e e'=
    let eVal = get_val (set_field_of_expr pkt field e) field in
    let eVal' = get_val (set_field_of_expr pkt field e') field in
    set_field pkt field (op eVal eVal')
  in
  match e with
  | Value v -> set_field pkt field v
  | Var (x,_) ->
     get_val pkt x
     |> set_field pkt field
  | Hole _ ->
     failwith "Packets cannot have holes in them"
  | Plus  (e, e') -> binop add_values e e'
  | Times (e, e') -> binop multiply_values e e'
  | Minus (e, e') -> binop subtract_values e e'
   
                

let init_field_to_random bound pkt (f,sz) =
  set_field pkt f (Int (Random.int bound |> Bigint.of_int_exn, sz))

let rec init_field_to_value_in (values : value list) pkt (f, sz) =
  match values with
  | [] -> init_field_to_random 10000000 pkt (f,sz)
  | _ -> 
     let i = Random.int (List.length values) in
     let vi = List.nth_exn values i in
     if size_of_value vi = sz then
       set_field pkt f vi
     else
       init_field_to_value_in (List.filter values ~f:(fun x -> x <> vi)) pkt (f, sz)

let to_test ?fvs:(fvs = []) ?random_fill:(random_fill=false) (pkt : t) =
  List.fold fvs ~init:True
    ~f:(fun acc (x,sz) ->
      acc %&% (
          match StringMap.find pkt x with
          | None ->
             if random_fill then
               Var(x,sz) %=% mkVInt(Random.int (pow 2 sz),sz)
             else                  
               True
          | Some v ->
             Var(x, sz) %=% Value(v)))


let test_of_wide ?fvs:(fvs = []) wide =
  StringMap.fold wide ~init:True
    ~f:(fun ~key ~data:(lo,hi,sz) test ->
      if key <> "loc" && List.exists fvs ~f:(fun (x,_) -> key = x) then
        (if lo = hi
         then Var (key, sz) %=% mkVInt(lo,sz)
         else (mkVInt(lo, sz) %<=% Var(key,sz)) %&% (Var (key, sz) %<=% mkVInt(hi,sz))
        ) %&% test
      else ( test ))    
   
let empty = StringMap.empty

let equal (pkt:t) (pkt':t) = StringMap.equal (=) pkt pkt'
                                                                                          
let generate ?bound:(bound=10000000) ?values:(values=([] : value list))  (vars : (string * size) list) =
  match values with
  | [] ->
    List.fold vars ~init:empty ~f:(init_field_to_random bound)
  | _ ->
    List.fold vars ~init:empty ~f:(init_field_to_value_in values)

let is_symbolic = String.is_suffix ~suffix:"_SYMBOLIC"
let symbolize str =
  if is_symbolic str then str else
    str ^ "_SYMBOLIC"
let unsymbolize = String.substr_replace_all ~pattern:"_SYMBOLIC" ~with_:""
              
let from_CE (model : value StringMap.t) : t =
  StringMap.fold model ~init:empty
    ~f:(fun ~key ~data pkt ->
      let key = String.split key ~on:('!') |> List.hd_exn in
      if is_symbolic key && not(String.is_prefix key ~prefix:"?ActIn")
      then pkt
      else
        let key = unsymbolize key in
        set_field pkt key data)

let un_SSA (pkt : t) : t =
  StringMap.fold pkt ~init:empty
    ~f:(fun ~key ~data acc_pkt ->
      match String.rsplit2 key ~on:'$' with
      | None ->
         StringMap.set acc_pkt ~key:(key) ~data
      | Some (key', i) ->
         if int_of_string i = 0
         then
           StringMap.set acc_pkt ~key:(key') ~data
         else acc_pkt
    )
                 
        
let mk_packet_from_list (assoc : (string * value) list) : t =
  List.fold assoc ~init:empty
    ~f:(fun pkt (f, v) -> set_field pkt f v)
