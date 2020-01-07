open Core
open Ast

module StringMap = Map.Make (String)

type t = value1 StringMap.t

type located = t * (int option)

let string__packet (p : t) =
  (StringMap.fold ~f:(fun ~key:k ~data:v acc -> acc ^ k ^ "," ^ (string_of_value1 v) ^ ",") p ~init:"(") ^ ")"

let set_field (pkt : t) (field : string) (v : value1) : t  =
  StringMap.set pkt ~key:field ~data:v
    
let get_val (pkt : t) (field : string) : value1 =
  match StringMap.find pkt field with
    | None -> failwith ("UseBeforeDef error" ^ field)
    | Some v -> v     
              
let rec set_field_of_expr1 (pkt : t) (field : string) (e : expr1) : t =
  let binop op e e'=
    let eVal = get_val (set_field_of_expr1 pkt field e) field in
    let eVal' = get_val (set_field_of_expr1 pkt field e') field in
    set_field pkt field (op eVal eVal')
  in
  match e with
  | Value1 v -> set_field pkt field v
  | Var1 (x,_) ->
     get_val pkt x
     |> set_field pkt field
  | Hole1 _ ->
     failwith "Packets cannot have holes in them"
  | Plus  (e, e') -> binop add_values1 e e'
  | Times (e, e') -> binop multiply_values1 e e'
  | Minus (e, e') -> binop subtract_values1 e e'
  | Tuple es ->
       List.map es ~f:(fun e -> get_val (set_field_of_expr1 pkt field e) field)
       |> VTuple
       |> set_field pkt field
                  
                

let init_field_to_random bound pkt (f,sz) =
  set_field pkt f (Int (Random.int bound, sz))

let rec init_field_to_value_in (values : value1 list) pkt (f, sz) =
  match values with
  | [] -> init_field_to_random 10000000 pkt (f,sz)
  | _ -> 
     let i = Random.int (List.length values) in
     let vi = List.nth_exn values i in
     if size_of_value1 vi = sz then
       set_field pkt f vi
     else
       init_field_to_value_in (List.filter values ~f:(fun x -> x <> vi)) pkt (f, sz)

let to_test (pkt : t) =
  StringMap.fold pkt ~init:True
    ~f:(fun ~key ~data test ->
      if key = "loc" then
        test
      else 
        Var1 (key,size_of_value1 data) %=% Value1 data
        %&% test
    )

let empty = StringMap.empty

let equal (pkt:t) (pkt':t) = StringMap.equal (=) pkt pkt'
  
let generate ?bound:(bound=10000000) ?values:(values=([] : value1 list))  (vars : (string * size) list) =
  match values with
  | [] ->
    List.fold vars ~init:empty ~f:(init_field_to_random bound)
  | _ ->
    List.fold vars ~init:empty ~f:(init_field_to_value_in values)

let from_CE (model : value1 StringMap.t) : t =
  StringMap.fold model ~init:empty
    ~f:(fun ~key ~data pkt ->
        if String.get key 0 = '$'
        then pkt
        else set_field pkt key data)

        
        
let mk_packet_from_list (assoc : (string * value1) list) : t =
  List.fold assoc ~init:empty
    ~f:(fun pkt (f, v) -> set_field pkt f v)
