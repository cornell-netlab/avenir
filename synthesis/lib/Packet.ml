open Core
open Ast

module StringMap = Map.Make (String)

type t = int StringMap.t

type located = t * (int option)

let string_of_packet (p) = (StringMap.fold ~f:(fun ~key:k ~data:v acc -> acc ^ k ^ "," ^ (string_of_int v) ^ ",") p ~init:"(") ^ ")"

let set_field pkt field i  =
  StringMap.set pkt ~key:field ~data:i
    
let get_val pkt field =
  match StringMap.find pkt field with
    | None -> failwith ("UseBeforeDef error" ^ field)
    | Some v -> v
    
let set_field_of_value pkt field value =
  match value with
    | Int i -> set_field pkt field i
    | Var v ->
       get_val pkt v
       |> set_field pkt field
    | Hole _ ->
       failwith "Packets cannot have holes in them"

let init_field_to_random bound pkt v =
  set_field pkt v (Random.int bound) 

let to_test pkt =
  StringMap.fold pkt ~init:True
    ~f:(fun ~key ~data test ->
      if key = "loc" then
        test
      else 
        Var key %=% Int data
        %&% test
    )

let empty = StringMap.empty

let equal (pkt:t) (pkt':t) = StringMap.equal (=) pkt pkt'
  

let generate ?bound:(bound=10000000) vars =
    List.fold vars ~init:empty ~f:(init_field_to_random bound)

let from_CE (model : value StringMap.t) : t =
  StringMap.fold model ~init:empty
    ~f:(fun ~key ~data pkt -> set_field_of_value pkt key data)

        
        
