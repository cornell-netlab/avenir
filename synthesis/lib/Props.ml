open Core
open Ast

let props : FSM.t list ref = ref []       

(* A List of properties *)
let true_prop _ =
  let open FSM in 
  mkfsm 0 [{id = 0; accept = true; trans = [(True,0)]}]

(* fabric.p4 properties *)
let defaultonly _ =
  let open FSM in
  let defonly =
    let open Hole in
    (add_row_hole "routing_v6" %=% mkVInt(1,1))
    %=>% bigand [
             which_act_hole "routing_v6" 1 %=% mkVInt(0,1);
           ]
  in
  mkfsm 0 [
      {id = 0;
       accept = true;
       trans = [ defonly, 0 ;
                 Hole.add_row_hole "simple" %=% mkVInt(1,1), 0;
                 Hole.add_row_hole "ingress_port_vlan" %=% mkVInt(1,1), 0;
                 Hole.add_row_hole "fwd_classifier" %=% mkVInt(1,1),0]}
    ]

(* ADD,simple,1017#32,17#9,0
 * ADD,simple,1018#32,18#9,0
 * ADD,simple,1010#32,10#9,0
 * ADD,simple,1012#32,12#9,0
 * ADD,simple,1011#32,11#9,0
 * ADD,simple,1008#32,8#9,0
 * ADD,simple,1003#32,3#9,0
 * ADD,simple,1019#32,19#9,0 *)

let join_conditions _ =
  let open FSM in
  let ok_simple_key v =
    let open Hole in
    bigand [     
        add_row_hole "simple" %=% mkVInt(1,1);
        Hole(match_hole_exact "simple" "fabric_metadata.next_id",32) %=% mkVInt(v,32)
      ]
  in
  let check_v6_rule v =
    let open Hole in
    bigand [
        add_row_hole "routing_v6" %=% mkVInt(1,1);
        action_data_hole "routing_v6" 0 "next_id" 32 %=% mkVInt(v,32)
      ]
  in
  let create_join_fsm v =
    mkfsm 0 [
        {id = 0; (* Haven't seen simple rule yet *)
         accept = false;
         trans = [ok_simple_key v, 1;                  
                  Hole.add_row_hole "simple" %=% mkVInt(1,1), 0;
                  Hole.add_row_hole "ingress_port_vlan" %=% mkVInt(1,1), 0;
                  Hole.add_row_hole "fwd_classifier" %=% mkVInt(1,1),0;
                  Hole.(bigand [add_row_hole "routing_v6" %=% mkVInt(1,1);
                                action_data_hole "routing_v6" 0 "next_id" 32 %<>% mkVInt(v,32)]), 0;
        ]};
        {id = 1; (* Have seen simple rule *)
         accept = false;
         trans = [check_v6_rule v, 2;
                  True, 1]};
        {id = 2;
         accept = true;
         trans = [True, 2]}
    ]        
  in
  [1018; 1017; 1010;  1012;  1011; 1008; 1003; 1019]
  |> List.map ~f:create_join_fsm

        
(* end fabric.p4 properties*)

let no_eth _ =
  let open FSM in
  mkfsm 0 [
      {id = 0;
       accept = true;
       trans = [Hole.add_row_hole "ipv4_forward" %=% mkVInt(1,1), 0]
      }
    ]        
              

(* simple_router_properties *)


(* End list of properties *)
                                 
       
let get_property (name : string) : FSM.t list =
  match name with
  | "trivial" ->
     Printf.printf "Initializing trivial property\n%!";
     [true_prop ()]
  | "defaults" | "def" | "defonly" ->
     Printf.printf "Initializing noop is default only in ipv6 table";
     [defaultonly ()]
  | "joins" ->
     let joins = join_conditions () in
     Printf.printf "Initializing %d join rules\n%!" (List.length joins);
     joins
  | "noeth" ->
     Printf.printf "Initializing eth-less property\n%!";
     [no_eth ()]
  | _ -> []


let init_properties (names : string list) : unit =
  props := List.bind names ~f:get_property
           
let init_properties_s (names : string) : unit =
  String.split names ~on:','
  |> init_properties
  
  
let reset () =
  props := List.map !props ~f:(FSM.reset)
