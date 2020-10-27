open Core
open Util
open Ast
open Tables

type state_idx = int

type state = {
    id : state_idx;
    accept : bool;
    trans : (test * state_idx) list
  }

let get_next (c : cmd) (q : state) (e : Edit.t) =
  let m = Edit.to_model c e in
  List.find_map q.trans
    ~f:(fun (cond, state_idx) ->
      let cond' = Manip.substV ~holes:true cond m
                  |> varify_all_test in
      Option.some_if
        (Prover.is_valid Parameters.default cond')
        (state_idx)
    )


type t = {
    states : state IntMap.t;
    config : state_idx;
  }


let mkfsm start (ss : state list) : t =
  let states =
    List.fold ss ~init:IntMap.empty ~f:(fun state_map state ->
        match IntMap.add state_map ~key:state.id ~data:state with
        | `Ok state_map -> state_map
        | `Duplicate ->
           Printf.sprintf "Node identifiers must be unique: %d is duplicated" state.id
           |> failwith
      )
  in
  { states; config = start}


let get_state (fsm : t) =
  match IntMap.find fsm.states fsm.config with
  | None -> failwith @@ Printf.sprintf "Couldn't find fsm state %d" fsm.config
  | Some st -> st

(* Step forward if possible and report the Category.t of result *)
let step (fsm : t) (c : cmd) (e : Edit.t) : t Category.t =
  let st = get_state fsm in
  match get_next c st e with
  | None ->
     Reject fsm
  | Some next ->
     let fsm' = {fsm with config = next} in
     let st' = get_state fsm' in
     if st'.accept then
       Accept fsm'
     else
       Unknown fsm'



let label_trace (fsm : t) (c : cmd) (trace : Edit.t list) : ( unit Category.t list)  =
  List.fold trace ~init:(fsm,[])
    ~f:(fun (fsm,tr) e ->
      let fsm = step fsm c e in
      let res = Category.keep fsm () in
      (Category.get fsm, tr @ [res])
    )
  |> snd
