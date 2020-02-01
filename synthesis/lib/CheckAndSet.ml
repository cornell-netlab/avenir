open Core
open Ast
open Util
open Manip
open Synthesis


let rec map_holes_expr e ~f =
  let binop op e1 e2 = op (map_holes_expr e1 ~f) (map_holes_expr e2 ~f) in
  match e with
  | Value1 _ | Var1 _ -> e
  | Hole1(h,sz) -> Hole1 (f h, sz)
  | Plus (e1,e2) -> binop (fun e1' e2' -> Plus (e1',e2')) e1 e2
  | Minus(e1,e2) -> binop (fun e1' e2' -> Minus (e1',e2')) e1 e2
  | Times(e1,e2) -> binop (fun e1' e2' -> Times (e1',e2')) e1 e2
  | Tuple _ -> failwith "tuples are deprecated"

let rec map_holes_test b ~f =
  let binope op e1 e2 = op (map_holes_expr e1 ~f) (map_holes_expr e2 ~f) in
  let binopb op b1 b2 = op (map_holes_test b1 ~f) (map_holes_test b2 ~f) in
  match b with
  | True | False -> b
  | Eq(e1,e2) -> binope (%=%) e1 e2
  | Le(e1,e2) -> binope (%<=%) e1 e2
  | And(e1,e2) -> binopb (%&%) e1 e2
  | Or(e1,e2) -> binopb (%+%) e1 e2
  | Impl(e1,e2) -> binopb (%=>%) e1 e2
  | Iff (e1,e2) -> binopb (%<=>%) e1 e2
  | Neg(e) -> !%(map_holes_test b ~f)
  | Member _ -> failwith "membership is deprecated"
  

let rec map_holes c ~f =
  match c with
  | Skip -> Skip
  | Assign(x,e) -> x %<-% map_holes_expr e ~f
  | Seq(c1,c2) -> map_holes c1 ~f %:% map_holes c2 ~f
  | Select(typ,cs) ->
     List.map cs ~f:(fun (b,c) -> map_holes_test b ~f, map_holes c ~f)
     |> mkSelect typ
  | Assert t -> map_holes_test t ~f |> Assert
  | Assume t -> map_holes_test t ~f |> Assume
  | While _ | Apply _ -> Printf.sprintf "no holes allowed in %s" (string_of_cmd c)
                         |> failwith

let rec freshen_holes_expr e holes_so_far =
  let freshen_hole h sz i =
    Printf.printf "replacing %s with %s\n%!" h (freshen h sz i |> fst);
    Hole1(freshen h sz i), StringMap.set holes_so_far ~key:h ~data:(i + 1)
  in
  let binop op e1 e2 =
    let (e1', holes_so_far') = freshen_holes_expr e1 holes_so_far in
    let (e2', holes_so_far'') = freshen_holes_expr e2 holes_so_far' in
    (op e1' e2', holes_so_far'')
  in
  match e with
  | Value1 _ | Var1 _ -> (e, holes_so_far)
  | Hole1 (h,sz) ->
     begin match StringMap.find holes_so_far h with
     | Some i -> freshen_hole h sz i
     | None -> freshen_hole h sz 0
     end
  | Plus(e1,e2) -> binop (fun e e' -> Plus(e,e')) e1 e2
  | Minus(e1,e2) -> binop (fun e e' -> Minus(e,e')) e1 e2
  | Times(e1,e2) -> binop (fun e e' -> Times(e,e')) e1 e2                                                  
  | Tuple _ -> failwith "tuples are deprecated"

              
let rec freshen_holes c holes_so_far =
  match c with
  | Skip -> (Skip, holes_so_far)
  | Assign(f, e) ->
     let (e', new_holes) = freshen_holes_expr e holes_so_far in
     (f %<-% e', new_holes)
  | Seq(c1,c2) ->
     let c1', new_holes1 = freshen_holes c1 holes_so_far in
     let c2', new_holes2 = freshen_holes c2 new_holes1 in
     (c1' %:% c2', new_holes2)
  | Select _ -> failwith "cannot branch in actions"
  | Assume _ | Assert _ | While _ | Apply _ ->
     Printf.sprintf "Cannot handle %s " (string_of_cmd c)
     |> failwith
     
              
       


let common_indices ksmall kbig : int option list =
  List.fold ksmall ~init:[] ~f:(fun acc k ->
      acc @ List.return (
                match List.findi kbig ~f:(fun _ -> (=) k ) with
                | None ->
                   Printf.printf "Couldn't find %s in logical table\n%!" (fst k);
                   None
                | Some (i,_) ->
                   Printf.printf "Found %s in logical table at index %d\n%!" (fst k) i;
                   Some i                     
              )
    )

let project keys idxs ms =
  List.foldi idxs ~init:[]
    ~f:(fun k acc idx ->
      match idx with
      | Some i ->
         begin match List.nth ms i with
         | Some m -> acc @ [m]
         | None -> Printf.sprintf "couldn't find %d in list of length %d" i (List.length ms)
                   |> failwith
         end

      | None ->
         begin match List.nth keys k with
         | Some (_, sz) ->
            acc @ [Between(0,pow 2 sz,sz)]
         | None -> Printf.sprintf "couldn't find %d in list of length %d" k (List.length keys)
                   |> failwith
         end
    )
  
       
let project_classifier keys table_keys rows =
  let projection = common_indices keys table_keys in
  List.map rows ~f:(fun (matches,_,_) ->
      Printf.printf "Projecting matches %s\n%!" (string_of_matches matches);
      project keys projection matches
      |> encode_matches keys
    )
  |> List.map ~f:(fun t ->
         Printf.printf "Match %s\n%!" (string_of_test t);
         t
       )

(* Skip must be one of the actions *)           
let instrumented_action_for_hole (h,_) actions =
  let act_size = pow 2 (List.length actions) - 1 in
  List.(mapi actions ~f:(fun i (data, act)->
            let act''= holify (data >>| fst) act
                       |> map_holes ~f:(fun hole -> Printf.sprintf "%s_%s" hole h) in
            Hole1("?act_for_path_"^h, act_size) %=% mkVInt(i,act_size),
            act''
  ))
  |> mkOrdered
        
    
                 
           
let populate_action_table name keys actions holes : cmd =
  match keys with
  | [k,sz] ->
     StringMap.fold holes ~init:[]
       ~f:(fun ~key:base_hole ~data:max_idx acc ->
         acc @ List.init max_idx ~f:(fun idx ->
                   let hole, _ =  freshen base_hole sz idx in
                   (Var1(k,sz) %=% Hole1(hole, sz),
                    instrumented_action_for_hole (hole,sz) actions
                   )
                 )
       ) |> mkOrdered
  | _ -> Printf.sprintf "%s must have one key since it is the action table. It has %d" name (List.length keys)
         |> failwith
           


let rec compute_entries (table_keys : (string * int) list) (rows:row list) (action_table : string)
          (pipeline : cmd) (holes : int StringMap.t) : cmd * (int StringMap.t)   =
  match pipeline with
  | Skip -> (Skip, holes)
  | Assert _ | Assume _ | Select _ ->
     failwith "don't know how to handle conditionals"
  | Assign _ ->
     failwith "don't know how to handle assignments"
  | Seq(c1,c2) ->
     let c1', holes' = compute_entries table_keys rows action_table c1 holes in
     let c2', holes'' = compute_entries table_keys rows action_table c2 holes' in
     (c1' %:% c2', holes'')
  | While(b,c) -> failwith "while loops are hard"
  | Apply(name, keys, actions, Skip) ->
     if name = action_table
     then (populate_action_table name keys actions holes, holes)
     else if List.length actions <> 1
     then failwith "too many actions"
     else
       (* let _ = Printf.printf "Projecting the classifier\n%!" in *)
       let (data, act) = List.hd_exn actions in
       let selects, holes =
         project_classifier keys table_keys  rows
         |> List.(fold ~init:([],holes)
                    ~f:(fun (ss,holes) test ->
                      Printf.printf "freshening entry %s -> %s\n%!" (string_of_test test) (string_of_cmd act);
                      let act', holes' = holify (data>>|fst) act
                                         |> Fun.flip freshen_holes holes in
                      (ss @ [test, act'], holes'))
            ) in
       (mkOrdered selects, holes)

  | Apply(name,_,_,_) ->
     Printf.sprintf "default action not Skip in table %s\n" name
     |> failwith 


let test_instrumentation _ =
  let instr,_ = compute_entries
    ["inport", 9; "ipv4", 32]
    [ [Exact(9,9); Exact(2, 32)], [], 0
    ; [Exact(3,9); Exact(255, 32)], [], 0
    ]
    "next"
    (sequence [
         Apply("ingress", ["inport", 9], [["n", 32], "next"%<-%Var1("n",32)], Skip)
       ; Apply("l3_fwd", ["ipv4", 32], [["n",32], "next" %<-% Var1("n",32)],Skip)
       ; Apply("next", ["m",32], [["o",9], sequence["outport"%<-% Var1("o", 32); "drop" %<-% mkVInt(0,1)]
                                 ;[], "drop"%<-% mkVInt(1,1)], Skip)
    ])
    StringMap.empty
  in
  Printf.printf "%s\n%!" (string_of_cmd instr)
  
  


let run x = test_instrumentation x
