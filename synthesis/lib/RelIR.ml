(**
 * Intermediate representation Based on a Relational interpretation of Tables
 *)

open Core
open Ast
open Prover       
open Synthesis
              
type field = string

type action = cmd
type action_set = action list
type action_seq = action list

let print_action (a : action) : unit = Printf.printf "%s" (string_of_cmd a)
                         
type condition =
  | SemFD of {act : size list;
              inputs : size list;
              outputs : string list;
              range : size list
             }
  | FD of size list * size list   
  | Test of test

let string_of_ilist = List.fold ~init:"" ~f:(Printf.sprintf "%s %i")
              
let string_of_condition =
  function
  | Test t -> string_of_test t
  | FD (dom, rng) ->
     Printf.sprintf "(%s) -> (%s)"
       (string_of_ilist dom)       
       (string_of_ilist rng)
  | SemFD {act; inputs; outputs; range} ->
     Printf.sprintf "[|%s|](%s).(%s) -> %s"
       (string_of_ilist act)
       (string_of_ilist inputs)
       (List.reduce_exn outputs ~f:(fun a b -> a ^ " " ^ b))
       (string_of_ilist range)
     
                    
                         
type schema = 
  { keys : (field * size * size) list; (* name, min, max *)
    actions : action_set list;
    constraints : condition list
  }

type key_match =
  | Int of int
  | Range of {lo: int; hi : int}

let print_key_match (km : key_match) : unit =
  match km with
  | Int i -> Printf.printf "%i" i
  | Range r -> Printf.printf "[%i;%i]" r.lo r.hi
    
type instance =
  { keys : key_match list list;
    actions : action_seq list
  }

let print_instance (r : instance) : unit =
  let rows = List.zip_exn r.keys r.actions in
  Printf.printf "------------------------------------------\\\n%!";
  List.iter rows
    ~f:(fun (keys, actions) ->
      List.iter keys ~f:(fun km -> print_key_match km; Printf.printf " ";);
      Printf.printf " -> ";
      List.iter actions ~f:(fun act -> print_action act; Printf.printf ";");
      Printf.printf "\n%!"
    );
  Printf.printf "------------------------------------------/\n%!"
      


let ids_to_fields (t : schema) (ids : size list) : string list =
  List.map ids ~f:(fun idx -> 
      let (a,_,_) = List.nth_exn t.keys idx in
      a
    )
                
let well_formed (r : instance) : bool =
  List.length r.keys = List.length r.actions

let proj rows set =
  List.map rows ~f:(fun (keys, acts) ->
      (List.filteri keys ~f:(fun idx _ ->
           List.exists set ~f:((=) idx))
      , List.filteri acts ~f:(fun idx _ ->
            List.exists set ~f:((=) (idx + List.length keys)))))

let does_induce_function dom rng : bool =
  let rel = List.zip_exn dom rng in
  List.cartesian_product rel rel
  |> List.exists
       ~f:(fun ((dom1, rng1), (dom2, rng2)) ->
         dom1 = dom2 && rng1 <> rng2)
  |> not
           
let satFD r dom rng : bool=
  let rows = List.zip_exn r.keys r.actions in
  does_induce_function (proj rows dom) (proj rows rng)
  
let eval_acts_rel (fields : string list) (values_acts : (key_match list * action_seq) list) =
  List.map values_acts
    ~f:(fun (values, acts) ->
      let _ = Printf.printf "%d actions \n%!" (List.length acts) in
      let act = List.reduce_exn acts ~f:(%:%) in
      List.map values ~f:(function
          | Int x -> Ast.Int (x,0)
          | Range _ -> failwith "Can't Handle Ranges")
      |> List.zip_exn fields
      |> Packet.mk_packet_from_list 
      |> Semantics.eval_act act
    )

let extract (pkts : Packet.t list) (fields : string list) : key_match list list =
  List.map pkts
    ~f:(fun pkt ->
      List.map fields
        ~f:(fun f -> match Packet.get_val pkt f with
                     | Ast.Int (f,_) -> Int f
                     | _ -> failwith "Cannot Handle Tuples"))
    
(* Precondition: act_ids must only be actions, and in_ids and out_ids
   must only be field indices *)
let satSemFD (r : instance) (t : schema) (act_ids : size list) (in_ids : size list) (out_fields : string list) (rng_ids : size list) : bool =
  let _ = Printf.printf "in_ids ";
          List.iter (in_ids) ~f:(Printf.printf "%i ");
          Printf.printf "\n%!" in
  let _ = Printf.printf "act_ids ";
          List.iter (act_ids) ~f:(Printf.printf "%i ");
          Printf.printf "\n%!" in
  let rows = List.zip_exn r.keys r.actions in
  let values_acts = proj rows (in_ids @ act_ids) in
  let fields = ids_to_fields t in_ids in
  let rng = proj rows rng_ids in
  let image = eval_acts_rel fields values_acts in
  let dom = extract image out_fields in
  does_induce_function dom rng 
  
                       
let rec evalRel (r : key_match list) (t : schema) (e : expr1) =   
  match e with
  | Value1 v -> v
  | Var1 (name, sz) ->
     begin match List.findi t.keys ~f:(fun _ (name', _, _) -> name = name') with
     | None -> failwith "Error could not evaluate column name"
     | Some (i, _) -> match List.nth_exn r i with
                      | Int x -> Int (x,sz)
                      | Range _ -> failwith "I dont want to symbolically evaluate with a range"
     end
  | Plus (e,e') ->
     begin match evalRel r t e, evalRel r t e' with
     | Int (x, sz), Int (y,_) -> Int (x + y, sz)
     | _, _ -> failwith "Dont know how to eval tuples"
     end
  | Times (e, e') ->
     begin match evalRel r t e, evalRel r t e' with
     | Int (x, sz), Int (y,_) -> Int (x * y, sz)
     | _, _ -> failwith "Dont know how to eval tuples"
     end
  | Minus (e, e') ->
     begin match evalRel r t e, evalRel r t e' with
     | Int (x, sz), Int (y,_) -> Int (x - y, sz)
     | _, _ -> failwith "Dont know how to eval tuples"
     end
  | _ -> failwith "IDK how to use tuples"


let rec satOneRow keys t b  =
  match b with
  | True -> true
  | False -> false
  | Neg b -> not (satOneRow keys t b)
  | And (a,b) -> satOneRow keys t a && satOneRow keys t b
  | Or (a,b) -> satOneRow keys t a && satOneRow keys t b
  | Eq (x, y) -> evalRel keys t x = evalRel keys t y
  | Lt (x, y) -> evalRel keys t x = evalRel keys t y
  | LocEq _ -> failwith "Error :: Don't know how to evaluate location in a table"
  | Member _ -> failwith " Error (Unimplemented) :: Membership Evaluation"
                         
                  
let satisfies (r : instance) (t : schema) : bool =
  let satOne (r : instance) t c =
    match c with
    | SemFD {act; inputs; outputs; range}
      -> satSemFD r t act inputs outputs range
    | FD (dom_ids, rng_ids) -> satFD r dom_ids rng_ids
    | Test b ->
       List.exists r.keys
         ~f:(fun keys -> not (satOneRow keys t b))
       |> not
  in
  List.fold t.constraints ~init:true
    ~f:(fun acc c -> if satOne r t c then acc else( Printf.printf "Violated %s\n%!" (string_of_condition c); false))
    
let is_inst_of (r : instance) (table : schema) : bool =
  not (List.exists r.keys
         ~f:(fun row_keys ->
           List.length row_keys <> List.length table.keys))
  && not (List.exists r.actions
            ~f:(fun row_acts -> List.length row_acts <> List.length table.keys))
  && List.fold r.actions ~init:true
       ~f:(fun rslt act_row ->
         List.foldi act_row ~init:true
           ~f:(fun idx rslt' act ->
             List.exists (List.nth_exn table.actions idx) ~f:((=) act)
             && rslt')
         && rslt)
  && satisfies r table


let inject_keys (tbl_log : schema) (tbl_phys : schema) (key_log : key_match list) : key_match list =
  List.fold tbl_phys.keys ~init:[]
    ~f:(fun  acc_row (name, lo, hi) ->
      acc_row @ 
        [match List.findi tbl_log.keys ~f:(fun _ (name', _, _) -> name = name') with
         | None -> Range {lo; hi}
         | Some (idx', _) ->
            List.nth_exn key_log idx'
        ]
    )
  

let rec list_cross (cols : 'a list list) : 'a list list  =
  match cols with
  | [] -> [[]]
  | [_] -> List.transpose_exn cols
  | col::cols ->
     list_cross cols
     |> List.cartesian_product col
     |> List.map ~f:(fun (act, row) -> act :: row)
                            
     
              
  (* List.fold cols ~init:[]
   *   ~f:(fun rst col ->
   *     List.map col ~f:(fun act ->
   *         List.fold rst
   *           ~init:[]
   *           ~f:(fun rows row ->
   *             (act::row) @ rows
   *           )
   *       )
   *   ) *)

let cmd_equal (a1 : action) (a2 : action) : bool =
  let wp1 = symb_wp a1 ~fvs:(free_vars_of_cmd a2) in
  let wp2 = symb_wp a2 ~fvs:(free_vars_of_cmd a1) in
  Printf.printf "Checking whether %s == %s\n%!" (string_of_cmd a1) (string_of_cmd a2);
  check_valid (wp1 %<=>% wp2) |> Option.is_none
    
    
let candidates (phys_acts : action_set list) (log_row : action_seq) : action_seq list =
  let all_phys_rows = list_cross phys_acts in
  let cands = List.fold all_phys_rows ~init:[]
    ~f:(fun candidate_acc phys_row ->
      if cmd_equal (List.reduce_exn log_row ~f:(%:%))
           (List.reduce_exn phys_row ~f:(%:%))
      then (Printf.printf "==equal==\n%!"; phys_row :: candidate_acc)
      else (Printf.printf "<>unequal<>\n%!";candidate_acc)
    )
  in
  Printf.printf "There are %i candidates for action sequence " (List.length cands);
  List.iter log_row ~f:(fun act -> print_action act; Printf.printf ";");
  Printf.printf "\n%!";
  cands
  
                           
let rec flatten (alts_for_inst : action_seq list list) : action_seq list list =
  match alts_for_inst with
  | [] -> []
  | row_alts :: rest ->
     List.fold row_alts
       ~init:[]
       ~f:(fun acc altAct ->
         let ihop = flatten rest in
         if List.length ihop = 0 then
           [altAct] :: acc
         else
           (List.map (flatten rest)
              ~f:(fun altInst ->  altAct :: altInst)
            @ acc))

let instantize (ms : key_match list list) =
  List.map ~f:(fun acts -> {keys = ms; actions = acts})
           
let one_table_synth (tbl_log : schema) (tbl_phys : schema) (rlog : instance) =
  if is_inst_of rlog tbl_log && well_formed rlog  then
    let key_mapper = inject_keys tbl_log tbl_phys in
    let get_cands = candidates tbl_phys.actions in
    let new_matches = List.map rlog.keys ~f:key_mapper in
    let search_space = List.map rlog.actions ~f:get_cands |> flatten in
    Printf.printf "Search Space has %i candidates \n%!" (List.length search_space);
    List.exists (instantize new_matches search_space)
      ~f:(fun candidate ->
        print_instance candidate;
        if satisfies candidate tbl_phys then
          (Printf.printf "Works!!\n%!";
           true)
        else
          (Printf.printf "Violates a condition!\n%!";
           false)
      )
  else
    failwith "Precondition Failure: rlog has to be a well-formed instance of tbl_log"


(* TESTS *)
let%test _ =
  let log_schema =
    { keys = [("x", 0, 2);
              ("y", 0, 3)               
             ];
      actions = [
          ["op" %<-% Value1 (Int (1,2));
           "op" %<-% Value1 (Int (0,2))
          ];
          ["op" %<-% Value1 (Int (1,2));
           Assume True]
        ];
      constraints = [FD ([0], [2]); FD ([1], [3]); FD([0;1],[2;3])]
    }
  in
  let phys_schema =
     { keys = [("x", 0, 2);
              ("y", 0, 2)               
             ];
      actions = [
          ["op" %<-% Value1 (Int (1,2));
           "op" %<-% Value1 (Int (0,2))
          ];
          ["op" %<-% Value1 (Int (1,2));
           "op" %<-% Value1 (Int (0,2))]
        ];
      constraints = [FD([0;1],[2;3])]
     } 
 in
  let log_rel = (* op = x *)
    { keys =
        [[Int 0; Int 0];
         [Int 0; Int 1];
         [Int 1; Int 0];
         [Int 1; Int 1]
        ];
      actions =
        [ ["op" %<-% Value1 (Int (0,2)); Assume True];
          ["op" %<-% Value1 (Int (0,2)); Assume True];
          ["op" %<-% Value1 (Int (1,2)); Assume True];
          ["op" %<-% Value1 (Int (1,2)); Assume True]
        ]
    } in
  one_table_synth log_schema phys_schema log_rel


(* TESTS *)
let%test _ =
  let phys_schema =
    { keys = [("x", 0, 2);
              ("y", 0, 3)               
             ];
      actions = [
          ["op" %<-% Value1 (Int (1,2));
           "op" %<-% Value1 (Int (0,2))
          ];
          ["op" %<-% Value1 (Int (1,2));
           Assume True]
        ];
      constraints = [FD ([0], [2]); FD ([1], [3]); FD([0;1],[2;3])]
    }
  in
  let log_schema =
    { keys = [("x", 0, 2);
              ("y", 0, 2)               
             ];
      actions = [
          ["op" %<-% Value1 (Int (1,2));
           "op" %<-% Value1 (Int (0,2))
          ];
          ["op" %<-% Value1 (Int (1,2));
           "op" %<-% Value1 (Int (0,2))]
        ];
      constraints = [FD([0;1],[2;3])]
    } 
  in
  let log_rel = (* op = x *)
    { keys =
        [[Int 0; Int 0];
         [Int 0; Int 1];
         [Int 1; Int 0];
         [Int 1; Int 1]
        ];
      actions =
        [ ["op" %<-% Value1 (Int (0,2)); "op" %<-% Value1 (Int (0,2))];
          ["op" %<-% Value1 (Int (0,2)); "op" %<-% Value1 (Int (0,2))];
          ["op" %<-% Value1 (Int (1,2)); "op" %<-% Value1 (Int (1,2))];
          ["op" %<-% Value1 (Int (1,2)); "op" %<-% Value1 (Int (1,2))]
        ]
    } in
  one_table_synth log_schema phys_schema log_rel
                  

let%test _ =
  let phys_schema =
    { keys = [("src", 0, 2);
              ("x0", 0, 2)               
             ];
      actions = [
          ["x1" %<-% Value1 (Int (1,2));
           "x1" %<-% Var1 ("x0", 2)
          ];
          ["dst" %<-% Value1 (Int (1,2));
           "dst" %<-% Value1 (Int (2,2));
           "dst" %<-% Value1 (Int (3,2));
           "dst" %<-% Value1 (Int (4,2));
          ]
        ];
      constraints = [FD ([0], [2]);
                     SemFD {act=[2]; inputs=[1]; outputs = ["x1"]; range=[3]};
                     FD([0;1],[2;3])]
    }
  in
  let log_schema =
    { keys = [("src", 0, 2);
              ("x0", 0, 2)               
             ];
      actions =
        [["x1" %<-% Value1 (Int (1,2));
          "x1" %<-% Var1 ("x0", 2);
          "dst" %<-% Value1 (Int (1,2));
          "dst" %<-% Value1 (Int (2,2));
          "dst" %<-% Value1 (Int (3,2));
          "dst" %<-% Value1 (Int (4,2))
         ];
         ["x1" %<-% Value1 (Int (1,2));
          "x1" %<-% Var1 ("x0", 2);
          "dst" %<-% Value1 (Int (1,2));
          "dst" %<-% Value1 (Int (2,2));
          "dst" %<-% Value1 (Int (3,2));
          "dst" %<-% Value1 (Int (4,2))
        ]];
      constraints = [FD([0;1],[2;3])]
    } 
  in
  let log_rel = (* op = x *)
    { keys =
        [[Int 0; Int 0];
         [Int 0; Int 1];
         [Int 1; Int 0];
         [Int 1; Int 1]
        ];
      actions =
        [ ["dst" %<-% Value1 (Int (1,2)); "x1" %<-% Value1 (Int (1,2))];
          ["dst" %<-% Value1 (Int (1,2)); "x1" %<-% Value1 (Int (1,2))];
          ["dst" %<-% Value1 (Int (2,2)); "x1" %<-% Var1 ("x0",2)];
          ["dst" %<-% Value1 (Int (1,2)); "x1" %<-% Var1 ("x0",2)]
        ];
    } in
  one_table_synth log_schema phys_schema log_rel

