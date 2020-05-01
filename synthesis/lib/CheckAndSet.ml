open Core
open Ast
open Util
open Manip
open Synthesis
open Prover
open Tables

let rec map_holes_expr e ~f =
  let binop op e1 e2 = op (map_holes_expr e1 ~f) (map_holes_expr e2 ~f) in
  match e with
  | Value _ | Var _ -> e
  | Hole(h,sz) -> Hole (f h, sz)
  | Plus (e1,e2) -> binop mkPlus e1 e2
  | Minus(e1,e2) -> binop mkMinus e1 e2
  | Times(e1,e2) -> binop mkTimes e1 e2
  | Mask (e1,e2) -> binop mkMask e1 e2

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
    Hole(freshen h sz i), StringMap.set holes_so_far ~key:h ~data:(i + 1)
  in
  let binop op e1 e2 =
    let (e1', holes_so_far') = freshen_holes_expr e1 holes_so_far in
    let (e2', holes_so_far'') = freshen_holes_expr e2 holes_so_far' in
    (op e1' e2', holes_so_far'')
  in
  match e with
  | Value _ | Var _ -> (e, holes_so_far)
  | Hole (h,sz) ->
     begin match StringMap.find holes_so_far h with
     | Some i -> freshen_hole h sz i
     | None -> freshen_hole h sz 0
     end
  | Plus(e1,e2) -> binop mkPlus e1 e2
  | Minus(e1,e2) -> binop mkMinus e1 e2
  | Times(e1,e2) -> binop mkTimes e1 e2
  | Mask (e1,e2) -> binop mkMask e1 e2


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
            acc @ [Match.Between(mkInt (0,sz), mkInt(pow 2 sz,sz))]
         | None -> Printf.sprintf "couldn't find %d in list of length %d" k (List.length keys)
                   |> failwith
         end
    )


let project_classifier table keys table_keys rows =
  let projection = common_indices keys table_keys in
  List.mapi rows ~f:(fun i (matches,_,_) ->
      Printf.printf "Projecting matches %s\n%!" (Match.list_to_string matches);
      project keys projection matches
      |> Match.list_to_test keys
      (* |> mkAnd (Hole(Printf.sprintf "?keep_%d_%s" i table,1) %=% mkVInt(1,1)) *)
    )
  (* |> List.map ~f:(fun t ->
   *        Printf.printf "Match %s\n%!" (string_of_test t);
   *        t
   *      ) *)

(* Skip must be one of the actions *)
let instrumented_action_for_hole (h,_) actions =
  let act_size = max (log2 (List.length actions)) 1 in
  List.(mapi actions ~f:(fun i (data, act)->
            let act''= holify (data >>| fst) act
                       |> map_holes ~f:(fun hole -> Printf.sprintf "%s_%s" hole h) in
            Hole("?act_for_path_"^h, act_size) %=% mkVInt(i,act_size),
            act''
  ))
  |> mkOrdered


let populate_action_table name keys actions holes : cmd =
  match keys with
  | [k,sz] ->
     StringMap.fold holes ~init:[]
       ~f:(fun ~key:base_hole ~data:max_idx acc ->
         acc @ List.init max_idx ~f:(fun idx ->
                   let hole, _ =  freshen (base_hole ^ "__") sz idx in
                   (Var(k,sz) %=% Hole(hole, sz),
                    instrumented_action_for_hole (hole,sz) actions
                   )
                 )
       )@ [True, Skip] |> mkOrdered
  | _ -> Printf.sprintf "%s must have one key since it is the action table. It has %d" name (List.length keys)
         |> failwith



let rec instrument_entries (table_keys : (string * int) list) (rows : Row.t list) (action_table : string)
          (pipeline : cmd) (holes : int StringMap.t) : cmd * (int StringMap.t)   =
  match pipeline with
  | Skip -> (Skip, holes)
  | Assert _ | Assume _ | Select _ ->
     failwith "don't know how to handle conditionals"
  | Assign (f,e) -> (f %<-% e, holes)
  | Seq(c1,c2) ->
     let c1', holes' = instrument_entries table_keys rows action_table c1 holes in
     let c2', holes'' = instrument_entries table_keys rows action_table c2 holes' in
     (c1' %:% c2', holes'')
  | While(b,c) -> failwith "while loops are hard"
  | Apply t ->
     if t.name = action_table
     then (populate_action_table t.name t.keys t.actions holes, holes)
     else if List.length t.actions <> 1
     then failwith "too many actions"
     else
       (* let _ = Printf.printf "Projecting the classifier\n%!" in *)
       let (data, act) = List.hd_exn t.actions in
       let selects, holes =
         project_classifier t.name t.keys table_keys  rows
         |> List.(fold ~init:([],holes)
                    ~f:(fun (ss,holes) test ->
                      Printf.printf "freshening entry %s -> %s\n%!" (string_of_test test) (string_of_cmd act);
                      let act', holes' = holify (data>>|fst) act
                                         |> flip freshen_holes holes in
                      (ss @ [test, act'], holes'))
            ) in
       let default', holes' = freshen_holes t.default holes in
       (mkOrdered (selects @ [True,default']), holes')

let rec apply_model_expr m e =
  let binop op e1 e2 =
    op (apply_model_expr m e1) (apply_model_expr m e2)
  in
  match e with
  | Value _ | Var _ -> e
  | Hole(x, sz) ->
     begin match StringMap.find m x with
     | None -> Printf.sprintf "Model is incomplete, no value for %s" x |> failwith
     | Some v -> Value(v)
     end
  | Plus(e1,e2) -> binop mkPlus e1 e2
  | Minus(e1,e2) -> binop mkMinus e1 e2
  | Times(e1,e2) -> binop mkTimes e1 e2
  | Mask(e1,e2) -> binop mkMask e1 e2


let rec apply_model_test m t : test =
  let binopb (op : test -> test -> test) b1 b2 : test =
    op (apply_model_test m b1) (apply_model_test m b2)
  in
  let binope (op : expr -> expr -> test) e1 e2 : test =
    op (apply_model_expr m e1) (apply_model_expr m e2)
  in
  match t with
  | True | False -> t
  | Eq(e1,e2) -> binope (%=%) e1 e2
  | Le(e1,e2) -> binope (%<=%) e1 e2
  | And(b1,b2) -> binopb (%&%) b1 b2
  | Or (b1,b2) -> binopb (%+%) b1 b2
  | Impl(b1,b2) -> binopb (%=>%) b1 b2
  | Iff(b1,b2) -> binopb (%<=>%) b1 b2
  | Neg(b) -> !%(apply_model_test m b)

let rec apply_model m cmd =
  match cmd with
  | Skip -> Skip
  | Assert t -> apply_model_test m t |> Assert
  | Assume t -> apply_model_test m t |> Assume
  | Assign (f,e) ->
     Assign(f,apply_model_expr m e)
  | Seq(c1,c2) -> apply_model m c1 %:% apply_model m c2
  | Select(typ, ss) ->
     List.map ss ~f:(fun (t, c) ->
         apply_model_test m t, apply_model m c
       )
     |> mkSelect typ
  | While _ | Apply _ -> failwith "you died of dysentery"


let populate one_big_table fvs physical_pipeline physical_check_name logical_rows =
  match one_big_table with
  | Apply t ->
     let phys_instrumented,_ = instrument_entries t.keys logical_rows physical_check_name physical_pipeline StringMap.empty in
     let log_inst = StringMap.of_alist_exn [(t.name,logical_rows)] in
     let log_populated =
       sequence [
           "drop"%<-%mkVInt(0,1);
           "outport"%<-%mkVInt(0,9);
           (Instance.apply Instance.NoHoles `Exact log_inst one_big_table |> fst) ]
     in
     begin match check_sat Parameters.default (equivalent fvs phys_instrumented log_populated) with
     | None, d ->
        Printf.printf "Logical Program:\n%s\n%!The instrumented physical program:\n%s\n"
          (string_of_cmd log_populated)
          (string_of_cmd phys_instrumented);
        Printf.printf "Could not find a model in %fms\n" (Time.Span.to_ms d)
     | Some m, d ->
        Printf.printf "Logical program:\n%s\n%!The instrumented physical program:\n%s\nThe synthesized program: \n %s\n%!"
          (string_of_cmd log_populated)
          (string_of_cmd phys_instrumented)
          (string_of_cmd (apply_model (Packet.un_SSA m) phys_instrumented))
     end
  | _ -> Printf.sprintf "logical_table was not one big table, it was %s"(string_of_cmd one_big_table)
         |> failwith




let test_population _ =
  populate
    (mkApply("megatable",
           ["inport",9; "ipv4", 32],
           [ ["o",9], "outport" %<-% Var("o", 9)
           ; [], "drop"%<-% mkVInt(1,1)
           ],
           Skip))
    (["inport",9; "ipv4", 32; "outport",9; "drop",1])
    (sequence [
         "drop"%<-%mkVInt(0,1); "outport"%<-%mkVInt(0,9); "next"%<-%mkVInt(0,32)
       ; mkApply("ingress", ["inport", 9], [["n", 32], "next"%<-%Var("n",32)], "next"%<-%Hole("n",32))
       ; mkApply("l3_fwd", ["ipv4", 32], [["n",32], "next" %<-% Var("n",32)], "next"%<-%Hole("n",32))
       ; mkApply("next", ["next",32], [["o",9], sequence["outport"%<-% Var("o", 9)]
                                 ;[], Skip], Skip)
    ])
    "next"
    [  [Exact(mkInt(3,9)); Exact(mkInt(255, 32))], [mkInt(2,9)], 0
     (* ; [Between(0,pow 2 9-1,9); Exact(2, 32)], [1,9], 0 *)
    ]





(* let test_instrumentation _ =
 *   let instr,_ = instrument_entries
 *     ["inport", 9; "ipv4", 32]
 *     [ [Exact(9,9); Exact(2, 32)], [], 0
 *     ; [Exact(3,9); Exact(255, 32)], [], 0
 *     ]
 *     "next"
 *     (sequence [
 *          Apply("ingress", ["inport", 9], [["n", 32], "next"%<-%Var("n",32)], Skip)
 *        ; Apply("l3_fwd", ["ipv4", 32], [["n",32], "next" %<-% Var("n",32)],Skip)
 *        ; Apply("next", ["next",32], [["o",9], sequence["outport"%<-% Var("o", 32); "drop" %<-% mkVInt(0,1)]
 * (\*;[], "drop"%<-% mkVInt(1,1)*\)], Skip)
 *     ])
 *     StringMap.empty
 *   in
 *   Printf.printf "%s\n%!" (string_of_cmd instr) *)


let test_diagram _ =
  let open Diagram in
  let zero = Int64.of_int 0 in
  let five = Int64.of_int 5 in
  let four = Int64.of_int 4 in
  let three = Int64.of_int 3 in
  let two = Int64.of_int 2 in
  let star = Value.Mask(zero,0) in
  (* let _ = Printf.printf "The first one\n" in *)
  let fdd = create_from_rows
              Field.([InPort; IP4Dst ; IP4Src])
              Value.([[star; Const five; Const four], [1]
                     ;[Const two; Const four; star], [2]
                     ;[Const four; Const four; Const four ], [2]
                     ;[star; Const four; star], [2]
                     ;[Const three; Const three; Const three],[3]
                     ;[star; star; star], [0]]) in
  let fdd' = fdd in (*prune_unreachable fdd in*)
  to_dot fdd' |> compile_dot |> Printf.printf "%s\n%!";
  let tbl, cont = delta fdd [IP4Src; IP4Dst] in
  (* List.iter rows ~f:(fun d -> Printf.printf "%s\n%!" (d |> to_string)); *)
  (* printf.printf "The second one\n%!"; *)
  to_dot tbl |> compile_dot |> Printf.printf "%s\n%!";
  fold cont ~f:mk_leaf ~g:cond
  |> to_dot |> compile_dot |> Printf.printf "%s\n%!"


let run x = (*test_instrumentation x*)
  (* test_population x *)
  test_diagram x
