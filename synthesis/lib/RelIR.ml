(**
 * Intermediate representation Based on a Relational interpretation of Tables
 *)

open Core
open Ast
open Prover
open Packet
open Synthesis
open Util

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
let print_schema schema =
  List.iter schema.keys ~f:(fun (f,_,_) -> Printf.printf "%s " f);
  Printf.printf " |  ";
  List.iteri schema.actions ~f:(fun i _ -> Printf.printf "%d " i);
  Printf.printf "\n%!"


type key_match =
  | Int of Bigint.t
  | Range of {lo: int; hi : int}

let print_key_match (km : key_match) : unit =
  match km with
  | Int i -> Printf.printf "%s" (Bigint.to_string i)
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

let eval_acts_rel (fields : string list) (values_acts : (key_match list * action_seq) list) : Packet.t list =
  List.map values_acts
    ~f:(fun (values, acts) ->
      let () = Printf.printf "%d actions \n%!" (List.length acts) in
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
                     | Ast.Int (f,_) -> Int f))

(* Precondition: act_ids must only be actions, and in_ids and out_ids
   must only be field indices *)
let satSemFD (r : instance) (t : schema) (act_ids : size list) (in_ids : size list) (out_fields : string list) (rng_ids : size list) : bool =
  let () = Printf.printf "in_ids ";
          List.iter (in_ids) ~f:(Printf.printf "%i ");
          Printf.printf "\n%!" in
  let () = Printf.printf "act_ids ";
          List.iter (act_ids) ~f:(Printf.printf "%i ");
          Printf.printf "\n%!" in
  let rows = List.zip_exn r.keys r.actions in
  let values_acts = proj rows (in_ids @ act_ids) in
  let fields = ids_to_fields t in_ids in
  let rng = proj rows rng_ids in
  let image = eval_acts_rel fields values_acts in
  let dom = extract image out_fields in
  does_induce_function dom rng


let rec evalRel (r : key_match list) (t : schema) (e : expr) =
  match e with
  | Value v -> v
  | Var (name, sz) ->
     begin match List.findi t.keys ~f:(fun _ (name', _, _) -> name = name') with
     | None -> failwith "Error could not evaluate column name"
     | Some (i, _) -> match List.nth_exn r i with
                      | Int x -> Int (x,sz)
                      | Range _ -> failwith "I dont want to symbolically evaluate with a range"
     end
  | Plus (e,e') ->
     begin match evalRel r t e, evalRel r t e' with
     | Int (x, sz), Int (y,_) -> Int (Bigint.(x + y), sz)
     end
  | Times (e, e') ->
     begin match evalRel r t e, evalRel r t e' with
     | Int (x, sz), Int (y,_) -> Int (Bigint.(x * y), sz)
     end
  | Minus (e, e') ->
     begin match evalRel r t e, evalRel r t e' with
     | Int (x, sz), Int (y,_) -> Int (Bigint.(x - y), sz)
     end
  | _ -> failwith "IDK how to use tuples"


let rec satOneRow keys t b  =
  match b with
  | True -> true
  | False -> false
  | Neg b -> not (satOneRow keys t b)
  | And  (a, b) -> satOneRow keys t a && satOneRow keys t b
  | Or   (a, b) -> satOneRow keys t a || satOneRow keys t b
  | Impl (a, b) -> not(satOneRow keys t a) || satOneRow keys t b
  | Iff  (a, b) -> satOneRow keys t (a %=>% b) && satOneRow keys t (b %=>% a)
  | Eq (x, y) -> evalRel keys t x = evalRel keys t y
  | Le (x, y) -> evalRel keys t x = evalRel keys t y


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
  if List.exists r.keys
         ~f:(fun row_keys ->
           List.length row_keys <> List.length table.keys)
  then (Printf.printf "keys are incorrect length!\n%!"; false)
  else
    match List.findi r.actions
            ~f:(fun _ row_acts -> List.length row_acts <> List.length table.actions)
    with
    | Some (i,_) -> (Printf.printf "acts %d are incorrect length, must be %d \n%!"
                       (i) (List.length table.keys)
                ; false)
    | None ->
       (if List.fold r.actions ~init:true
            ~f:(fun rslt act_row ->
              List.foldi act_row ~init:true
                ~f:(fun idx rslt' act ->
                  List.exists (List.nth_exn table.actions idx) ~f:((=) act)
                  && rslt')
              && rslt)
        then true
        else (Printf.printf "theres an unknown action in the table!"; false))
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

let sym_diff u v =
  let open List in
  let not_contain lst (str, _) = for_all lst ~f:(fun (str',_) -> str <> str') in
  let unotv = filter u ~f:(not_contain v) in
  let vnotu = filter v ~f:(not_contain u) in
  unordered_append unotv vnotu

let group_by_keys u v =
  List.merge u v ~compare:(fun (f,_) (g,_) -> Stdlib.compare f g)
  |> List.group ~break:(fun (f,_) (g,_) -> f <> g)



(* PRE: Assume valuations are functions *)
let combine_valuations u v =
  let open Option in
  let rec reduce_group = function
    | [] -> failwith "group gave an empty sequence to reduce_group"
    | [(x,e)] -> Some (x,e)
    | ((x,e)::assoc) ->
       match reduce_group assoc with
       | Some (y, e') ->
          if x <> y then failwith "group gave a sequence with different fields"
          else
            (* We should check whether e e' is sat *)
            if e = e' then Some (x, e) else None
       | None -> None
  in
  group_by_keys u v
  |> List.fold ~init:(Some [])
       ~f:(fun acc_opt grp -> liftO2 mkCons (reduce_group grp) acc_opt)
  >>| List.unordered_append (sym_diff u v)

let rec test_to_valuation (t : test) : (string * expr) list option =
  match t with
  | True -> Some []
  | False -> None
  | And (a,b) ->
     let open Option in
     test_to_valuation a >>= fun avals ->
     test_to_valuation b >>= fun bvals ->
     combine_valuations avals bvals
  | Eq (e1, e2) ->
     begin match e1,e2 with
     | Var(x,_), Var(y,_) ->
        if is_symbolic x
        then if is_symbolic y then (failwith (Printf.sprintf "ERROR: SymbEq %s = %s" x y))
             else Some [(x,e2)]
        else if is_symbolic y then Some [(x, e1)]
        else failwith (Printf.sprintf "Don't know how to handle %s = %s \
                                      since one side is not a symbolic variable" x y)

     | e,Var (x,_) | Var (x,_), e -> Some [(x, e)]
     | _ -> failwith (Printf.sprintf "Don't know how to handle %s = %s \
                                      since one side is not a variable"
                        (string_of_expr e1)
                        (string_of_expr e2))
     end
  | Impl _ | Iff _
    | Or _ -> failwith "cannot convert a disjunction/implication/iff into a valuation"
  | Neg _ -> failwith "cannot convert a negation into a valuation"
  | Le _ -> failwith "cannot convert a <= into a valuation"

let compute_eq_cond (u : (string * expr) list option) (v : (string * expr) list option) =
  match (u, v) with
  | Some u, Some v  ->
     let d = List.fold (sym_diff u v) ~init:True
               ~f:(fun acc (x,e) ->
                 acc %&% (Var (unsymbolize x,2) %=% e)) in
     let m = group_by_keys u v in
     List.fold m ~init:d
       ~f:(fun cond grp ->
         List.cartesian_product grp grp
         |> List.fold ~init:cond ~f:(fun cond ((_,e),(_,e')) ->
                cond %&%
                  if e = e' then True
                  else e %=% e'))
  | _, _ -> False





let cmd_equal (a1 : action) (a2 : action) : bool =
  let wp1 = symb_wp a1 ~fvs:(free_vars_of_cmd a2) in
  let wp2 = symb_wp a2 ~fvs:(free_vars_of_cmd a1) in
  Printf.printf "Checking whether %s == %s\n%!" (string_of_cmd a1) (string_of_cmd a2);
  check_valid Parameters.default (wp1 %<=>% wp2) |> fst |> Option.is_none


let cmd_equalable bound_vars (a1 : action) (a2 : action) : test option =
  let open Option in
  let wp1 = symb_wp a1 ~fvs:(free_vars_of_cmd a2) in
  let wp2 = symb_wp a2 ~fvs:(free_vars_of_cmd a1) in
  test_to_valuation wp1 >>= fun v1 ->
  test_to_valuation wp2 >>= fun v2 ->
  let () = printf "wp1 = %s \n wp2 = %s\n%!" (string_of_test wp1) (string_of_test wp2) in
  let () =
    Printf.printf "v1 = [\n%!";
    List.iter v1 ~f:(fun (x,e) -> Printf.printf "   %s |-> %s ,\n%!" x (string_of_expr e));
    Printf.printf "]\n%!"
  in
  let () =
    Printf.printf "v2 = [\n%!";
    List.iter v2 ~f:(fun (x,e) -> Printf.printf "%s |-> %s ,\n%!" x (string_of_expr e));
    Printf.printf "]\n%!"
  in
  let condition = compute_eq_cond (test_to_valuation wp1) (test_to_valuation wp2) in
  Printf.printf "Checking whether sat %s == %s\n via the formula %s!\n%!"
    (string_of_cmd a1)
    (string_of_cmd a2)
    (string_of_test condition);
  holify_test bound_vars condition |> check_sat Parameters.default |> fst >>= const (Some condition)

let candidates (phys : schema) (log_row : action_seq) : (test * action_seq) list =
  let all_phys_rows = list_cross phys.actions in
  let key_fields = List.map ~f:(fun (x,_,_) -> x) phys.keys in
  let mk_act = List.reduce_exn ~f:(%:%) in
  let log_act = mk_act log_row in
  (* let cands = List.fold all_phys_rows ~init:[]
   *   ~f:(fun candidate_acc phys_row ->
   *     if cmd_equal (List.reduce_exn log_row ~f:(%:%))
   *          (List.reduce_exn phys_row ~f:(%:%))
   *     then (Printf.printf "==equal==\n%!"; phys_row :: candidate_acc)
   *     else (Printf.printf "<>unequal<>\n%!";candidate_acc)
   *   )
   * in *)
  let cands =
    List.fold all_phys_rows ~init:[]
      ~f:(fun candidate_acc phys_row ->
        match cmd_equalable key_fields log_act (mk_act phys_row) with
        | Some cond ->
           Printf.printf "==equal if %s holds==\n%!" (string_of_test cond);
           (cond, phys_row) :: candidate_acc
        | None ->
           Printf.printf "<>unequal<>\n%!";
           candidate_acc
      )
  in
  Printf.printf "There are %i candidates for action sequence %s \n%!"
    (List.length cands) (string_of_cmd log_act);
  cands

let match_test (tbl : schema) (matches : key_match list) =
  List.fold2_exn tbl.keys matches ~init:True
    ~f:(fun acc (k, _, _)  m ->
      match m with
      | Int i -> (Var (k, 2) %=% Value(Int (i,2))) %&% acc
      | Range {lo; hi} -> (Var (k, 2) %>=% mkVInt (lo, 2)) %&%
                            (Var (k, 2) %<=% mkVInt (hi, 2)) %&%
                              acc)


let check_keys_sat_cond phys_tbl phys_matches (cond, act) =
  let open Option in
  check_sat Parameters.default (match_test phys_tbl phys_matches %=>% cond) |> fst >>| const act


let cands_for_row  (phys_tbl : schema) (phys_matches : key_match list) (log_acts : action_seq)  =
  candidates phys_tbl log_acts
  |> List.filter_map ~f:(check_keys_sat_cond phys_tbl phys_matches)


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

let compute_candidate_map (log : schema) (phys : schema) :
      ((action_seq * ((test * action_seq) list)) list ) =
  list_cross log.actions
  |> List.map  ~f:(fun actseq ->
      (actseq, List.dedup_and_sort (candidates phys actseq) ~compare:Stdlib.compare))

let one_table_synth
      ?cand_map:(cand_map=None)
      (tbl_log : schema) (tbl_phys : schema) (rlog : instance)
  =
  if is_inst_of rlog tbl_log  && well_formed rlog  then
    let key_mapper = inject_keys tbl_log tbl_phys in
    let get_cands matches acts =
      match cand_map with
      | None -> cands_for_row tbl_phys matches acts
      | Some m -> match List.find m ~f:(fun (a, _) -> acts = a) with
                  | None ->
                     failwith "Malformed Map Supplied"
                  | Some (_, cands) ->
                     List.filter_map cands ~f:(check_keys_sat_cond tbl_phys matches)
    in
    let new_matches = List.map rlog.keys ~f:key_mapper in
    let search_space_big = List.map2_exn new_matches rlog.actions ~f:get_cands in
    let _ : unit Core.List.Or_unequal_lengths.t =
      List.iter2 rlog.actions search_space_big
        ~f:(fun l cs ->
          Printf.printf "logical action:\n   %s\ncan be mapped to any \
                         of the following:\n%!" (string_of_cmd (List.reduce_exn l ~f:(%:%)));
          List.iter cs ~f:(fun c ->
              Printf.printf "\t%s\n%!" (string_of_cmd (List.reduce_exn c ~f:(%:%)))
            )
        )
    in
    if List.exists search_space_big ~f:(List.is_empty)
    then false
    else
      let search_space = flatten search_space_big in
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
    (print_schema tbl_log;
     print_instance rlog;
     failwith "Precondition Failure: above has to be a well-formed instance of tbl_log")



let dominating_vars (act_seq : action_set list) (keys : (string * size * size) list) : (string * size * size) list =
  let rec delta (act : action) (var, lo, hi) =
    match act with
    | Assign (x, e) -> if x = var
                       then (free_of_expr `Var e)
                            |> List.map ~f:(fun (v,_) -> (v,2, 2))
                       else [(var, lo, hi)]
    | Seq (c1, c2) ->
       delta c2 (var, lo, hi)
       |> List.fold ~init:[]
            ~f:(fun acc var ->  acc @ delta c1 var)
    | Assert (True) | Assume True -> [(var, lo, hi)]
    | _ -> failwith "Don't know how to implement complicated \
                     assertions/assumptions or choice in actions"
  in
  let delta' (acts : action_set) key =
    List.fold acts ~init:[] ~f:(fun acc a -> acc @ delta a key )
  in
  let delta'' (acts_seq : action_set list) key =
    List.fold acts_seq ~init:[] ~f:(fun acc acts -> acc @ delta' acts key)
  in
  List.fold keys ~init:[] ~f:(fun acc k-> acc @ delta'' act_seq k)
  |> List.dedup_and_sort ~compare:Stdlib.compare


let rec range lo hi =
  if lo >= hi
  then []
  else if lo + 1 = hi
  then [lo]
  else lo :: range (lo + 1) hi

(**************************** NORMALIZATION ***********************************)

let compose_schemas (s : schema) (t : schema) : schema =
  let open List in
  let dom_vars = dominating_vars s.actions t.keys in
  let new_keys = s.keys @ dom_vars in
  let num_keys = length new_keys in
  let new_actions = s.actions @ t.actions in
  let num_actions = length new_actions in
  let s_is_table =
    FD (range 0 (length s.keys)
      , range num_keys (num_keys + length s.actions))
  in
  let st_is_table =
    FD (range 0 num_keys,
        range num_keys (num_keys + num_actions))
  in
  let comp_is_decomp =
    SemFD { act = range num_keys (num_keys + length s.actions);
            inputs = range (length s.keys) num_keys;
            outputs = List.map t.keys ~f:(fun (k,_,_) -> k);
            range = range (num_keys + length s.actions)
                      (num_keys + num_actions) }
  in
  { keys = new_keys;
    actions = s.actions @ t.actions;
    constraints = [s_is_table; st_is_table; comp_is_decomp]
  }

(*************************** END NORMALIZATION *******************************)





(* TESTS *)
let append_instance (s : instance) (t : instance) =
  {
    keys = s.keys @ t.keys ;
    actions = s.actions @ t.actions
  }
let random_elt lst = Random.int (List.length lst)
                     |> List.nth_exn lst

let rec mk_random_instance n (table : schema) =
  if n = 0 then {keys = []; actions = []}
  else
    {
      keys = [List.map table.keys ~f:(fun (_,lo,hi) -> Int (Random.int (hi+1) + lo |> Bigint.of_int_exn))];
      actions = [List.map table.actions ~f:random_elt]
    } |> append_instance (mk_random_instance (n-1) table)

let rec generate_valid_instance ?n:(n = 10) ~table =
  let instance = mk_random_instance n table in
  if satisfies instance table
  then instance
  else generate_valid_instance ~n ~table

let rec qc ~reps ~size f ~table =
  if reps = 0 then true else
    let inst = generate_valid_instance ~n:size ~table in
    if f inst
    then
      qc ~reps:(reps - 1) ~size f ~table
    else
      (Printf.printf "Counter Example. The following cannot be mapped\n%!"
      ; print_instance inst
      ; false)

(* let%test _ =
 *   let log_schema =
 *     { keys = [("x", 0, 2);
 *               ("y", 0, 3)
 *              ];
 *       actions = [
 *           ["op" %<-% Value (Int (1,2));
 *            "op" %<-% Value (Int (0,2))
 *           ];
 *           ["op" %<-% Value (Int (1,2));
 *            Assume True]
 *         ];
 *       constraints = [FD ([0], [2]); FD ([1], [3]); FD([0;1],[2;3])]
 *     }
 *   in
 *   let phys_schema =
 *      { keys = [("x", 0, 2);
 *               ("y", 0, 2)
 *              ];
 *       actions = [
 *           ["op" %<-% Value (Int (1,2));
 *            "op" %<-% Value (Int (0,2))
 *           ];
 *           ["op" %<-% Value (Int (1,2));
 *            "op" %<-% Value (Int (0,2))]
 *         ];
 *       constraints = [FD([0;1],[2;3])]
 *      }
 *  in
 *   let log_rel = (\* op = x *\)
 *     { keys =
 *         [[Int 0; Int 0];
 *          [Int 0; Int 1];
 *          [Int 1; Int 0];
 *          [Int 1; Int 1]
 *         ];
 *       actions =
 *         [ ["op" %<-% Value (Int (0,2)); Assume True];
 *           ["op" %<-% Value (Int (0,2)); Assume True];
 *           ["op" %<-% Value (Int (1,2)); Assume True];
 *           ["op" %<-% Value (Int (1,2)); Assume True]
 *         ]
 *     } in
 *   one_table_synth log_schema phys_schema log_rel
 *
 * let%test _ =
 *   let phys_schema =
 *     { keys = [("x", 0, 2);
 *               ("y", 0, 3)
 *              ];
 *       actions = [
 *           ["op" %<-% Value (Int (1,2));
 *            "op" %<-% Value (Int (0,2))
 *           ];
 *           ["op" %<-% Value (Int (1,2));
 *            Assume True]
 *         ];
 *       constraints = [FD ([0], [2]); FD ([1], [3]); FD([0;1],[2;3])]
 *     }
 *   in
 *   let log_schema =
 *     { keys = [("x", 0, 2);
 *               ("y", 0, 2)
 *              ];
 *       actions = [
 *           ["op" %<-% Value (Int (1,2));
 *            "op" %<-% Value (Int (0,2))
 *           ];
 *           ["op" %<-% Value (Int (1,2));
 *            "op" %<-% Value (Int (0,2))]
 *         ];
 *       constraints = [FD([0;1],[2;3])]
 *     }
 *   in
 *   let log_rel = (\* op = x *\)
 *     { keys =
 *         [[Int 0; Int 0];
 *          [Int 0; Int 1];
 *          [Int 1; Int 0];
 *          [Int 1; Int 1]
 *         ];
 *       actions =
 *         [ ["op" %<-% Value (Int (0,2)); "op" %<-% Value (Int (0,2))];
 *           ["op" %<-% Value (Int (0,2)); "op" %<-% Value (Int (0,2))];
 *           ["op" %<-% Value (Int (1,2)); "op" %<-% Value (Int (1,2))];
 *           ["op" %<-% Value (Int (1,2)); "op" %<-% Value (Int (1,2))]
 *         ]
 *     } in
 *   one_table_synth log_schema phys_schema log_rel
 *
 *
 * let%test _ =
 *   let phys_schema =
 *     { keys = [("src", 0, 2);
 *               ("x0", 0, 2)
 *              ];
 *       actions = [
 *           ["x1" %<-% Value (Int (1,2));
 *            "x1" %<-% Var ("x0", 2);
 *            Assume True
 *           ];
 *           ["dst" %<-% Value (Int (1,2));
 *            "dst" %<-% Value (Int (2,2));
 *            "dst" %<-% Value (Int (3,2));
 *            "dst" %<-% Value (Int (4,2));
 *            Assume True
 *           ]
 *         ];
 *       constraints = [FD ([0], [2]);
 *                      SemFD {act=[2]; inputs=[1]; outputs = ["x1"]; range=[3]};
 *                      FD([0;1],[2;3])]
 *     }
 *   in
 *   let log_schema =
 *     { keys = [("src", 0, 2);
 *               ("x0", 0, 2)
 *              ];
 *       actions =
 *         [["x1" %<-% Value (Int (1,2));
 *           "x1" %<-% Var ("x0", 2);
 *           "dst" %<-% Value (Int (1,2));
 *           "dst" %<-% Value (Int (2,2));
 *           "dst" %<-% Value (Int (3,2));
 *           "dst" %<-% Value (Int (4,2))
 *          ];
 *          ["x1" %<-% Value (Int (1,2));
 *           "x1" %<-% Var ("x0", 2);
 *           "dst" %<-% Value (Int (1,2));
 *           "dst" %<-% Value (Int (2,2));
 *           "dst" %<-% Value (Int (3,2));
 *           "dst" %<-% Value (Int (4,2))
 *         ]];
 *       constraints = [FD([0;1],[2;3])]
 *     }
 *   in
 *   let log_rel = (\* op = x *\)
 *     { keys =
 *         [ [Int 0; Int 0];
 *           [Int 0; Int 1];
 *           [Int 1; Int 0];
 *           [Int 1; Int 1]
 *         ];
 *       actions =
 *         [ ["dst" %<-% Value (Int (1,2)); "x1" %<-% Value (Int (1,2))];
 *           ["dst" %<-% Value (Int (1,2)); "x1" %<-% Value (Int (1,2))];
 *           ["dst" %<-% Value (Int (2,2)); "x1" %<-% Var ("x0",2)];
 *           ["dst" %<-% Value (Int (1,2)); "x1" %<-% Var ("x0",2)]
 *         ];
 *     } in
 *   one_table_synth log_schema phys_schema log_rel
 *   && not ((one_table_synth log_schema phys_schema)
 *           |> qc ~reps:20 ~size:2 ~table:log_schema)
 *
 *
 * let%test _ =
 *   let phys_schema =
 *     { keys = [("src", 0, 2);
 *               ("y", 0, 2)
 *              ];
 *       actions = [
 *           [ "x1" %<-% Var ("y", 2);
 *           ];
 *           ["dst" %<-% Value (Int (1,2));
 *            "dst" %<-% Value (Int (2,2));
 *            "dst" %<-% Value (Int (3,2));
 *            "dst" %<-% Value (Int (4,2));
 *           ]
 *         ];
 *       constraints = [FD ([0], [2]);
 *                      SemFD {act=[2]; inputs=[1]; outputs = ["x1"]; range=[3]};
 *                      FD([0;1],[2;3])]
 *     }
 *   in
 *   let log_schema =
 *     { keys = [("src", 0, 2);
 *               ("y", 0, 2)
 *              ];
 *       actions =
 *         [[  "x1" %<-% mkVInt (0,2);
 *             "x1" %<-% mkVInt (1,2)
 *          ];
 *          [
 *            "dst" %<-% Value (Int (1,2));
 *            "dst" %<-% Value (Int (2,2));
 *            "dst" %<-% Value (Int (3,2));
 *            "dst" %<-% Value (Int (4,2))
 *          ]
 *         ];
 *       constraints = [FD([0;1],[2;3])]
 *     }
 *   in
 *   let log_rel_succeed =
 *     { keys =
 *         [ [Int 0; Int 1];
 *           [Int 1; Int 1];
 *           [Int 0; Int 0];
 *           [Int 1; Int 0]
 *         ];
 *       actions =
 *         [ ["x1" %<-% mkVInt (1,2); "dst" %<-% mkVInt (1,2)];
 *           ["x1" %<-% mkVInt (1,2); "dst" %<-% mkVInt (1,2)];
 *           ["x1" %<-% mkVInt (0,2); "dst" %<-% mkVInt (1,2)];
 *           ["x1" %<-% mkVInt (0,2); "dst" %<-% mkVInt (1,2)]
 *         ];
 *     } in
 *   let log_rel_fail =
 *     { keys =
 *         [ [Int 0; Int 1];
 *           [Int 1; Int 1];
 *           [Int 0; Int 0];
 *           [Int 1; Int 0]
 *         ];
 *       actions =
 *         [ ["x1" %<-% mkVInt (1,2); "dst" %<-% mkVInt (1,2)];
 *           ["x1" %<-% mkVInt (1,2); "dst" %<-% mkVInt (1,2)];
 *           ["x1" %<-% mkVInt (0,2); "dst" %<-% mkVInt (1,2)];
 *           ["x1" %<-% mkVInt (1,2); "dst" %<-% mkVInt (1,2)]
 *         ];
 *     } in
 *   one_table_synth log_schema phys_schema log_rel_succeed
 *   && not (one_table_synth log_schema phys_schema log_rel_fail) *)



(*** Andy's Examples ***)
let keyify = List.map ~f:(fun x -> (x, 0, 3))
let actify = List.map ~f:(fun x ->
             List.map (range 0 3)
               ~f:(fun v -> x %<-% mkVInt(v,2)))

let a = ["w"; "x"; "y"; "z"]
let b = ["a"; "b"; "c"; "d";"e"]
let c = ["p"; "q"]
let d = ["r";"s";"t"]

(* let%test _ =
 *   match cmd_equalable a
 *           ("p" %<-% mkVInt (2,2)
 *            %:% ("q" %<-% mkVInt (2,2))
 *            %:% ("r" %<-% mkVInt (2,2))
 *            %:% ("s" %<-% mkVInt (1,2))
 *            %:% ("t" %<-% mkVInt (0,2)))
 *           ("r" %<-% mkVInt (2,2)
 *            %:% ("s" %<-% mkVInt (1,2))
 *            %:% ("t" %<-% mkVInt (1,2))
 *            %:% ("p" %<-% mkVInt (2,2))
 *            %:% ("q" %<-% mkVInt (2,2)))
 *   with
 *   | None -> true
 *   | Some t ->
 *      Printf.printf ("Implementable With %s\n%!") (string_of_test t); false *)

(* let%test _ =
 *   let a_to_c = { keys = keyify a; actions = actify c; constraints = [] } in
 *   let b_to_d = { keys = keyify b; actions = actify d; constraints = [] } in
 *   let log = compose_schemas a_to_c b_to_d in
 *   let phys = compose_schemas b_to_d a_to_c in
 *   let cand_map = compute_candidate_map log phys in
 *   let counts = List.map cand_map ~f:(fun (_, cands) -> List.length cands) in
 *   Printf.printf "Candidate for each act-seq : on average %f, max %d, min %d"
 *     ((float_of_int (List.fold counts ~init:0 ~f:(+))) /. float_of_int(List.length counts))
 *     (List.max_elt ~compare counts |> Option.value_exn)
 *     (List.min_elt ~compare counts |> Option.value_exn)
 *   ; List.iter cand_map
 *       ~f:(fun (aseq, cands) ->
 *         Printf.printf "%s -> \n%!" (List.reduce_exn aseq ~f:(%:%)|> string_of_cmd);
 *         List.iter cands ~f:(fun (phi, cs) ->
 *             Printf.printf "   %s ==> %s\n%!"
 *               (string_of_test phi)
 *               (List.reduce_exn cs ~f:(%:%)|> string_of_cmd));
 *         Printf.printf "\n\n%!")
 *   ; true one_table_synth log phys ~cand_map:(Some cand_map)
  |> qc ~reps:2 ~size:2 ~table:log*)

(* let%test _ =
 *   let a_to_bc = { keys = keyify a; actions = actify b @ actify c; constraints = [] } in
 *   let b_to_d = { keys = keyify b; actions = actify d; constraints = []} in
 *   let act_bcd = actify b @ actify c @ actify d in
 *   let a_to_bcd = {keys = keyify a; actions = act_bcd;
 *                   constraints = [FD (range 0 (List.length a),
 *                                      range (List.length a) (List.length act_bcd))]
 *                  } in
 *   let log = compose_schemas a_to_bc b_to_d in
 *   let cand_map = compute_candidate_map log a_to_bcd |> Some in
 *   one_table_synth log a_to_bcd ~cand_map
 *   |> qc ~reps:2 ~size:2 ~table:log *)
