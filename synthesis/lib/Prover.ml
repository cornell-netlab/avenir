(*
 * Interface to Z3
 *)

open Core
open Ast
open Util
   

    
let string_of_map m =
  StringMap.fold ~f:(fun ~key:k ~data:v acc -> ("(" ^ k ^ " -> " ^ (string_of_value v) ^ ") " ^ acc)) m ~init:""
   
let rec mkZ3Value (rhoVars : (string * size) list) (v : expr) ctx (deBruijn : int StringMap.t) (quantified : (string * size) list) : Z3.Expr.expr =
  (* let () = Printf.printf "building z3 value for %s\n%!" (string_of_expr v) in *)
  let open Z3.Arithmetic in
  let binop op e e'=
    op ctx (mkZ3Value rhoVars e ctx deBruijn quantified) (mkZ3Value rhoVars  e' ctx deBruijn quantified)
  in
  match v with
  | Value (Int (i,sz)) ->  Z3.BitVector.mk_numeral ctx (Printf.sprintf "%d" i) sz 
  | Hole (x,sz) | Var (x,sz) ->     
     if List.exists quantified ~f:(fun (x', _) -> x = x') then
       match StringMap.find deBruijn x with
       | None -> failwith (Printf.sprintf "Could not find value for variable %s " x)
       | Some idx -> (* Printf.printf "%s is BOUND\n%!" x; *)
          if sz <= 0 then failwith ("Size for " ^x^ "is "^ (string_of_int sz) ^" < 0")
          else
            Z3.Quantifier.mk_bound ctx idx (Z3.BitVector.mk_sort ctx sz)
     else
       (* (Printf.printf "%s is a constant" x; *)
       if [] = rhoVars then
         ((* Printf.printf "RHOVARS IS EMPTY \n"; *)
          if sz <= 0 then failwith ("Size for " ^x^ "is "^ (string_of_int sz) ^" < 0")
          else
           (Z3.BitVector.mk_const_s ctx x sz))
       else Z3.Expr.mk_app ctx
              (Z3.FuncDecl.mk_func_decl_s ctx x
                 (List.map rhoVars ~f:(fun (_,sz) -> Z3.BitVector.mk_sort ctx sz))
                 (Z3.BitVector.mk_sort ctx sz))
              (List.map rhoVars ~f:(fun x -> mkZ3Value rhoVars (Var x) ctx deBruijn quantified))
  | Plus (e,e') -> binop Z3.BitVector.mk_add e e'
  | Times (e, e') -> binop Z3.BitVector.mk_mul e e'
  | Minus (e, e') -> binop Z3.BitVector.mk_sub e e'

let emptyset ctx sz = Z3.Z3Array.mk_const_array ctx (Z3.BitVector.mk_sort ctx sz) (Z3.Boolean.mk_false ctx)
                 
                      
let rec mkZ3Test (rhoVars : (string * size) list) (t : test) ctx deBruijn quantified =
  (* let () = Printf.printf "Building z3 test for %s \n %!" (string_of_test t) in *)
  let z3_value (v : expr) = mkZ3Value rhoVars v ctx deBruijn quantified in
  let z3_test t = mkZ3Test rhoVars t ctx deBruijn quantified in 
  match t with 
  | True -> Z3.Boolean.mk_true ctx
  | False -> Z3.Boolean.mk_false ctx
  | Eq (left, right) ->
     (* Printf.printf "%s = %s\n%!" (string_of_expr left) (string_of_expr right); *)
     Z3.Boolean.mk_eq ctx (z3_value left) (z3_value right)
  | Le (left, right) ->
     (* Printf.printf "%s < %s" (string_of_expr left) (string_of_expr right); *)
     Z3.BitVector.mk_ule ctx (z3_value left) (z3_value right)                          
  | Or (left, right) -> Z3.Boolean.mk_or ctx   [(z3_test left); (z3_test right)]
  | And (left, right) -> Z3.Boolean.mk_and ctx [(z3_test left); (z3_test right)]
  | Impl (left, right) -> Z3.Boolean.mk_implies ctx (z3_test left) (z3_test right)
  | Iff (left, right) -> Z3.Boolean.mk_eq ctx (z3_test left) (z3_test right)
  | Neg tt -> Z3.Boolean.mk_not ctx (z3_test tt) 


let context = Z3.mk_context [("model", "true")]
let satsolver _ = Z3.Solver.mk_solver_t context (Z3.Tactic.mk_tactic context "ufbv")
let vdsolver _ = Z3.Solver.mk_solver_t context (Z3.Tactic.mk_tactic context "qfbv")
let maxsatsolver _ = Z3.Optimize.mk_opt context
let solver = satsolver

let mk_deBruijn vars : int StringMap.t =
  let ctr = ref 0 in
  let max = List.length vars - 1 in
  List.fold vars ~init:StringMap.empty
    ~f:(fun db_map v ->
      let db_map' = StringMap.set db_map ~key:v ~data:(max - !ctr) in
      ctr := !ctr + 1;
      db_map'
    )

let bind_vars typ ctx vs formula =
  let open Z3 in
  let open Quantifier in
  let types = List.map vs ~f:(fun (v,sz) ->
                  if String.get v 0 |> Char.is_uppercase then
                    Z3Array.mk_sort ctx (BitVector.mk_sort ctx sz) (Boolean.mk_sort ctx)
                  else
                      BitVector.mk_sort ctx sz) in
  let names = List.map vs ~f:(fun (v,_) -> Symbol.mk_string ctx v) in
  match typ with
  | `All -> mk_forall ctx types names formula (Some 1) [] [] None None
            |> expr_of_quantifier
  | `Exists ->
     mk_exists ctx types names (formula) (Some 1) [] [] None None
     |> expr_of_quantifier



let initZ3Test ctx test funcArgs bindable : Z3.Expr.expr =
  (* let () = Printf.printf "Free Variables are \n%!";
   *          List.iter bindable ~f:(fun (v,sz) -> Printf.printf "\t%s#%d\n%!" v sz) in *)
  let phi = mkZ3Test funcArgs test ctx (mk_deBruijn (List.map ~f:fst bindable)) bindable
  in
  (bind_vars `All ctx bindable phi)
    
          
let initSolver typ solver ctx test =
  (* Printf.printf "SENDING TEST TO Z3: %s\n%!" (sexp_string_of_test test); *)
  match typ with
  | `Sat ->
     [initZ3Test ctx test [] (free_vars_of_test test)]
     |> Z3.Solver.add solver
  | `Valid ->
     initZ3Test ctx test [] (holes_of_test test)
     |> Z3.Boolean.mk_not ctx
     |> List.return |> Z3.Solver.add solver
  | _ -> failwith "uh oh"



let parse_int str =
  let parse_z3string str = int_of_string ("0" ^ String.chop_prefix_exn str ~prefix:"#")in
  if String.is_prefix str ~prefix:"#"
  then if String.is_prefix str ~prefix:"#b"
       then (parse_z3string str, (String.length str - 2))
       else
         let nchars = String.length str - 2 in
         if nchars <= 0
         then failwith ("Error parsing int " ^ str ^ " length is " ^ string_of_int nchars)
         else
         (parse_z3string str, ((String.length str - 2) * 4))
  else (int_of_string str, int_of_float((2. ** (float_of_int (int_of_string str)))) -1)
(*
 Converts a Z3 expression to Motley expression 
*)
let mkMotleyExpr expr =	
  match Z3.AST.get_ast_kind (Z3.Expr.ast_of_expr expr) with
  | NUMERAL_AST ->
     Z3.Expr.to_string expr |> parse_int |> Int
  | APP_AST  
  | VAR_AST   
  | _  -> raise (Failure ("Prover: still not supporting: " ^ (Z3.AST.to_string (Z3.Expr.ast_of_expr expr)) ^ "\n"))

(*
 Converts a Z3 model to a map from String to Motley value
*) 
let mkMotleyModel model = 
  let consts = Z3.Model.get_const_decls model in
  let name_vals: (string * value) list = (List.map
    ~f:(fun func_decl ->
      let name = (Z3.Symbol.get_string (Z3.FuncDecl.get_name func_decl)) in
      let value = Z3.Model.get_const_interp model func_decl in
      (match value with
       | Some v -> name, (mkMotleyExpr v) 
       | None -> raise (Failure "Prover: empty model")))
    consts)
  in
  StringMap.of_alist_exn(name_vals)

let toZ3String test =
  let mySolver = satsolver () in
  let _ = initSolver `Sat mySolver context test in
  Printf.sprintf "%s" (Z3.Solver.to_string mySolver)


let parse_results holes results =
  if String.is_substring results ~substring:"sat"
  then
    match String.substr_index results ~pattern:"(model" with
    | None -> (Z3.Solver.SATISFIABLE, None)
    | Some i ->
       let model_str = String.drop_prefix results i in       
       let model_lines = String.split_lines model_str in       
       (* let model = List.fold holes ~init:StringMap.empty
        *               ~f:(fun acc (hole,_) ->
        *                 match List.findi model_lines ~f:(fun _ line ->
        *                           String.is_substring line ~substring:hole
        *                         ) with
        *                 | None -> acc
        *                 | Some (i,_) ->
        *                    match List.nth  model_lines (i+1) with
        *                    | None -> acc
        *                    | Some str ->
        *                       let (v,sz) = String.lstrip str |> String.rstrip ~drop:((=) ')') |> parse_int in
        *                       Printf.printf "Extracting %s |-> %d from model \n%!" hole v;
        *                       StringMap.set acc hole (Int(v,sz))
        *               ) in *)
       let model = List.foldi model_lines ~init:StringMap.empty
                     ~f:(fun i acc_model line ->
                       if String.is_substring ~substring:"define-fun" line
                       then let key = List.nth_exn (String.lstrip line |> String.split ~on:' ') 1 in
                            match List.nth  model_lines (i+1) with
                            | None -> acc_model
                            | Some str ->
                               let (v,sz) = String.lstrip str |> String.rstrip ~drop:((=) ')') |> parse_int in
                               Printf.printf "Extracting %s |-> %d from model \n%!" key v; 
                               StringMap.set acc_model key (Int(v,sz))
                       else acc_model) in
       (Z3.Solver.SATISFIABLE, Some model)
  else (Z3.Solver.UNSATISFIABLE, None)                 
         
(*Check MaxSMT*)
let check_opt (test : test ) =
  let solver = Z3.Optimize.mk_opt context in
  let holes = holes_of_test test in
  let constraints =
    List.fold holes
      ~init:[]
      ~f:(fun acc (hi, sz) ->        
        if String.is_suffix hi ~suffix:"_hi"
        then
          let hivar = String.rev hi |> String.substr_replace_first ~pattern:"ih_" ~with_:"" |> String.rev in
          List.fold holes
            ~init:acc
            ~f:(fun acc' (lo, sz) ->
              if String.is_suffix lo ~suffix:"_lo"
              then
                let lovar = String.rev lo |> String.substr_replace_first ~pattern:"ol_" ~with_:"" |> String.rev in
                if hivar = lovar
                then acc @ [mkZ3Value [] (Minus(Hole(hi,sz), Hole(lo,sz))) context StringMap.empty []]
                else acc'
              else acc'
            )
        else acc
      )
  in
  [initZ3Test context test [] (free_vars_of_test test) ]
  |> Z3.Optimize.add solver;
  List.iter constraints ~f:(fun e ->
      Z3.Optimize.minimize solver e |> ignore
    );
  Core.Out_channel.write_all "query.smt" ~data:(Printf.sprintf "%s\n(get-model)" (Z3.Optimize.to_string solver));
  (* Printf.printf "OPTIMAL SOLVER :\n %s \n\n%!" (Z3.Optimize.to_string solver); *)
  try Shell.run_full "/usr/bin/z3" ["-smt2";"query.smt" ]|> parse_results holes
  with _ -> (Z3.Solver.UNSATISFIABLE, None)
  
            

                 
(*
 Checks SMT query. Returns either None (UNSAT) or SAT (model map) 
*)
let check _ typ test =
  (* let mySolver = solver () in *)
  let response,model, dur = match typ with
    | `MinSat -> let r, m = check_opt test in r, m,  Time.Span.zero
    | _ -> 
       let mySolver = match typ with
         | `Valid -> vdsolver ()
         | `Sat -> satsolver ()
         | _ -> failwith "impossible"
       in
       let st = Time.now() in
       let _ = Z3.Solver.push mySolver;
               initSolver typ mySolver context test in
       (* let _ = Printf.printf "SOLVER:\n%s\n%!" (Z3.Solver.to_string mySolver) in *)
       let response = Z3.Solver.check mySolver [] in
       let dur = Time.(diff (now()) st) in
       (* let _ = Printf.printf "Motley formula:\n%s\nZ3 formula:\n%s\n" (string_of_test test) (Z3.Solver.to_string mySolver) in *)
       let model =
         if response = SATISFIABLE
         then match Z3.Solver.get_model mySolver with
              | None -> None
              | Some m ->
                 Printf.printf "SAT: %s \n%!" (Z3.Model.to_string m);
                 Some (mkMotleyModel m)
         else None in
       response, model , dur in
     match response, model, dur  with  
     | UNSATISFIABLE, _, _ ->
        (* Printf.printf "UNSAT\n%!"; *)
        (* begin match typ with
         * | `Valid -> (None,dur)
         * | `Sat -> (Z3.Solver.pop mySolver 1;
         *            (None, dur))
         * end *)
        None, dur
     | UNKNOWN,_,_ ->
        (* Printf.printf "UNKNOWN:\n \t%s \n%!" (Z3.Solver.get_reason_unknown mySolver); *)
        (* Z3.Solver.pop mySolver 1; *)
        (None, dur)
     | SATISFIABLE, model, dur ->
        match model with 
        | Some model ->
           (* Printf.printf "SAT: %s \n%!" (Z3.Model.to_string m);
            * let model = mkMotleyModel m in *)
           (* Z3.Solver.pop mySolver 1; *)
           (Some model, dur)
        | None ->
           (* Z3.Solver.pop mySolver 1; *)
           (None, dur)

(* Checks SMT Query for validity. Returns None (VALID) or Some model (Counter Example) *)          
let check_valid mySolver test = check mySolver `Valid test

let rec all_agree (vs :  (string * size) list) =
  match vs with
  | [] -> true
  | ((v,sz)::vs) -> if List.exists vs ~f:(fun (v',sz') ->
                           if v = v' && sz <> sz' then
                             (Printf.printf "[ERROR] %s found with multiple sizes %d & %d" v sz sz';
                              true)
                           else
                             false
                         )
                    then false
                    else all_agree vs
                  


(** Collect const_s pktAndMtchs for all fvs in logUniv & in realUniv 
  * Produce const_s addedRow for all fvs in logOneUniv not in C
  * Produce quantified mods for all fvs in realNExist not in C
  * send the following to Z3
  * const addedRow
  * assert (forall (mods) (exists (pktAndMtchs) (
  *    logUniv <=>realUniv /\ 
  *    logOneUniv <=/=> realNExist)))
  * This will return SAT & a counter example, in which case we conclude invalid,
  * or it will return UNSAT and we will conclude VALID
  *)
let check_valid_impl (logUniv : test) (realUniv : test) (logOneUniv : test) (realNExist : test) =
  let () = Printf.printf "check\n %s \n" (string_of_test logUniv) in (*" is equal to \n %s is \n%s\n equal to \n %s\n??????????\n%!" (string_of_test logUniv) (string_of_test realUniv) (string_of_test logOneUniv) (string_of_test realNExist)in *)
  (*the free variables in the original program*)
  let pktAndMtchs = List.dedup_and_sort ~compare (free_vars_of_test logUniv @ free_vars_of_test realUniv) in
  let () = Printf.printf "pktAndMtchs == \n";
           List.iter pktAndMtchs ~f:(fun (v,_) -> Printf.printf "\t%s\n%!" v) 
  in
  (* The free variables in the row added to the logical program *)
  let addedRow = List.dedup_and_sort ~compare (difference (free_vars_of_test logOneUniv) pktAndMtchs) in
  let () = Printf.printf "addedRow == \n ";
           List.iter addedRow ~f:(fun (v,_) -> Printf.printf "\t%s\n%!" v) in
  (* the free variables in the modification to the realProgram *)
  let mods = List.dedup_and_sort ~compare (difference (free_vars_of_test realNExist) pktAndMtchs) in
  let () = Printf.printf "mods == \n";
           List.iter mods ~f:(fun (v,_) -> Printf.printf "\t%s\n%!" v) 
  in
  let test = logUniv %<=>% realUniv %&% !%(logOneUniv %<=>% realNExist) in
  let allBound = mods @ pktAndMtchs in
  let indices = mk_deBruijn (List.map ~f:fst allBound) in  
  let () = Printf.printf "debruijn indicies are \n%!";
           StringMap.iteri indices ~f:(fun ~key ~data -> Printf.printf "\t%s -> %d\n%!" key data) in
  let phi = mkZ3Test [] test context indices allBound in
  let mySolver = satsolver () in
  (* let () = Printf.printf "the test, pre-binding \n %s \n%!" (Z3.Expr.to_string phi) in *)
  let () = Printf.printf "=============\nthe inner bound expression\n %s \n =============="
             (Z3.Expr.to_string (bind_vars `Exists context pktAndMtchs phi)) in
  let () = Z3.Solver.add mySolver [
               bind_vars `Exists context pktAndMtchs phi
               |> bind_vars `All context mods
             ]
  in
  let _ = Printf.printf "SOLVER:\n%s\n%!" (Z3.Solver.to_string mySolver) in
  let response = Z3.Solver.check mySolver [] in
  match response with
  | UNSATISFIABLE -> Printf.printf "UNSAT\n%!"; None
  | UNKNOWN  -> failwith(Printf.sprintf "UNKNOWN! %s" (Z3.Solver.get_reason_unknown mySolver))
  | SATISFIABLE ->
    match Z3.Solver.get_model mySolver with 
    | Some m ->
      (* let model = mkMotleyModel m in *)
       Printf.printf "SAT: %s \n%!" (Z3.Model.to_string m);       
       Some (StringMap.empty)
    | None -> None



let checkSynthProblem rhoVars logwp realwp log1wp realNwp =
  let mySolver = satsolver () in
  let ctx = context in
  let eqAssm = logwp %<=>% realwp in
  let eqAssm_free = free_vars_of_test eqAssm in
  let (pktVars, matchVars) =
    (List.partition_map eqAssm_free
       ~f:(fun (x,sz) ->
         if String.get x 0 = 'e'
         then `Fst (x,sz)
         else `Snd (x,sz))) in
  let eqAssm_indices = mk_deBruijn (List.map (matchVars @ pktVars) ~f:fst) in
  let eqAssmZ3 =
    mkZ3Test [] eqAssm ctx eqAssm_indices (matchVars @ pktVars)
    |> bind_vars `All ctx pktVars
  in

  let editEq = log1wp %<=>% realNwp in
  let editVars = difference (free_vars_of_test editEq) (matchVars) in
  let editEq_indices = mk_deBruijn (List.map (editVars @ matchVars) ~f:fst) in
  let editEqZ3 =
    mkZ3Test rhoVars editEq ctx editEq_indices (editVars @ matchVars)
  in

  let phi = 
    Z3.Boolean.mk_implies ctx (eqAssmZ3) (editEqZ3)
    |> bind_vars `All ctx (editVars @ matchVars) in

  Z3.Solver.add mySolver [ phi ];
  Printf.printf "SOLVER:\n%s\n%!" (Z3.Solver.to_string mySolver);  
  match Z3.Solver.check mySolver [] with
  | UNSATISFIABLE -> Printf.printf "UNSAT\n%!"; None
  | UNKNOWN  -> failwith(Printf.sprintf "UNKNOWN! %s" (Z3.Solver.get_reason_unknown mySolver))
  | SATISFIABLE ->
     match Z3.Solver.get_model mySolver with 
     | Some m ->
        (* let model = mkMotleyModel m in *)
        Printf.printf "SAT: %s \n%!" (Z3.Model.to_string m);       
        Some (StringMap.empty)
     | None -> None
                 
                 
                 
                 
                 
