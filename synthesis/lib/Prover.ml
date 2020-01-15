(*
 * Interface to Z3
 *)

open Core
open Ast
open Util

   
module StringMap = Map.Make (String)
    
let string_of_map m =
  StringMap.fold ~f:(fun ~key:k ~data:v acc -> ("(" ^ k ^ " -> " ^ (string_of_value1 v) ^ ") " ^ acc)) m ~init:""
   
let rec mkZ3Value (rhoVars : (string * size) list) (v : expr1) ctx (deBruijn : int StringMap.t) (quantified : (string * size) list) : Z3.Expr.expr =
  (* let () = Printf.printf "building z3 value for %s\n%!" (string_of_expr1 v) in *)
  let open Z3.Arithmetic in
  let binop op e e'=
    op ctx [mkZ3Value rhoVars e ctx deBruijn quantified; mkZ3Value rhoVars  e' ctx deBruijn quantified]
  in
  match v with
  | Value1 (Int (i,sz)) ->  Z3.BitVector.mk_numeral ctx (Printf.sprintf "%d" i) sz 
  | Value1 (VTuple vs ) ->
     concatMap vs ~c:(Z3.BitVector.mk_concat ctx) ~f:(fun e -> mkZ3Value rhoVars (Value1 e) ctx deBruijn quantified)
  | Tuple es ->
     concatMap es ~c:(Z3.BitVector.mk_concat ctx) ~f:(fun e -> mkZ3Value rhoVars e ctx deBruijn quantified)
  | Hole1 (x,sz) | Var1 (x,sz) ->     
     if List.exists quantified ~f:(fun (x', _) -> x = x') then
       match StringMap.find deBruijn x with
       | None -> failwith (Printf.sprintf "Could not find value for variable %s " x)
       | Some idx -> (* Printf.printf "%s is BOUND\n%!" x; *)
          Z3.Quantifier.mk_bound ctx idx (Z3.BitVector.mk_sort ctx sz)
     else
       (* (Printf.printf "%s is a constant" x; *)
       if [] = rhoVars then
         ((* Printf.printf "RHOVARS IS EMPTY \n"; *)
           (Z3.BitVector.mk_const_s ctx x sz))
       else Z3.Expr.mk_app ctx
              (Z3.FuncDecl.mk_func_decl_s ctx x
                 (List.map rhoVars ~f:(fun (_,sz) -> Z3.BitVector.mk_sort ctx sz))
                 (Z3.BitVector.mk_sort ctx sz))
              (List.map rhoVars ~f:(fun x -> mkZ3Value rhoVars (Var1 x) ctx deBruijn quantified))
  | Plus (e,e') -> binop mk_add e e'
  | Times (e, e') -> binop mk_mul e e'
  | Minus (e, e') -> binop mk_sub e e'

let emptyset ctx sz = Z3.Z3Array.mk_const_array ctx (Z3.BitVector.mk_sort ctx sz) (Z3.Boolean.mk_false ctx)

let rec value2_to_list (set : value2) : value1 list =
  match set with
  | Empty -> []
  | VSingle v -> [v]
  | VUnion (setL,setR) -> value2_to_list setL @ value2_to_list setR
                 
let rec mkZ3Value2 rhoVars set ctx (deBruijn : int StringMap.t) quantified : Z3.Expr.expr =
  let arraySort sz = (Z3.Z3Array.mk_sort ctx
                        (Z3.BitVector.mk_sort ctx sz)
                        (Z3.Boolean.mk_sort ctx)) in
  let () = Printf.printf "Making Array Sort %s\n %!" (Z3.Sort.to_string (arraySort 4)) in
  match set with
  | Value2 (vSet) -> List.fold (value2_to_list vSet) ~init:(emptyset ctx (size_of_value2 vSet))
                       ~f:(fun acc v -> Z3.Z3Array.mk_store ctx acc (mkZ3Value rhoVars (Value1 v) ctx deBruijn quantified) (Z3.Boolean.mk_true ctx))
     
  | Var2 (x,sz) | Hole2 (x,sz) ->
     if List.exists ~f:(fun (x',_) -> x = x') quantified then
       begin match StringMap.find deBruijn x with
       | None -> let () =
                   Printf.printf "DE_BRUIJN (%d elements):\n%!" (StringMap.length deBruijn);
                   StringMap.iteri deBruijn ~f:(fun ~key ~data ->
                       Printf.printf "\t %s -> %d\n%!" key data) in
                 Printf.sprintf ("unbound second-order variable %s") x |> failwith
       | Some x' ->
          Z3.Quantifier.mk_bound ctx x' (arraySort sz)
       end
     else
       Z3.Z3Array.mk_const_s ctx x (Z3.BitVector.mk_sort ctx sz) (Z3.Boolean.mk_sort ctx)
  | Single e ->
     Z3.Z3Array.mk_store ctx (emptyset ctx (size_of_expr1 e)) (mkZ3Value rhoVars e ctx deBruijn quantified) (Z3.Boolean.mk_true ctx)
  | Union (s,s') -> Z3.Z3Array.mk_map ctx
                         (Z3.Expr.get_func_decl (Z3.Boolean.mk_and ctx []))
                         [ mkZ3Value2 rhoVars s ctx deBruijn quantified
                         ; mkZ3Value2 rhoVars s' ctx deBruijn quantified]
                      
let rec mkZ3Test (rhoVars : (string * size) list) (t : test) ctx deBruijn quantified =
  (* let () = Printf.printf "Building z3 test for %s \n %!" (string_of_test t) in *)
  let z3_value (v : expr1) = mkZ3Value rhoVars v ctx deBruijn quantified in
  let z3_value2 (set : expr2) = mkZ3Value2 rhoVars set ctx deBruijn quantified in 
  let z3_test t = mkZ3Test rhoVars t ctx deBruijn quantified in 
  match t with 
  | True -> Z3.Boolean.mk_true ctx
  | False -> Z3.Boolean.mk_false ctx
  | LocEq _ -> failwith "dont know how to insert locations into tests"
  | Eq (left, right) -> Z3.Boolean.mk_eq ctx   (z3_value left) (z3_value right)
  | Lt (left, right) -> Z3.Arithmetic.mk_lt ctx   (z3_value left) (z3_value right)
  | Member (expr, set) ->
     let z = Z3.Z3Array.mk_select ctx (z3_value2 set) (z3_value expr) in
     let () = Printf.printf "Making Z3 Membership query for \n \t %s \n%!" (string_of_test t) in
     let () = Printf.printf "Produce \n \t %s \n %!" (Z3.Expr.to_string z) in
     z
                          
  | Or (left, right) -> Z3.Boolean.mk_or ctx   [(z3_test left); (z3_test right)]
  | And (left, right) -> Z3.Boolean.mk_and ctx [(z3_test left); (z3_test right)]
  | Neg tt -> Z3.Boolean.mk_not ctx (z3_test tt) 


let context = Z3.mk_context [("model", "true")]
let solver _ = Z3.Solver.mk_solver context None

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


  
let initSolver typ solver ctx test =
  (* Printf.printf "SENDING TEST TO Z3: %s\n%!" (sexp_string_of_test test); *)
  let init funcArgs bindable ctor =
    let () = Printf.printf "Free Variables are \n%!";
             List.iter bindable ~f:(fun (v,sz) -> Printf.printf "\t%s#%d\n%!" v sz) in
    let phi = mkZ3Test funcArgs test ctx (mk_deBruijn (List.map ~f:fst bindable)) bindable
    in
    Z3.Solver.add solver [ctor (bind_vars `All ctx bindable phi)]
  in
  match typ with
  | `Sat ->
    init [] (free_vars_of_test test) (fun x -> x)
  | `Valid ->
     init [] (holes_of_test test) (Z3.Boolean.mk_not ctx)
  | `Synth rhoVars -> init rhoVars (free_vars_of_test test) (fun x -> x)
  
(*
 Converts a Z3 expression to Motley expression 
*)
let mkMotleyExpr expr =	
  match Z3.AST.get_ast_kind (Z3.Expr.ast_of_expr expr) with
  | NUMERAL_AST -> let i = int_of_string (Z3.Expr.to_string expr) in
                   Int (i, int_of_float((2. ** (float_of_int i) )))
  | APP_AST  
  | VAR_AST   
  | _  -> raise (Failure ("Prover: still not supporting: " ^ (Z3.AST.to_string (Z3.Expr.ast_of_expr expr)) ^ "\n"))

(*
 Converts a Z3 model to a map from String to Motley value
*) 
let mkMotleyModel model = 
  let consts = Z3.Model.get_const_decls model in
  let name_vals: (string * value1) list = (List.map
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
  let mySolver = solver () in
  let _ = initSolver `Sat mySolver context test in
  Printf.sprintf "%s" (Z3.Solver.to_string mySolver)

                        
(*
 Checks SMT query. Returns either None (UNSAT) or SAT (model map) 
*)
let check typ test =
  let mySolver = solver () in
  let _ = initSolver typ mySolver context test in
  let _ = Printf.printf "SOLVER:\n%s\n%!" (Z3.Solver.to_string mySolver) in
  let response = Z3.Solver.check mySolver [] in
  (* Printf.printf "Motley formula:\n%s\nZ3 formula:\n%s\n" (string_of_test test) (Z3.Solver.to_string mySolver); *)
  match response with
  | UNSATISFIABLE -> Printf.printf "UNSAT\n%!"; None
  | UNKNOWN -> Printf.printf "UNKNOWN:\n \t%s \n%!" (Z3.Solver.get_reason_unknown mySolver); None
  | SATISFIABLE ->
    match Z3.Solver.get_model mySolver with 
    | Some _ ->      
      (* let model = mkMotleyModel m in *)
      (* Printf.printf "SAT: %s \n%!" (string_of_map model); *)
       Some StringMap.empty (*model*)
    | None -> None

(* Checks SMT Query for validity. Returns None (VALID) or Some model (Counter Example) *)          
let check_valid test = check `Valid test



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
  let mySolver = solver () in
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
  let mySolver = solver () in
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
                 
                 
                 
                 
                 
