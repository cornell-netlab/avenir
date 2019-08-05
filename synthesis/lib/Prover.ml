(*
 * Interface to Z3
 *)

open Core
open Ast

   
module StringMap = Map.Make (String)
    
let string_of_map m =
  StringMap.fold ~f:(fun ~key:k ~data:v acc -> ("(" ^ k ^ " -> " ^ (string_of_value v) ^ ") " ^ acc)) m ~init:""
   
let mkZ3Value typ v ctx (deBruijn : int StringMap.t) : Z3.Expr.expr =
  let open Z3.Arithmetic in
  match v, typ with
  | Int i, _ ->
    Integer.mk_numeral_i ctx i
  | Hole x, `Sat
  | Var x, `Valid ->
    Integer.mk_const_s ctx x
  | Var x, `Sat
  | Hole x, `Valid  ->
    match StringMap.find deBruijn x with
    | None -> failwith ( "unbound variable " ^ x) 
    | Some x' -> Z3.Quantifier.mk_bound ctx x' (Integer.mk_sort ctx)

let rec mkZ3Test typ t ctx deBruijn =
  let z3_value (v : value) = mkZ3Value typ v ctx deBruijn in
  let z3_test t = mkZ3Test typ t ctx deBruijn in 
  match t with 
  | True -> Z3.Boolean.mk_true ctx
  | False -> Z3.Boolean.mk_false ctx
  | LocEq _ -> failwith "dont know how to insert locations into tests"
  | Eq (left, right) -> Z3.Boolean.mk_eq ctx   (z3_value left) (z3_value right)
  | Lt (left, right) -> Z3.Arithmetic.mk_lt ctx   (z3_value left) (z3_value right)
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

let bind_vars ctx vs formula =
  let open Z3 in
  let types = List.map vs ~f:(fun _ -> Arithmetic.Integer.mk_sort ctx) in
  let names = List.map vs ~f:(Symbol.mk_string ctx) in
  let q = Quantifier.mk_forall ctx types names formula (Some 1) [] [] None None in
  Quantifier.expr_of_quantifier q


  
let initSolver typ solver ctx test =
  Printf.printf "SENDING TEST TO Z3: %s\n%!" (sexp_string_of_test test);
  let init bindable ctor =
    let phi = mkZ3Test typ test ctx (mk_deBruijn bindable) in
    Z3.Solver.add solver [ctor (bind_vars ctx bindable phi)]
  in
  match typ with
  | `Sat ->
    init (free_vars_of_test test) (fun x -> x)
  | `Valid ->
    init (holes_of_test test) (Z3.Boolean.mk_not ctx)
  
(*
 Converts a Z3 expression to Motley expression 
*)
let mkMotleyExpr expr =	
  match Z3.AST.get_ast_kind (Z3.Expr.ast_of_expr expr) with
  | NUMERAL_AST -> Int (int_of_string (Z3.Expr.to_string expr))
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
  | UNSATISFIABLE | UNKNOWN -> Printf.printf "UNSAT\n%!"; None
  | SATISFIABLE ->
    match Z3.Solver.get_model mySolver with 
    | Some m ->
      let model = mkMotleyModel m in
      Printf.printf "SAT: %s \n%!" (string_of_map model);
      Some model
    | None -> None

(* Checks SMT Query for validity. Returns None (VALID) or Some model (Counter Example) *)          
let check_valid test = check `Valid (test)
  


