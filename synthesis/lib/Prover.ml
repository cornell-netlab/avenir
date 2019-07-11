open Core
open Ast

   
module StringMap = Map.Make (String) 
   
let mkZ3Value v ctx (deBruijn : int StringMap.t) : Z3.Expr.expr =
  let open Z3.Arithmetic in
  match v with
  | Hole h -> Integer.mk_const_s ctx h
  | Int i  -> Integer.mk_numeral_i ctx i
  | Var v  -> match StringMap.find deBruijn v with
              | None -> failwith ( "unbound variable " ^ v) 
              | Some x -> Z3.Quantifier.mk_bound ctx x (Integer.mk_sort ctx)

let rec mkZ3Test t ctx deBruijn =
  let z3_value v = mkZ3Value v ctx deBruijn in
  let z3_test t = mkZ3Test t ctx deBruijn in 
  match t with 
  | True -> Z3.Boolean.mk_true ctx
  | False -> Z3.Boolean.mk_false ctx
  | Eq (left, right) -> Z3.Boolean.mk_eq ctx   (z3_value left) (z3_value right)
	| Lt (left, right) -> Z3.Arithmetic.mk_lt ctx   (z3_value left) (z3_value right)
  | Or (left, right) -> Z3.Boolean.mk_or ctx   [(z3_test left); (z3_test right)]
  | And (left, right) -> Z3.Boolean.mk_and ctx [(z3_test left); (z3_test right)]
  | Neg tt -> Z3.Boolean.mk_not ctx (z3_test tt) 

let context = Z3.mk_context [("model", "true"); ("unsat_core", "true")]
let solver = Z3.Solver.mk_solver context None

let mk_deBruijn vars : int StringMap.t =
  let ctr = ref 0 in
  List.fold vars ~init:StringMap.empty
    ~f:(fun db_map v ->
      let db_map' = StringMap.set db_map ~key:v ~data:!ctr in
      ctr := !ctr + 1;
      db_map'
    )

let bind_vars ctx vs formula =
  let open Z3 in
  let types = List.map vs ~f:(fun _ -> Arithmetic.Integer.mk_sort ctx) in
  let names = List.map vs ~f:(Symbol.mk_string ctx) in
  let q = Quantifier.mk_forall ctx types names formula (Some 1) [] [] None None in
  Quantifier.expr_of_quantifier q

  
let initSolver ctx test =
  let bindable_vars = free_vars_of_test test in
  let phi = mkZ3Test test ctx (mk_deBruijn bindable_vars) in
  Z3.Solver.add solver [bind_vars ctx bindable_vars phi]

  
let checkSMT expect test =
  let _ = initSolver context test in
  let response = Z3.Solver.check solver [] in
  begin match response with
  | UNSATISFIABLE -> Printf.printf "unsat\n"
  | UNKNOWN -> Printf.printf "unknown"
  | SATISFIABLE ->
     match Z3.Solver.get_model solver with
     | None -> ()
     | Some model ->
        Printf.printf "%s\n"
          (Z3.Model.to_string model)
  end;
  expect = response


let checkModel test =
  let _ = initSolver context test in
  let response = Z3.Solver.check solver [] in
  match response with
  | UNSATISFIABLE | UNKNOWN -> None
  | SATISFIABLE -> Z3.Solver.get_model solver

let checkCE test =
  let _ = initSolver context test in
  let response = Z3.Solver.check solver [] in
  match response with
  | SATISFIABLE | UNKNOWN -> None
  | UNSATISFIABLE -> Some (Z3.Solver.get_unsat_core solver)
  

let synthesize p q =
  Printf.printf "Synthesize %s\n with %s \n"
    (string_of_expr p)
    (string_of_expr q)
