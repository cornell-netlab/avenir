open Ast


let mkZ3Value v ctx = match v with
  | Var s -> Z3.Arithmetic.Integer.mk_const_s ctx s
  | Int i -> Z3.Arithmetic.Integer.mk_numeral_i ctx i

let rec mkZ3Test t ctx = match t with 
  | True -> Z3.Boolean.mk_true ctx
  | False -> Z3.Boolean.mk_false ctx
  | Eq (left, right) -> Z3.Boolean.mk_eq ctx (mkZ3Value left ctx)  (mkZ3Value right ctx)
  | Or (left, right) -> Z3.Boolean.mk_or ctx [(mkZ3Test left ctx);(mkZ3Test right ctx)]
  | And (left, right) -> Z3.Boolean.mk_and ctx [(mkZ3Test left ctx);(mkZ3Test right ctx)]
  | Neg tt -> Z3.Boolean.mk_not ctx (mkZ3Test tt ctx) 

let context = Z3.mk_context [("model", "true")]
let solver = Z3.Solver.mk_solver context None

let checkSMT test =
	let phi = mkZ3Test test context in
	let _ = Z3.Solver.add solver [phi] in 
    match Z3.Solver.check solver [] with
    | UNSATISFIABLE -> Printf.printf "unsat\n"
    | UNKNOWN -> Printf.printf "unknown"
    | SATISFIABLE ->
        match Z3.Solver.get_model solver with
        | None -> ()
        | Some model ->
            Printf.printf "%s\n"
                (Z3.Model.to_string model)