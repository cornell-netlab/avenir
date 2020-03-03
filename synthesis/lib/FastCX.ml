open Core
open Ast
let rec truncate (Apply(table) : Ast.cmd) (program : Ast.cmd) : Ast.cmd option =
  match program with
  | Skip
  | Assign _
  | Assert _
  | Assume _ -> None
  | Seq (c1, c2) -> (match truncate (Apply(table)) c2 with
      | None -> truncate (Apply(table)) c1
      | Some c2' -> Some (Seq (c1, c2')))
  | While _ -> None | Select (Partial, [(b, c1); (Neg b', c2)]) -> (match (truncate (Apply(table)) c1, truncate (Apply(table)) c2) with
      | None, None -> None
      | Some c, None -> Some (Seq (Assume b, c1))
      | None, Some c -> Some (Seq (Assume (Neg b'), c2))
      | Some _, Some _ -> failwith "this should not happen")
  | (Apply t) when (phys_equal t table) -> Some Skip
  | (Apply _) -> None
  | _ -> failwith "unimplemented"