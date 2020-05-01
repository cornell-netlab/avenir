open Core
open Util
open Ast
open Tables


type t = value StringMap.t


(* HOLE-BASED optimization*)
let edit_domain (prog : cmd) (edit : Edit.t) =
  get_schema_of_table (Edit.table edit) prog
  |> Option.value_exn
  |> table_vars

let edits_domain (prog : cmd) (edits : Edit.t list) : (string * size) list =
  let open List in
  edits >>= edit_domain prog

let log_edit_domain problem = Problem.(edits_domain (log problem) (log_edits problem))

let make (problem : Problem.t) : t =
    List.fold (Problem.phys problem |> get_tables_vars) ~init:StringMap.empty
      ~f:(fun acc (tbl,vars) ->
        if nonempty_inter vars (log_edit_domain problem) then
          acc
        else
          StringMap.set acc ~key:("?AddRowTo"^tbl) ~data:(mkInt(0,1))
      )

let apply (inj : t) (phi : test) =
  Printf.printf "INJECTION\n%s\n" (string_of_map inj);
  Manip.fixup_test inj phi

let optimization (params : Parameters.t) (problem : Problem.t) : test -> test =
  if params.injection then make problem |> apply else Fn.id


(* END HOLE BASED OPTIMIZATION*)

let compute_hints (params : Parameters.t) (problem : Problem.t) : Hint.t list = failwith ""
