open Core
open Ast
open Util

let name = ref "a"
let incr c = Char.to_int c + 1 |> Char.of_int_exn

let rec get_fresh_name () =
  let freshname = !name in
  if freshname = "" then begin
      name := "a";
      get_fresh_name ()
    end
  else
    let c = String.prefix !name 1 |> Char.of_string in
    if c = 'z' then
      name := String.make (String.length freshname + 1) 'a'
    else
      name := Printf.sprintf "%c%s" (incr c) (String.drop_prefix freshname 1);
    freshname


let rec shorten_expr (bht : Bishtbl.t) (e : expr) : expr =
  let get key = Bishtbl.get bht ~key ~default:get_fresh_name in
  match e with
  | Value _ -> e
  | Var (x, sz) -> Var(get x, sz)
  | Hole (h, sz) -> Hole(get h, sz)
  | Plus (e1,e2) -> mkPlus (shorten_expr bht e1) (shorten_expr bht e2)
  | Minus (e1,e2) -> mkMinus (shorten_expr bht e1) (shorten_expr bht e2)
  | Times (e1,e2) -> mkTimes (shorten_expr bht e1) (shorten_expr bht e2)
  | Mask (e1,e2) -> mkMask (shorten_expr bht e1) (shorten_expr bht e2)

let rec shorten (bht : Bishtbl.t) (t : test) : test =
  match t with
  | True -> True
  | False -> False
  | Eq (e1,e2) -> shorten_expr bht e1 %=% shorten_expr bht e2
  | Le (e1,e2) -> shorten_expr bht e1 %<=% shorten_expr bht e2
  | And (t1,t2) -> shorten bht t1 %&% shorten bht t2
  | Or (t1,t2) -> shorten bht t1 %+% shorten bht t2
  | Iff (t1,t2) -> shorten bht t1 %<=>% shorten bht t2
  | Impl (t1,t2) -> shorten bht t1 %=>% shorten bht t2
  | Neg t1 -> mkNeg @@ shorten bht t1



let rec unshorten_expr (bht : Bishtbl.t) (e : expr) : expr =
  let unget key = Bishtbl.get_back bht ~key in
  match e with
  | Value _ -> e
  | Var (x, sz) -> Var(unget x, sz)
  | Hole (h, sz) -> Hole(unget h, sz)
  | Plus (e1,e2) -> mkPlus (unshorten_expr bht e1) (unshorten_expr bht e2)
  | Minus (e1,e2) -> mkMinus (unshorten_expr bht e1) (unshorten_expr bht e2)
  | Times (e1,e2) -> mkTimes (unshorten_expr bht e1) (unshorten_expr bht e2)
  | Mask (e1,e2) -> mkMask (unshorten_expr bht e1) (unshorten_expr bht e2)

let rec unshorten (bht : Bishtbl.t) (t : test) : test =
  match t with
  | True -> True
  | False -> False
  | Eq (e1,e2) -> unshorten_expr bht e1 %=% unshorten_expr bht e2
  | Le (e1,e2) -> unshorten_expr bht e1 %<=% unshorten_expr bht e2
  | And (t1,t2) -> unshorten bht t1 %&% unshorten bht t2
  | Or (t1,t2) -> unshorten bht t1 %+% unshorten bht t2
  | Iff (t1,t2) -> unshorten bht t1 %<=>% unshorten bht t2
  | Impl (t1,t2) -> unshorten bht t1 %=>% unshorten bht t2
  | Neg t1 -> mkNeg @@ unshorten bht t1


let rec unshorten_model (bht : Bishtbl.t) (m : value StringMap.t) : value StringMap.t =
  StringMap.fold m ~init:StringMap.empty
    ~f:(fun ~key ~data acc ->
      StringMap.add_exn acc ~key:(Bishtbl.get_back bht key) ~data
    )
