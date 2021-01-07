open Core
open Ast
open Manip
open Util

type action_data = value list

(* Match expressions, action data, action index*)
type t = Match.t list * action_data * int

let equals (ms, ad, i) (ms', ad', i') =
  List.equal Match.equal ms ms'
  && List.equal Stdlib.(=) ad ad'
  && i = i'

let action_data_to_string ad =
  (List.map ad ~f:(string_of_value)
   |> List.reduce ~f:(Printf.sprintf "%s;%s")
   |> Option.value ~default:"")

let to_string ((mtchs, ad, actid) : t) =
  Printf.sprintf "%s,%s,%d"
    (Match.list_to_string mtchs)
    (action_data_to_string ad)
    actid

let list_to_string ?tab:(tab="") rs : string =
  List.fold rs ~init:"" ~f:(fun acc r ->
      Printf.sprintf "%s\n%s%s" acc tab (to_string r)
    )

let test_of_data (tbl : string) (act_id : int) (vars : (string * size) list) (vals : action_data) =
  List.fold2_exn vars vals ~init:True
    ~f:(fun acc (x,sz) v ->
      assert (sz = size_of_value v);
      mkAnd acc @@
        (Hole.action_data_hole tbl act_id x sz %=% Value v)
    )

let model_alist_of_data (tbl : string) (act_id : int) (vars : (string * size) list) (vals : action_data) =
  List.fold2_exn vars vals ~init:[]
    ~f:(fun acc (x,sz) v ->
      assert (sz = size_of_value v);
      acc @
        [Hole.action_data tbl act_id x sz, v]
    )


let intersects (m1s, _,_ : t) (m2s, _, _ : t) : bool =
  List.fold2_exn m1s m2s ~init:true
    ~f:(fun acc m1 m2 -> acc && Match.has_inter m1 m2)

let get_ith_match (i : int) ((ms, _,_) : t) =
  List.nth ms i

let mk_new_row (match_model : Model.t) phys tbl_name data_opt act : t option =
  match get_schema_of_table tbl_name phys with
  | None -> failwith ("Couldnt find keys for table " ^ tbl_name)
  | Some (ks, acts, _) ->
     let keys_holes =
       List.fold ks ~init:(Some [])
         ~f:(fun acc (key, sz, v_opt) ->
           match v_opt with
           | Some _ -> acc
           | None ->
              let hlo,hhi = Hole.match_holes_range tbl_name key in
              match acc,
                    fixup_expr match_model (Hole(hlo, sz)),
                    fixup_expr match_model (Hole(hhi, sz))
              with
              | None, _,_ -> None
              | Some ks, Hole _, Hole _ ->  begin
                  let h, hm = Hole.match_holes_mask tbl_name key in
                  match fixup_expr match_model (Hole(h, sz)),
                        fixup_expr match_model (Hole(hm,sz))
                  with
                  | Hole _,_ ->
                     Some (ks @ [Match.mask_ key (mkInt(0,sz)) (mkInt(0,sz))])
                  | Value v,Hole _ ->
                     Some (ks @ [Match.exact_ key v])
                  | Value v, Value m ->
                     Some (ks @ [Match.mask_ key v m])
                  | _ -> failwith "Model did something weird"
                end
              | Some ks, Value lo, Value hi ->
                 let k = if vleq hi lo
                         then failwith "Low value greater than high value in model from z3"
                         else [Match.between_ key lo hi] in
                 Some (ks @ k)
              | _, _,_ -> failwith "got something that wasn't a model"
         ) in
     let data =
       match data_opt with
       | Some ds -> ds
       | None ->
          match List.nth acts act with
          | None -> []
          | Some (_, params, _) ->
             (* Printf.printf "Params for %s.action[%d] :%s\n%!" tbl_name act
              *   (List.fold params ~init:""
              *      ~f:(fun acc (p,_) -> Printf.sprintf "%s %s" acc p)); *)
             List.fold params ~init:[]
               ~f:(fun acc (p,sz) ->
                 let v =
                   let p_hole = Hole.action_data tbl_name act p sz in
                   match Model.find match_model p_hole  with
                   | None ->
                      Printf.printf "[WARNING] couldn't find action data %s in %s \n%!" p_hole (Model.to_string match_model);
                      Int (Random.int (pow 2 sz) |> Bigint.of_int_exn, sz)
                   | Some v ->
                      v
                 in
                 (* Printf.printf "\t%s -> %s\n" p (string_of_value v); *)
                 acc @ [v]
               )
     in
     match keys_holes with
     | None -> None
     | Some ks -> Some (ks, data, act)
