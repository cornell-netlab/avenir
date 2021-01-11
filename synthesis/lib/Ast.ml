open Core
open Util

let (=) = Stdlib.(=)
let (<) = Stdlib.(<)
let (<=) = Stdlib.(<=)
let (<>) = Stdlib.(<>)

let enable_smart_constructors = true

type size = int

type select_typ =
  | Partial
  | Total
  | Ordered

let string_of_select_typ styp =
  match styp with
  | Partial -> "partial"
  | Total -> "total"
  | Ordered -> "ordered"

let sexp_string_of_select_typ styp =
  String.capitalize (string_of_select_typ styp)

type cmd =
  | Skip
  | Assign of (string * Expr.t)
  | Assume of Test.t
  | Seq of (cmd * cmd)
  | Select of (select_typ * ((Test.t * cmd) list))
  | Apply of {name:string;
              keys:(string * size * Value.t option) list;
              actions:((string * (string * size) list * cmd) list);
              default: cmd}

let rec mkSeq first scnd =
  if not enable_smart_constructors then Seq(first, scnd) else
  match first, scnd with
  | Skip, x | x, Skip | Assume True, x | x, Assume False ->
     x
  | first, Seq(scnd1,scnd2) ->
     mkSeq (Seq(first,scnd1)) scnd2
  | _,_ ->
     Seq(first, scnd)


let (%:%) = mkSeq

let mkAssn f v =
  match v with
  | Expr.Var(x, _) when x = f -> Skip
  | _ -> Assign (f, v)
let (%<-%)= mkAssn


let mkAssume t =
  if t = Test.True then Skip else Assume t

let clean_selects_list ss = ss

let mkPartial ss = 
  if not enable_smart_constructors then Select(Partial, ss) else
    let selects = clean_selects_list ss in
    if List.length selects = 0 then
      Skip
    else
      Select (Partial, selects)

let mkTotal ss =
  if not enable_smart_constructors then Select(Total, ss) else
  let selects = clean_selects_list ss in
  if List.length selects = 0 then
    mkAssume False
  else
    Select (Total, selects)

let rec assumes_false c =
  let f = fun (_,c) -> assumes_false c in
  match c with
  | Skip | Assign _ -> false
  | Seq (c1,c2) -> assumes_false c1 || assumes_false  c2
  | Assume (t) -> t = False
  | Select (Ordered, cases) ->
     List.for_all cases ~f
  | Apply {actions;default;_} ->
     assumes_false default && List.for_all actions ~f:(fun (_, x, c) -> f(x, c))
  | Select _ -> failwith "deprecated"


let mkOrdered ss =
  if not enable_smart_constructors then Select(Ordered, ss) else
    (*make these all a single pass *)
    let selects = clean_selects_list ss
                  |> List.remove_consecutive_duplicates
                       ~which_to_keep:`First
                       ~equal:(fun (cond,_) (cond',_) -> cond = cond')
                  |> List.filter ~f:(fun (b,c) ->
                         b <> False && not (assumes_false c)
                       )
                  |> List.fold ~init:([],true) ~f:(fun (cases,reachable) (b,c) ->
                         if reachable then
                           (cases @ [b,c], b <> True)
                         else
                           (cases, reachable)
                       )
                  |> fst
    in
    if List.for_all selects ~f:(fun (_,c) -> c = Skip) then
      Skip
    else
      match selects with
      | [] -> Skip
      | [(b,c)] -> (mkAssume b) %:% c
      | _ -> Select (Ordered, selects)

let mkSelect styp =
  match styp with
  | Partial -> mkPartial
  | Total -> mkTotal
  | Ordered -> mkOrdered


let mkApply (name,keys,actions,default) =
  Apply{name;keys = List.map keys ~f:(fun (x,sz) -> (x,sz,None)); actions;default}


let combineSelects e e' =
  match e, e' with
  | Select (xs_typ, xs), Select (ys_typ, ys) ->
    if xs_typ = ys_typ then
      mkSelect xs_typ (xs @ ys)
    else
      failwith ("[ERROR] Cannot combine selects with different types: "
                ^ string_of_select_typ xs_typ
                ^ " and "
                ^ string_of_select_typ ys_typ)
  | _ -> failwith "Can only combine selects statements "

let (%%) = combineSelects

let rec repeat c n =  if n = 0 then "" else c ^ repeat c (n-1)
                    
let rec string_of_cmd ?depth:(depth=0) (e : cmd) : string =
  match e with
  | Skip ->    repeat "\t" depth ^ "skip"
  | Seq (firstdo, thendo) ->
     string_of_cmd ~depth firstdo ^ ";\n "
    ^ string_of_cmd ~depth thendo
  | Assume t ->
    repeat "\t" depth ^
    "assume (" ^ Test.to_string t ^ ")"
  | Assign (field, expr) ->
    repeat "\t" depth ^
    field ^ " := " ^ Expr.to_string expr
  | Select (styp, es) ->
    let modifier = (string_of_select_typ styp) in
    repeat "\t" depth ^
      "if " ^ modifier ^
    List.fold es ~init:"" ~f:(fun str (cond, act)->
        str ^ "\n" ^
        repeat "\t" (depth + 1)
        ^ Test.to_string cond  ^ " ->\n " ^ string_of_cmd ~depth:(depth+2) act ^ " []"
      )
    ^ "\n" ^ repeat "\t" depth ^ "fi"
  | Apply t ->
     repeat "\t" depth ^
       "apply (" ^ t.name ^ ",("
      ^ List.fold t.keys ~init:""
          ~f:(fun str (k,sz,v_opt) ->
            let eq = match v_opt with | None -> "" | Some v -> " = " ^ Value.to_string v in
            str ^ k ^ "#" ^ string_of_int sz ^ eq ^ ",") ^ ")"
      ^ ",(" ^ List.foldi t.actions ~init:""
                ~f:(fun i str a ->
                  str ^ (if i > 0 then " |" else "") ^ " { " ^
                    (if List.length (List.nth_exn t.actions i |> snd3) > 0 then "\\ ("^
                    List.fold (List.nth_exn t.actions i |> snd3) ~init:""
                    ~f:(fun acc (x,sz) -> Printf.sprintf "%s%s#%d," acc x sz)
                    ^") -> " else "")
                    ^ string_of_cmd (trd3 a) ^ "}")
      ^ "), {" ^ string_of_cmd t.default ^ "})"
                                       
            
  
let rec sexp_string_of_cmd e : string =
  let string_select = concatMap 
                        ~f:(fun (cond,act) -> "(" ^ Test.to_sexp_string cond ^ "," ^ sexp_string_of_cmd act ^ ")")
                        ~c:(fun acc d -> acc ^ ";" ^ d) in
  match e with
  | Skip -> "Skip"
  | Seq (p, q) -> "Seq(" ^ sexp_string_of_cmd p ^ "," ^ sexp_string_of_cmd q ^ ")"
  | Assume t -> "Assume(" ^ Test.to_sexp_string t ^ ")"
  | Assign (f,e) -> "Assign(\"" ^ f ^ "\"," ^ Expr.to_sexp_string e ^")"
  | Select (styp,es) ->
    let cases_string = match es with
      | [] -> "[]"
      | _  -> "[" ^ string_select es ^ "]"
    in
    "Select (" ^ sexp_string_of_select_typ styp ^ "," ^ cases_string ^ ")"
  | Apply t ->
     "Apply("
     ^ t.name ^ ",["
     ^ List.fold_left t.keys ~init:"" ~f:(fun str (k,sz,v) -> str ^ ";\"" ^ k ^ "\"," ^ string_of_int sz ^ "," ^
                                                                match v with
                                                                | None -> "None"
                                                                | Some v -> "Some(" ^ Value.to_string v ^ ")")
     ^ "],["
     ^ List.fold_left t.actions ~init:"" ~f:(fun str a -> str ^ ";((SOME ACTION DATA), " ^ sexp_string_of_cmd (trd3 a) ^")")
     ^ "]," ^ sexp_string_of_cmd t.default
     ^ ")"

let get_test_from_assume = function
  | Assume t -> t
  | c -> failwith @@ Printf.sprintf "tried to get test from command %s" (string_of_cmd c)

let rec tables_of_cmd (c:cmd) : string list =
  match c with
  | Skip | Assign _  | Assume _   -> []
  | Seq (c,c') -> tables_of_cmd c @ tables_of_cmd c'
  | Select (_, cs) -> concatMap cs ~c:(@) ~init:(Some []) ~f:(fun (_, c) -> tables_of_cmd c)
  | Apply t -> [t.name]
                                                              

let rec num_nodes_in_cmd c =
  1 +
  match c with
  | Skip -> 0
  | Assign (_, e) -> Expr.num_nodes e
  | Seq (c1,c2) -> num_nodes_in_cmd c1 + num_nodes_in_cmd c2
  | Assume t -> Test.num_nodes t
  | Select (_, cases) -> List.fold cases ~init:0 ~f:(fun acc (b, c) -> acc + Test.num_nodes b + num_nodes_in_cmd c)
  | Apply {keys;actions;default;_} ->
     List.length keys + num_nodes_in_cmd default
     + List.fold actions ~init:0
         ~f:(fun acc (_, data, act) ->
           acc + List.length data + num_nodes_in_cmd act
         )

let rec get_actions (c : cmd) : (string * ((string * size) list) * cmd) list =
  match c with
  | Skip | Assign _ | Assume _ -> []
  | Seq(c1,c2) -> get_actions c1 @ get_actions c2
  | Select (_, cases) ->
     List.bind cases ~f:(fun (_,c) -> get_actions c)
  | Apply {actions; default; _} ->
     ("default", [],default)::actions


let rec num_table_paths (c : cmd) : Bigint.t =
  let open Bigint in
  match c with
  | Skip | Assign _ | Assume _ -> one
  | Seq(c1,c2) -> num_table_paths c1 * num_table_paths c2
  | Select (_, cases) ->
     List.fold cases ~init:one
       ~f:(fun acc (_,c) ->
         let res = num_table_paths c in
         if res = one then acc else acc + res
       )
  | Apply {actions; _} ->
     of_int (List.length actions) + one (* for the default action*)

let rec num_paths (c : cmd) : Bigint.t =
  let open Bigint in
  match c with
  | Skip | Assign _  | Assume _ -> one
  | Seq (c1,c2) -> num_paths c1 * num_paths c2
  | Select (_, cases) ->
     List.fold cases ~init:one ~f:(fun acc (_,c) -> acc + num_paths c)
  | Apply _ ->
     (* Assume apply will execute the default action *)
     one


let free_keys =
  List.filter_map ~f:(fun (k,sz,v_opt) ->
      match v_opt with
      | None -> Some (k,sz)
      | Some _ -> None
    )

let rec free_of_cmd typ (c:cmd) : (string * size) list =
  begin match c with
  | Skip -> []
  | Assign (f, e) ->
     begin match typ with
     | `Hole -> Expr.frees typ e
     | `Var -> (f,Expr.size e) :: Expr.frees typ e
     end
  | Seq (c, c') ->
     free_of_cmd typ c @ free_of_cmd typ c'
  | Assume t -> Test.frees typ t
  | Select (_,ss) ->
    List.fold ss ~init:[] ~f:(fun fvs (test, action) ->
        Test.frees typ test
        @ free_of_cmd typ action
        @ fvs
      )
  | Apply t ->
     free_keys t.keys
     @ List.fold t.actions
         ~init:(free_of_cmd typ t.default)
         ~f:(fun acc (_, data, a) ->
           acc @ (free_of_cmd typ a
                |> List.filter ~f:(fun (x,_) ->
                       List.for_all (List.map data ~f:fst) ~f:((<>) x)
                     )
                 )
         )
  end
  |> dedup


let free_vars_of_cmd = free_of_cmd `Var
let holes_of_cmd = free_of_cmd `Hole
      
let rec multi_ints_of_cmd c : (Value.t) list =
  match c with
  | Skip -> []
  (* Only collect _tested_ inputs*)
  | Assign (_,e) -> Expr.multi_vals e
  | Seq (c, c') ->
    multi_ints_of_cmd c
    @ multi_ints_of_cmd c'
  | Assume t ->
    Test.multi_vals t
  | Select (_,ss) ->
    concatMap ss ~init:(Some []) ~c:(@)
      ~f:(fun (test, action) ->
          Test.multi_vals test
          @ multi_ints_of_cmd action
      )
  | Apply t ->
     List.fold t.actions ~init:(multi_ints_of_cmd t.default)
       ~f:(fun rst act -> rst @ multi_ints_of_cmd (trd3 act))

let rec holify_cmd ~f holes c : cmd=
  match c with
  | Skip -> c
  | Assign (v, e) -> v %<-% Expr.holify ~f holes e
  | Assume t -> Assume (Test.holify ~f holes t)
  | Seq (c, c') ->
     holify_cmd ~f holes c %:% holify_cmd ~f holes c'
  | Select (styp, cases) ->
     List.map cases ~f:(fun (t, c) -> Test.holify ~f holes t, holify_cmd ~f holes c)
     |> mkSelect styp
  | Apply t
    -> Apply {name=t.name;
              keys=t.keys;
              actions = List.map t.actions ~f:(fun (n, data, act) ->
                            let holes' = List.filter holes ~f:(fun h ->
                                             List.for_all (List.map data ~f:fst) ~f:((<>) h)
                                           ) in
                            (n, data, holify_cmd ~f holes' act));
             default = holify_cmd ~f holes t.default}
              
    
(** replace all vars in cmd that are also in holes with holes having the same name*)
let holify ?(f=fun x -> x) holes c =  holify_cmd ~f holes c
        
let sequence = List.reduce_exn ~f:(%:%)

let rec assigned_vars = function
  | Skip | Assume _  -> StringSet.empty
  | Assign (f,_) -> StringSet.singleton f
  | Seq (c1,c2) -> StringSet.union (assigned_vars c1) (assigned_vars c2)
  | Select (_,cs) -> List.fold cs ~init:StringSet.empty
                       ~f:(fun acc (_,c) -> StringSet.union acc (assigned_vars c))
  | Apply t -> List.fold t.actions ~init:(assigned_vars t.default)
                 ~f:(fun acc (_, _,act) -> StringSet.union acc @@ assigned_vars act)


let rec get_schema_of_table name phys =
  match phys with
  | Skip 
    | Assume _
    | Assign _
    -> None
  | Seq (c1, c2) ->
     begin match get_schema_of_table name c1 with
     | None -> get_schema_of_table name c2
     | Some ks -> Some ks
     end
  | Select (_, cs) ->
     List.find_map cs ~f:(fun (_, c) -> get_schema_of_table name c)
  | Apply t
    -> if name = t.name then Some (t.keys, t.actions, t.default) else None

let rec get_tables_actions = function
  | Skip | Assume _ | Assign _ -> []
  | Seq(c1,c2) -> get_tables_actions c1 @ get_tables_actions c2
  | Select(_,cs) -> List.fold cs ~init:[] ~f:(fun acc (_,c) -> acc @ get_tables_actions c)
  | Apply t -> [t.name, t.actions]

let rec get_tables_actsizes = function
  | Skip | Assume _ | Assign _ -> []
  | Seq (c1,c2) -> get_tables_actsizes c1 @ get_tables_actsizes c2
  | Select(_,cs) ->
     List.fold cs ~init:[] ~f:(fun acc (_,c) -> acc @ get_tables_actsizes c)
  | Apply t -> [t.name, List.length t.actions]

let table_vars ?keys_only:(keys_only=false) (keys, acts, default) =
  let open List in
  free_keys keys
  @ if keys_only then
      []
    else
      (acts >>= fun (_, ad,c) ->
       free_vars_of_cmd c
       |> filter ~f:(fun (x,_) ->
              for_all ad ~f:(fun (y,_) -> x <> y)))
      @ free_vars_of_cmd default

let rec get_tables_vars ?keys_only:(keys_only=false) = function
  | Skip | Assume _ | Assign _ -> []
  | Seq (c1,c2) -> get_tables_vars ~keys_only c1 @ get_tables_vars ~keys_only c2
  | Select(_,cs) ->
     List.fold cs ~init:[] ~f:(fun acc (_,c) -> acc @ get_tables_vars ~keys_only c)
  | Apply t -> [t.name, table_vars ~keys_only (t.keys, t.actions, t.default)]


let rec get_tables_keys = function
  | Skip | Assume _ | Assign _ -> []
  | Seq (c1,c2) -> get_tables_keys c1 @ get_tables_keys c2
  | Select (_, cs) ->
     List.bind cs ~f:(fun (_,c) -> get_tables_keys c)
  | Apply t -> [t.name, t.keys]


let string_of_map m =
  StringMap.fold ~f:(fun ~key:k ~data:v acc -> ("(" ^ k ^ " -> " ^ (Value.to_string v) ^ ") " ^ acc)) m ~init:""
