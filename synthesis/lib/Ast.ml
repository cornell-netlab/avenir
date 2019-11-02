open Core
open Util

type value1 =
  | Int of int
  | VTuple of (value1 list)
   
type expr1 =
  | Value1 of value1
  | Var1 of string
  | Hole1 of string
  | Plus of (expr1 * expr1)
  | Times of (expr1 * expr1)
  | Minus of (expr1 * expr1)
  | Tuple of (expr1 list)

let mkTuple es =
  let rec all_val acc es = match es with
    | [] -> Some acc
    | ((Value1 v)::es) -> all_val (v::acc) es
    | _  -> None 
  in
  match all_val [] es with
  | None -> Tuple es
  | Some vs -> Value1 (VTuple vs)

type value2 =
  | Empty
  | VSingle of value1
  | VUnion of (value2 * value2)
           
type expr2 =
  | Value2 of value2
  | Hole2 of string
  | Var2 of string
  | Single of expr1
  | Union of (expr2 * expr2)

let mkSingle (e : expr1) =
  match e with
  | Value1 v -> Value2 (VSingle v)
  | _ -> Single e

let mkUnion (e : expr2) (e' : expr2) =
  match e, e' with
  | Value2 v, Value2 v' -> VUnion (v, v') |> Value2
  | _, _ -> Union (e, e')

let mkEmpty : expr2 = Value2 Empty          
           
let rec string_of_value1 (v : value1) : string =
  match v with
  | Int i -> string_of_int i
  | VTuple vs -> "(" ^ concatMap vs
                         ~c:(fun s s' -> s ^ "," ^ s')
                         ~f:(string_of_value1)
                 ^ ")"
let rec string_of_expr1 (e : expr1) : string =
  match e with
  | Value1 v -> string_of_value1 v
  | Var1 s -> s
  | Hole1 s -> "?" ^ s
  | Tuple es -> "(" ^ concatMap es
                        ~c:(fun c c' -> c ^ "," ^ c')
                        ~f:(string_of_expr1)
                ^ ")"
  | Plus (e, e') -> "(" ^ string_of_expr1 e ^ " + " ^ string_of_expr1 e' ^ ")"
  | Times (e, e') -> string_of_expr1 e ^ " * " ^ string_of_expr1 e'
  | Minus (e, e') -> "(" ^ string_of_expr1 e ^ " - " ^ string_of_expr1 e' ^ ")"

let rec sexp_string_of_value1 (v : value1) =
  match v with
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | VTuple vs -> "VTuple([" ^ concatMap vs
                                ~c:(fun s s' -> s ^ "," ^ s')
                                ~f:(sexp_string_of_value1)
                 ^ "])"
                   
let rec sexp_string_of_expr1 (e : expr1) =
  match e with
  | Value1 v -> "Value1(" ^ sexp_string_of_value1 v ^ ")"
  | Var1 s -> "Var(" ^ s ^ ")"
  | Hole1 s -> "Hole(" ^ s ^ ")"  
  | Tuple es -> "Tuple([" ^
                  concatMap es ~c:(fun c c' -> c ^ "," ^ c')
                    ~f:sexp_string_of_expr1
                  ^ "])"
  | Plus (e,e') ->
     "Plus(" ^ sexp_string_of_expr1 e ^ ", " ^ sexp_string_of_expr1 e' ^ ")"
  | Times (e,e') ->
     "Times(" ^ sexp_string_of_expr1 e ^ ", " ^ sexp_string_of_expr1 e' ^ ")"
  | Minus (e, e') ->
     "Minus(" ^ sexp_string_of_expr1 e ^ ", " ^ sexp_string_of_expr1 e' ^ ")"

let mkInt i = Int i     
let mkVInt i = Value1 (mkInt i)    
let mkPlus e e' = Plus(e,e')
let mkMinus e e' = Minus (e, e')
let mkTimes e e' = Times (e, e')


let rec string_of_value2 (v : value2) : string =
  match v with
  | Empty -> "{}"
  | VSingle v -> "{" ^ string_of_value1 v ^ "}"
  | VUnion (v,v') -> string_of_value2 v ^ " U " ^ string_of_value2 v'

let rec string_of_expr2 (e : expr2) : string =
  match e with
  | Value2 v2 -> string_of_value2 v2
  | Var2 s -> s
  | Hole2 s -> "?" ^ s
  | Single e1 -> "{" ^ string_of_expr1 e1 ^ "}"
  | Union (e2, e2') -> string_of_expr2 e2 ^ " U " ^ string_of_expr2 e2'


let rec sexp_string_of_value2 (v : value2) : string =
  match v with
  | Empty -> "Empty"
  | VSingle v -> "VSingle(" ^ sexp_string_of_value1 v ^ "}"
  | VUnion (v,v') -> "VUnion("
                     ^ sexp_string_of_value2 v
                     ^ "," ^ sexp_string_of_value2 v' ^ ")"
                                               
                     
let rec sexp_string_of_expr2 (e : expr2) : string =
  match e with
  | Value2 v2 -> "Value2(" ^ sexp_string_of_value2 v2 ^")"
  | Var2 s -> "Var2("^ s ^")"
  | Hole2 s -> "Hole2(" ^ s ^ ")"
  | Single e -> "Single(" ^ sexp_string_of_expr1 e ^ ")"
  | Union(e, e') -> "Union(" ^ sexp_string_of_expr2 e ^ "," ^ sexp_string_of_expr2 e' ^")"


let rec add_values1 (v : value1) (v' : value1) : value1 =
  match v, v' with
  | Int x, Int x' -> Int (x + x')
  | Int _, VTuple vs | VTuple vs, Int _ -> VTuple (List.map ~f:(add_values1 v) vs)
  | VTuple vs, VTuple vs' -> VTuple (vs @ vs')

let rec multiply_values1 (v : value1) (v' : value1) : value1 =
  match v, v' with
  | Int x, Int x' -> Int (x * x')
  | Int _, VTuple vs | VTuple vs, Int _ -> VTuple (List.map vs ~f:(multiply_values1 v))
  | VTuple vs, VTuple vs' -> List.cartesian_product vs vs'
                             |> List.map ~f:(fun (v,v') -> VTuple [v;v'])
                             |> VTuple

let rec subtract_values1 (v : value1) (v' : value1) : value1 =
  match v, v' with
  | Int x, Int x' -> Int (x - x')
  | Int _, VTuple vs' -> VTuple (List.map vs' ~f:(fun v' -> subtract_values1 v v'))
  | VTuple vs, Int _ -> VTuple (List.map vs ~f:(fun v -> subtract_values1 v v'))
  | VTuple vs, VTuple vs' -> List.filter vs ~f:(fun v -> not (List.exists vs' ~f:(fun v' -> v = v')))
                             |> VTuple

let equal_values1 (v : value1) (v' : value1) : bool =
  match v, v' with
  | Int x, Int x' -> x = x'
  | Int _ , VTuple _ | VTuple _, Int _ -> false
  | VTuple vs, VTuple vs' -> List.equal (=) vs vs'
  
let rec lt_values1 (v : value1) (v' : value1) : bool =
  match v, v' with
  | Int x, Int x' -> x < x'
  | Int _, VTuple _ | VTuple _, Int _ -> failwith "Incomparable, tuple and int"
  | VTuple vs, VTuple vs' ->  List.compare (fun v v' ->
                                  if equal_values1 v v' then
                                    0
                                  else if lt_values1 v v' then
                                    -1
                                  else
                                    1) vs vs' < 0
                  
type test =
  | True | False
  | LocEq of int
  | Eq of (expr1 * expr1)
  | Lt of (expr1 * expr1)
  | Member of (expr1 * expr2)
  | And of (test * test)
  | Or of (test * test)
  | Neg of test       

let mkEq (e : expr1) (e':expr1) =
  if e = e' then True else
    let ord e = match e with
      | Var1 _ -> 0
      | Hole1 _ -> 1
      | Plus _ -> 3
      | Minus _ -> 4
      | Times _ -> 5
      | Tuple _ -> 6
      | Value1 _ -> 7
    in
    match e, e' with 
    | Value1 _, Value1 _ -> False
    | Hole1 x , Hole1 x'
    | Var1 x, Var1 x' ->
       if x < x' then Eq (e, e') else Eq (e', e)
    | _, _ ->
       if ord e < ord e' then Eq (e, e') else Eq (e', e)

let mkLt (e : expr1) (e' : expr1) : test = match e, e' with
    | Value1(Int first), Value1(Int second) -> 
       if first < second then True else False
    | _, _ -> Lt(e, e')
            
let (%=%) = mkEq
let (%<>%) v v' = Neg(v %=% v') 

let (%<%) = mkLt

let rec mkOr (t : test) (t' : test) : test =
  match t, t' with
  | False, x  | x, False -> x
  | True, _ | _, True -> True
  | _, Or (t'', t''') -> (* left-associative *)
     mkOr (mkOr t t'') t'''
  | _ -> Or (t, t')

let (%+%) = mkOr

let rec mkAnd (t : test) (t' : test) =
  match t, t' with
  | True, x | x, True -> x
  | False, _ | _, False -> False
  | _, And ( t'', t''') -> (* left-associative *)
     mkAnd (mkAnd t t'') t'''
  | _ -> And (t, t')

let (%&%) = mkAnd
       
let mkNeg t =
  match t with
  | True -> False
  | False -> True
  | Neg t -> t
  | _ -> Neg t

let (!%) = mkNeg      
  
let mkImplies assum conseq = mkOr (mkNeg assum) conseq
let (%=>%) = mkImplies

let mkIff lhs rhs = (lhs %=>% rhs) %&% (rhs %=>% lhs)
let (%<=>%) = mkIff

let rec string_of_test t =
  match t with
  | True -> "true"
  | False -> "false"
  | LocEq i -> "loc = " ^ string_of_int i
  | Eq (left, right) -> string_of_expr1 left ^ " = " ^ string_of_expr1 right
  | Lt (left, right) -> string_of_expr1 left ^ " < " ^ string_of_expr1 right
  | Member (expr, set) -> string_of_expr1 expr ^ " in " ^ string_of_expr2 set
  | Or (Neg(assum), conseq) -> "(" ^ string_of_test assum ^ " ==> " ^ string_of_test conseq ^ ")"
  | Or (left, right) -> "(" ^ string_of_test left ^ " || " ^ string_of_test right ^ ")"
  | And (left, right) -> "(" ^ string_of_test left ^ "&&" ^ string_of_test right ^ ")"
  | Neg t -> "~(" ^ string_of_test t ^ ")"

let rec sexp_string_of_test t =
  let binop opname left right recfun : string=
    opname ^ "(" ^ recfun left ^ "," ^ recfun right ^ ")" in
  match t with
  | True -> "True"
  | False -> "False"
  | LocEq l -> "LocEq(" ^ string_of_int l ^ ")"
  | Eq  (left, right) -> binop "Eq" left right sexp_string_of_expr1
  | Lt  (left, right) -> binop "Lt" left right sexp_string_of_expr1
  | Member (expr, set) -> "Member(" ^ string_of_expr1 expr ^ "," ^ string_of_expr2 set ^ ")" 
  | Or  (left, right) -> binop "Or" left right sexp_string_of_test
  | And (left, right) -> binop "And" left right sexp_string_of_test
  | Neg t -> "Neg(" ^ sexp_string_of_test t ^ ")"
           
let rec remove_dups y xs =
  match xs with
  | [] -> []
  | x::xs ->
     if y = x then
       remove_dups y xs
     else
       x :: remove_dups y xs
    
let rec dedup xs =
  match xs with
  | [] -> []
  | x::xs ->
     let xs' = remove_dups x xs in
     x :: dedup xs'
  
let rec free_of_test typ test =
  begin match test with
  | True | False | LocEq _ ->
    []
  | Or (l,r) | And (l, r) ->
     free_of_test typ l @ free_of_test typ r
  | Neg t ->
     free_of_test typ t
  | Member _ -> Printf.printf "Warning [Ast.free_of_test] Dont know how to collect free variables from a membership query"; []
  | Eq (v, v') | Lt (v, v') ->
    match typ, v, v'  with
    | `Var, Var1 x, Var1 x' ->
      [x; x']
    | `Var, _, Var1 x
    | `Var, Var1 x, _ ->
      [x]
    | `Hole, Hole1 x, Hole1 x' ->
      [x; x']
    | `Hole, Hole1 x, _
    | `Hole, _, Hole1 x ->
      [x]
    | _ -> []
  end
  |> dedup

let free_vars_of_test = free_of_test `Var
let holes_of_test = free_of_test `Hole

let rec multi_ints_of_value1 e =
  match e with
  | Int i -> [i]
  | VTuple vs -> concatMap vs ~c:(@) ~f:multi_ints_of_value1

let rec multi_ints_of_expr1 e =
  match e with
  | Value1 v -> multi_ints_of_value1 v
  | Var1 _ | Hole1 _ -> []
  | Plus (e,e') | Times (e,e') | Minus (e,e')
    -> multi_ints_of_expr1 e @ multi_ints_of_expr1 e'
  | Tuple es ->
     concatMap es ~init:(Some []) ~c:(@) ~f:multi_ints_of_expr1

let rec multi_ints_of_value2 v2 =
  match v2 with
  | Empty -> []
  | VSingle v -> multi_ints_of_value1 v
  | VUnion (v,v') -> multi_ints_of_value2 v @ multi_ints_of_value2 v'
    
let rec multi_ints_of_expr2 e2 =
  match e2 with
  | Var2 _ | Hole2 _ -> []
  | Value2 v -> multi_ints_of_value2 v 
  | Single e -> multi_ints_of_expr1 e
  | Union (v,v') -> multi_ints_of_expr2 v @ multi_ints_of_expr2 v'
                  
let rec multi_ints_of_test test =
  begin match test with
    | True | False | LocEq _ ->
      []
    | Or (l, r) | And (l, r) ->
      multi_ints_of_test l
      @ multi_ints_of_test r
    | Neg t ->
       multi_ints_of_test t
    | Member (expr, set) ->
       multi_ints_of_expr1 expr @ multi_ints_of_expr2 set
    | Eq (e, e') | Lt (e, e') ->
       multi_ints_of_expr1 e @ multi_ints_of_expr1 e'
  end

let rec remove_locs_neq l (t:test) : test =
  match t with
  | LocEq l'
    -> if l = l' then True else False
  | True
  | False
  | Eq _
  | Lt _
  | Member _
    -> t
  | Neg t
    -> !%(remove_locs_neq l t)
  | And (a, b)
    -> remove_locs_neq l a %&% remove_locs_neq l b
  | Or (a, b)
    -> remove_locs_neq l a %+% remove_locs_neq l b


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
  | SetLoc of int
  | Assign of (string * expr1)
  | Assert of test
  | Assume of test
  | Seq of (cmd * cmd)
  | While of (test * cmd)
  | Select of (select_typ * ((test * cmd) list))
  | Apply of (string * string list * cmd list * cmd)


let clean_selects_list =
  concatMap ~init:(Some []) ~c:(@)
    ~f:(fun (cond, act) ->
        if cond = False then
          []
        else
          [cond, act])

let mkPartial ss =
  let selects = clean_selects_list ss in
  if List.length selects = 0 then
    Skip
  else
    Select (Partial, selects)

let mkTotal ss =
  let selects = clean_selects_list ss in
  if List.length selects = 0 then
    Assert False
  else
    Select (Total, selects)

let mkOrdered ss =
  let selects = clean_selects_list ss
                |> List.remove_consecutive_duplicates
                  ~which_to_keep:`First
                  ~equal:(fun (cond,_) (cond',_) -> cond = cond')
  in
  if List.length selects = 0 then
    Skip
  else
    Select (Ordered, selects)

let mkSeq first scnd =
  match first, scnd with
  | Skip, x | x, Skip -> x
  | _,_ -> Seq(first, scnd)

let mkSelect styp =
  match styp with
  | Partial -> mkPartial
  | Total -> mkTotal
  | Ordered -> mkOrdered

         
let (%:%) = mkSeq

let mkAssn f v = Assign (f, v)
let (%<-%)= mkAssn
         

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

let mkWhile t e = While(t,e)
       
let rec repeat c n =  if n = 0 then "" else c ^ repeat c (n-1)
                    
let rec string_of_cmd ?depth:(depth=0) (e : cmd) : string =
  match e with
  | Skip -> "skip"
  | While (cond, body) ->
    "\n" ^ repeat "\t" depth ^
    "while(" ^ string_of_test cond ^ ") {\n"
      ^ repeat "\t" (depth+1)
      ^ string_of_cmd ~depth:(depth+1) body
      ^ "\n" ^ repeat "\t" depth
      ^ "}\n" ^ repeat "\t" depth
  | Seq (firstdo, thendo) ->
    string_of_cmd ~depth firstdo ^ "; "
    ^ string_of_cmd ~depth thendo
  | Assert t ->
    (* repeat "\t" depth ^ *)
    "assert (" ^ string_of_test t ^ ")"
  | Assume t ->
    (* repeat "\t" depth ^ *)
    "assume (" ^ string_of_test t ^ ")"
  | SetLoc i -> "loc := " ^ string_of_int i
  | Assign (field, expr) ->
    field ^ " := " ^ string_of_expr1 expr
  | Select (styp, es) ->
    let modifier = (string_of_select_typ styp) in
    "if " ^ modifier ^
    List.fold_left es ~init:"" ~f:(fun str (cond, act)->
        str ^ "\n" ^
        repeat "\t" (depth + 1)
        ^ string_of_test cond  ^ " -> " ^ string_of_cmd ~depth:(depth+2) act ^ " []"
      )
    ^ "\n" ^ repeat "\t" depth ^ "fi"
  | Apply (name, keys, acts, default) ->
      name ^ "apply ("
      ^ List.fold_left keys ~init:""
          ~f:(fun str k ->
            str ^ ";" ^ k)
      ^ "," ^ List.fold_left acts ~init:""
                ~f:(fun str a ->
                  str ^ ";" ^ string_of_cmd a)
      ^ "," ^ string_of_cmd default
                                       
            
  
let rec sexp_string_of_cmd e : string =
  let string_select = concatMap 
                        ~f:(fun (cond,act) -> "(" ^ sexp_string_of_test cond ^ "," ^ sexp_string_of_cmd act ^ ")")
                        ~c:(fun acc d -> acc ^ ";" ^ d) in
  match e with
  | Skip -> "Skip"
  | While (cond, body) -> "While("^ sexp_string_of_test cond ^ "," ^ sexp_string_of_cmd body ^")"
  | Seq (p, q) -> "Seq(" ^ sexp_string_of_cmd p ^ "," ^ sexp_string_of_cmd q ^ ")"
  | Assert t -> "Assert(" ^ sexp_string_of_test t ^ ")"
  | Assume t -> "Assume(" ^ sexp_string_of_test t ^ ")"
  | SetLoc l ->  "SetLoc(" ^ string_of_int l ^ ")"
  | Assign (f,e) -> "Assign(" ^ f ^ "," ^ string_of_expr1 e ^")"
  | Select (styp,es) ->
    let cases_string = match es with
      | [] -> "[]"
      | _  -> "[" ^ string_select es ^ "]"
    in
    "Select (" ^ sexp_string_of_select_typ styp ^ "," ^ cases_string ^ ")"
  | Apply (name, keys, actions, default) ->
     "Apply("
     ^ name ^ ",["
     ^ List.fold_left keys ~init:"" ~f:(fun str k -> str ^ ";\"" ^ k ^ "\"")
     ^ "],["
     ^ List.fold_left actions ~init:"" ~f:(fun str a -> str ^ ";" ^ sexp_string_of_cmd a)
     ^ "]," ^ sexp_string_of_cmd default
     ^ ")"

let rec tables_of_cmd (c:cmd) : string list =
  match c with
  | Skip | SetLoc _ | Assign _  | Assume _ | Assert _  -> []
  | Seq (c,c') -> tables_of_cmd c @ tables_of_cmd c'
  | Select (_, cs) -> concatMap cs ~c:(@) ~init:(Some []) ~f:(fun (_, c) -> tables_of_cmd c)
  | While (_,c) -> tables_of_cmd c
  | Apply (s,_,_,_) -> [s]
                                                              

    
let rec free_of_cmd typ (c:cmd) : string list =
  begin match c with
  | Skip | SetLoc _ -> []
  | Assign (f, e) ->
     f :: (match e,typ with
        | Hole1 x,`Hole -> [x]
        | Var1 x, `Var -> [x]
        | _, _ -> [] )
  | Seq (c, c') ->
     free_of_cmd typ c @ free_of_cmd typ c'
  | While (cond, body) ->
     free_of_test typ cond
     @ free_of_cmd typ body
  | Assert t | Assume t -> free_of_test typ t
  | Select (_,ss) ->
    List.fold ss ~init:[] ~f:(fun fvs (test, action) ->
        free_of_test typ test
        @ free_of_cmd typ action
        @ fvs
      )
  | Apply (_,_,actions, default) ->
     List.fold actions
       ~init:(free_of_cmd typ default)
       ~f:(fun acc a -> acc @ (free_of_cmd typ a))
  end
  |> dedup


let free_vars_of_cmd = free_of_cmd `Var
let holes_of_cmd = free_of_cmd `Hole
      
let rec multi_ints_of_cmd c =
  match c with
  | Skip
  | SetLoc _ ->
    []
  (* | Assign (_, Int i) ->
   *   [i] *)
  (* Only collect _tested_ inputs*)
  | Assign _ ->
    []
  | Seq (c, c') ->
    multi_ints_of_cmd c
    @ multi_ints_of_cmd c'
  | While (cond, body) ->
     multi_ints_of_test cond
     @ multi_ints_of_cmd body
  | Assert t
  | Assume t ->
    multi_ints_of_test t
  | Select (_,ss) ->
    concatMap ss ~init:(Some []) ~c:(@)
      ~f:(fun (test, action) ->
          multi_ints_of_test test
          @ multi_ints_of_cmd action
      )
  | Apply (_,_,actions, default) ->
     List.fold actions ~init:(multi_ints_of_cmd default)
       ~f:(fun rst act -> rst @ multi_ints_of_cmd act)

let no_nesting ss =
  let rec no_while_or_select_in_cmd c =
    match c with
    | Skip
    | SetLoc _
    | Assign _
    | Assert _
    | Assume _
      -> true
    | Seq (c, c')
      -> no_while_or_select_in_cmd c
         && no_while_or_select_in_cmd c'
    | Apply (_,_,actions, default)
      -> List.fold actions ~init:(no_while_or_select_in_cmd default)
           ~f:(fun acc act -> acc && no_while_or_select_in_cmd act)
    | While _
    | Select _
      -> false
  in
  concatMap ss ~init:(Some true) ~c:(&&)
    ~f:(fun (_,act) -> no_while_or_select_in_cmd act)

let instrumented =
  let rec instrumented_test found_loc t =
    if found_loc then true else
      match t with
      | LocEq _
        -> true
      | True
      | False
      | Eq _
      | Lt _
      | Member _
        -> false
      | And (a, b) ->
        instrumented_test false a
        || instrumented_test false b
      | Or (a, b) ->
        instrumented_test false a
        && instrumented_test false b
      | Neg _ ->
        failwith "Borked, [instrumented] can only be called on a list of selects with no negative tests"
  in
  let rec instrumented_cmd found_loc c =
    if found_loc then found_loc else
      match c with
      | Skip
      | Assign _
      | Assert _
      | Assume _
        -> false
      | SetLoc _
        -> true
      | Apply (_,_,acts,dflt)
        -> List.fold acts  ~init:(instrumented_cmd found_loc dflt)
             ~f:(fun found act -> found || instrumented_cmd found act)
      | Seq (p, q)
        -> instrumented_cmd false p
           || instrumented_cmd false q
      | While _
      | Select _
        -> failwith "Borked, instrumented can only be called on a list of selects with no nesting"
  in
  concatMap ~c:(&&)
    ~f:(fun (cond, act) ->
        instrumented_test false cond
        && instrumented_cmd false act)
       
let no_negated_holes ss =
  let rec no_negated_holes_test t =
    match t with
    | True
    | False
    | LocEq _
    | Eq _
    | Lt _
    | Member _
      -> true
    | And (a, b)
      -> no_negated_holes_test a && no_negated_holes_test b
    | Or (a,b)
      -> no_negated_holes_test a && no_negated_holes_test b
    | Neg a ->
      let rec has_hole t =
        match t with
        | True
        | False
        | LocEq _
          -> false
        | Eq (e, e')
        | Lt (e, e')
          -> begin match (e, e') with
              | Hole1 _, _ | _, Hole1 _ ->
                true
              | _, _ ->
                false
             end
        | Member (expr, set)
          -> begin match (expr, set) with
             | Hole1 _, _ | _, Hole2 _ ->  true
             | _ ,_ -> false
             end
        | Neg t' (* double-negation *)
          -> not (has_hole t')
        | And (a, b)
        | Or (a, b)
          -> has_hole a || has_hole b
      in
      not (has_hole a)
  and no_negated_holes_cmd c =
    let gpair (t, c) =
      no_negated_holes_test t && no_negated_holes_cmd c
    in
    match c with
    | Skip
    | Assign _
    | SetLoc _
      -> true
    | Assume t
    | Assert t
      -> no_negated_holes_test t
    | Seq (c, c')
      -> no_negated_holes_cmd c && no_negated_holes_cmd c'
    | While (cond, body)
      -> gpair (cond, body)
    | Select (_,ss)
      -> concatMap ss ~init:(Some true) ~c:(&&) ~f:gpair
    | Apply (_,_,acts,dflt)
      -> List.fold acts ~init:(no_negated_holes_cmd dflt)
           ~f:(fun acc act -> acc && no_negated_holes_cmd act )
  in
  concatMap ss ~init:(Some true) ~c:(&&)
    ~f:(fun (cond, act) ->
        no_negated_holes_test cond
          && no_negated_holes_cmd act)

(** replace all vars in cmd that are also in holes with holes having the same name*)
let holify holes c =
  let rec holify_expr1 (e : expr1) : expr1 =
    match e with
    | Hole1 _ | Value1 _ -> e
    | Var1 x ->
      begin match List.find holes ~f:(fun elem -> x = elem)  with
      | None -> e
      | Some _ -> Hole1 ("?" ^ x)
      end
    | Plus (e,e') -> Plus (holify_expr1 e, holify_expr1 e')
    | Times (e,e') -> Times (holify_expr1 e, holify_expr1 e')
    | Minus (e,e') -> Minus (holify_expr1 e, holify_expr1 e')
    | Tuple es -> List.map es ~f:holify_expr1 |>  Tuple 
  in
  let rec holify_expr2 (e : expr2) : expr2 =
    match e with
    | Value2 _ -> e
    | Hole2 _ -> e
    | Var2 x ->
       begin match List.find holes ~f:(fun elem -> x = elem) with
       | None -> e
       | Some _ -> Hole2 ("?" ^ x)
       end
    | Single e -> Single (holify_expr1 e)
    | Union (e,e') -> Union (holify_expr2 e, holify_expr2 e')
  in
  let rec holify_test b : test =
    match b with
    | True | False | LocEq _ -> b
    | Eq (e, e') -> holify_expr1 e %=% holify_expr1 e'
    | Lt (e, e') -> holify_expr1 e %<% holify_expr1 e'
    | Member (expr, set) -> Member(holify_expr1 expr, holify_expr2 set)
    | And (b, b') -> holify_test b %&% holify_test b'
    | Or (b, b')  -> holify_test b %+% holify_test b'
    | Neg b       -> !%(holify_test b)
  in
  let rec holify_cmd c : cmd=
    match c with
    | Skip | SetLoc _ -> c
    | Assign (f, e) -> f %<-% holify_expr1 e
    | Assert t -> Assert (holify_test t)
    | Assume t -> Assume (holify_test t)
    | Seq (c, c') ->
      holify_cmd c %:% holify_cmd c'
    | While (t, c) ->
      mkWhile (holify_test t) (holify_cmd c)
    | Select (styp, cases) ->
      List.map cases ~f:(fun (t, c) -> holify_test t, holify_cmd c)
      |> mkSelect styp
    | Apply (name,keys,acts,dflt)
      -> Apply(name, keys, List.map acts ~f:holify_cmd, holify_cmd dflt)
  in
  holify_cmd c
        
