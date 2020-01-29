open Core
open Util

let enable_smart_constructors = true

type size = int
              
type match_expr =
  | Exact of int * size
  | Between of int * int * size 

let string_of_match m =
  match m with 
  | Exact (i,s) -> Printf.sprintf "%d#%d" i s
  | Between (lo,hi,s) -> Printf.sprintf "[%d,%d]#%d" lo hi s
                                       

type action_data = (int * size) list
type row = match_expr list * action_data * int
let string_of_row (mtchs, ad, actid) =
  Printf.sprintf "%s   ---(%s)---> %d"
    (List.fold mtchs ~init:"" ~f:(fun acc m -> Printf.sprintf "%s, %s" acc (string_of_match m)))
    (List.fold ad ~init:"" ~f:(fun acc (d,_) -> Printf.sprintf "%s, %d" acc d))
    actid

type instance = row list StringMap.t

type edit = string * row
let string_of_edit (nm,(row) : edit) =
  Printf.sprintf "%s <++ %s" nm (string_of_row row)

                 
type value1 =
  | Int of (int * size)
  | VTuple of (value1 list)

type expr1 =
  | Value1 of value1
  | Var1 of (string * size)
  | Hole1 of (string * size)
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
  | Some vs -> Value1 (VTuple vs)
  | None ->
     match es with
     | [e] -> e
     | _ -> Tuple es

let (%@%) e e' =
  match e, e' with
  | Tuple es, Tuple es' -> Tuple (es @ es')
  | _, _ -> failwith "Must Concat tuples"

                     
type value2 =
  | Empty
  | VSingle of value1
  | VUnion of (value2 * value2)
           
type expr2 =
  | Value2 of value2
  | Hole2 of (string * size)
  | Var2 of (string * size)
  | Single of expr1
  | Union of (expr2 * expr2)



               
let mkSingle (e : expr1) =
  match e with
  | Value1 v -> Value2 (VSingle v)
  | _ -> Single e

let mkUnion (e : expr2) (e' : expr2) : expr2 =
  match e, e' with
  | Value2 Empty, o | o, Value2 Empty -> o
  | Value2 v, Value2 v' -> VUnion (v, v') |> Value2
  | _, _ -> Union (e, e')

let mkEmpty : expr2 = Value2 Empty
let mkInsert (el : expr1) (set: expr2) : expr2 =
  mkSingle el |> mkUnion set
           
let rec string_of_value1 (v : value1) : string =
  match v with
  | Int (i,x) -> string_of_int i ^ "#" ^ string_of_int x
  | VTuple vs -> "(" ^ concatMap vs
                         ~c:(fun s s' -> s ^ "," ^ s')
                         ~f:(string_of_value1)
                 ^ ")"
let rec string_of_expr1 (e : expr1) : string =
  match e with
  | Value1 v -> string_of_value1 v
  | Var1 (x,s) -> x ^ "#" ^ string_of_int s
  | Hole1 (x,s) -> "?" ^ x ^ "#" ^ string_of_int s
  | Tuple es -> "(" ^ concatMap es ~init:(Some "")
                        ~c:(fun c c' -> c ^ "," ^ c')
                        ~f:(string_of_expr1)
                ^ ")"
  | Plus (e, e') -> "(" ^ string_of_expr1 e ^ " + " ^ string_of_expr1 e' ^ ")"
  | Times (e, e') -> string_of_expr1 e ^ " * " ^ string_of_expr1 e'
  | Minus (e, e') -> "(" ^ string_of_expr1 e ^ " - " ^ string_of_expr1 e' ^ ")"

let rec sexp_string_of_value1 (v : value1) =
  match v with
  | Int (i,sz) -> "Int(" ^ string_of_int i ^ "," ^ string_of_int sz ^ ")"
  | VTuple vs -> "VTuple([" ^ concatMap vs
                                ~c:(fun s s' -> s ^ "," ^ s')
                                ~f:(sexp_string_of_value1)
                 ^ "])"
                   
let rec sexp_string_of_expr1 (e : expr1) =
  match e with
  | Value1 v -> "Value1(" ^ sexp_string_of_value1 v ^ ")"
  | Var1 (x, s) -> "Var(\"" ^ x ^ "\"," ^ string_of_int s ^ ")"
  | Hole1 (x, s) -> "Hole(\"" ^ x ^ "\"," ^ string_of_int s ^ ")"  
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


let get_int (v : value1) : int =
  match v with
  | Int (x, _) -> x
  | _ -> failwith ("value not an int: " ^ string_of_value1 v)
                  
let rec size_of_value1 (v : value1) : size =
  match v with
  | Int (_, s) -> s
  | VTuple vs -> List.fold vs ~init:0 ~f:(fun sum v -> size_of_value1 v + sum)
                           
let rec size_of_expr1 (e : expr1) : size =
  match e with
  | Value1 v -> size_of_value1 v
  | Var1 (_,s) -> s
  | Hole1 (_,s) -> s
  | Plus (e, e') | Minus(e,e') | Times (e,e') ->
     let s = size_of_expr1 e in
     let s' = size_of_expr1 e' in
     if s = s' then s
     else failwith (Printf.sprintf "size of expressions: %s, and %s differs (%d and %d)"
                      (string_of_expr1 e)
                      (string_of_expr1 e')
                      s s')
  | Tuple es -> List.fold es ~init:0 ~f:(fun sum e -> size_of_expr1 e + sum)

let rec num_nodes_in_expr1 e =
  match e with
  | Value1 _ | Var1 _ | Hole1 _ -> 1
  | Plus (e,e') | Minus(e,e') | Times(e,e') ->
     num_nodes_in_expr1 e
     + num_nodes_in_expr1 e'
     + 1
  | Tuple es -> List.fold es ~init:0 ~f:(fun sum e -> size_of_expr1 e + sum)

                                                                            
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
  | Var2 (x,sz) -> x ^ "#" ^ string_of_int sz
  | Hole2 (x,sz) -> "?" ^ x ^ "#" ^ string_of_int sz
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
  | Var2 (x,sz) -> "Var2(\""^ x ^ "\"," ^ string_of_int sz  ^")"
  | Hole2 (x,sz) -> "Hole2(\"" ^ x ^ "\","^ string_of_int sz ^ ")"
  | Single e -> "Single(" ^ sexp_string_of_expr1 e ^ ")"
  | Union(e, e') -> "Union(" ^ sexp_string_of_expr2 e ^ "," ^ sexp_string_of_expr2 e' ^")"

let rec size_of_value2 (v : value2) : size =
  match v with
  | Empty -> -1
  | VSingle x -> size_of_value1 x
  | VUnion (v,v') ->
     let s = size_of_value2 v in
     let s' = size_of_value2 v' in
     if s = s' || s < 0 || s' < 0 then if s > s' then s else s'
     else failwith (Printf.sprintf "Size of values %s and %s are different (%d and %d)"
                      (string_of_value2 v) (string_of_value2 v')
                      s s')

let rec size_of_expr2 (e : expr2) : size =
  match e with
  | Value2 v -> size_of_value2 v
  | Var2 (_, s) -> s
  | Hole2 (_, s) -> s
  | Single e -> size_of_expr1 e
  | Union (e, e') ->
     let s = size_of_expr2 e in
     let s' = size_of_expr2 e' in
     if s = s' || s < 0 || s' < 0 then if s > s' then s else s'                      
     else failwith (Printf.sprintf "Size of values %s and %s are different (%d and %d)"
                                   (string_of_expr2 e) (string_of_expr2 e) s s')
                   
let rec add_values1 (v : value1) (v' : value1) : value1 =
  match v, v' with
  | Int (x, sz), Int (x',sz') -> if sz = sz'
                                 then Int (x + x',sz)
                                 else
                                   failwith (Printf.sprintf "Type error %d#%d and %d#%d have different bitvec sizes"
                                               x sz x' sz')
  | Int _, VTuple vs | VTuple vs, Int _ -> VTuple (List.map ~f:(add_values1 v) vs)
  | VTuple vs, VTuple vs' -> VTuple (vs @ vs')

let rec multiply_values1 (v : value1) (v' : value1) : value1 =
  match v, v' with
  | Int (x, sz), Int (x',sz') -> if sz = sz' then Int (x * x', sz) else
                                   failwith (Printf.sprintf "Type error %d#%d and %d#%d have different file sizes"
                                               x sz x' sz')

  | Int _, VTuple vs | VTuple vs, Int _ -> VTuple (List.map vs ~f:(multiply_values1 v))
  | VTuple vs, VTuple vs' -> List.cartesian_product vs vs'
                             |> List.map ~f:(fun (v,v') -> VTuple [v;v'])
                             |> VTuple

let rec subtract_values1 (v : value1) (v' : value1) : value1 =
  match v, v' with
  | Int (x, sz), Int (x',sz') -> if sz = sz' then Int (x - x', sz) else
                                   failwith (Printf.sprintf "Type error %d#%d and %d#%d have different file sizes"
                                               x sz x' sz')

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
  | Eq of (expr1 * expr1)
  | Lt of (expr1 * expr1)
  | Member of (expr1 * expr2)
  | And of (test * test)
  | Or of (test * test)
  | Impl of (test * test)
  | Iff of (test * test)
  | Neg of test       



let rec mkOr (t : test) (t' : test) : test =
  if enable_smart_constructors then begin
      match t, t' with
      | False, x  | x, False -> x
      | True, _ | _, True -> True
      | _, Or (t'', t''') -> (* left-associative *)
         mkOr (mkOr t t'') t'''
      | _ -> Or (t, t')
    end
  else Or (t,t')

let (%+%) = mkOr

let rec mkAnd (t : test) (t' : test) =
  if enable_smart_constructors then begin
      if t = t' then t else
        match t, t' with
        | True, x | x, True -> x
        | False, _ | _, False -> False
        | _, And ( t'', t''') -> (* left-associative *)
           mkAnd (mkAnd t t'') t'''
        | _ -> And (t, t')
    end
  else And(t,t')

let (%&%) = mkAnd
       
let mkNeg t =
  if enable_smart_constructors then begin
      match t with
      | True -> False
      | False -> True
      | Neg t -> t
      | _ -> Neg t
    end
  else Neg t

let (!%) = mkNeg

             
let rec mkEq (e : expr1) (e':expr1) =
  if not enable_smart_constructors then Eq(e,e') else 
  if e = e' then
    ((*Printf.printf "[=] %s and %s are equal, so True\n" (string_of_expr1 e) (string_of_expr1 e');*)
    True) else
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
    | Value1 _, Value1 _
      ->
       (*Printf.printf "[=] %s and %s are unequal values so False\n" (string_of_expr1 e) (string_of_expr1 e');*)
       False
    | Hole1 x , Hole1 x'
    | Var1 x, Var1 x'
      -> if x < x'
         then Eq (e, e')
         else Eq (e', e)           
    | Tuple es, Tuple es'
      -> begin match es, es' with
         | [], [] -> True
         | _, [] | [], _ ->            
            (*Printf.printf "[=] %s and %s are unequal length so False \n" (string_of_expr1 e) (string_of_expr1 e');*)
            False
         | (h::es), (h'::es')
           ->
            (*Printf.printf "[=] %s and %s are equal length so far\n" (string_of_expr1 e) (string_of_expr1 e');*)
            mkEq (Tuple es) (Tuple es')
              |> mkAnd (mkEq h h') 
         end
    | _, _ ->
       if ord e < ord e'
       then Eq (e, e')
       else Eq (e', e)


let mkLt (e : expr1) (e' : expr1) : test =
  if not enable_smart_constructors then Lt(e,e') else
  match e, e' with
    | Value1(Int first), Value1(Int second) -> 
       if first < second then True else False
    | _, _ -> Lt(e, e')
            
let (%=%) = mkEq
let (%<>%) v v' = Neg(v %=% v') 

let (%<%) = mkLt
let (%>%) e e' = e' %<% e                
let (%<=%) e e' = !%(e %>% e')
let (%>=%) e e' = !%(e %<% e')

let rec mkMember el set =
  match set with
  | Value2 Empty ->  False
  | Value2 (VSingle v) ->  el %=% Value1 v
  | Value2 (VUnion (v,v')) -> mkMember el (Value2 v) %+% mkMember el (Value2 v')
  | Single e -> el %=% e
  | Union (set, set') -> mkMember el set %+% mkMember el set'
  | _ -> Member(el, set)
               
let mkImplies assum conseq =
  if not enable_smart_constructors then Impl(assum, conseq) else
    if assum = conseq then True else
    match assum, conseq with
    | True, _ -> conseq
    | _, True -> True
    | _, False -> !%(assum)
    | _, _ -> Impl(assum, conseq) 

let (%=>%) = mkImplies

let mkIff lhs rhs =
  if enable_smart_constructors
  then if lhs = rhs then True else Iff(lhs, rhs)
  else Iff(lhs, rhs)
let (%<=>%) = mkIff

let rec string_of_test t =
  match t with
  | True -> "true"
  | False -> "false"
  | Eq (left, right) -> string_of_expr1 left ^ " = " ^ string_of_expr1 right
  | Lt (left, right) -> string_of_expr1 left ^ " < " ^ string_of_expr1 right
  | Member (expr, set) -> string_of_expr1 expr ^ " in " ^ string_of_expr2 set
  | Impl (assum, conseq) -> "(" ^ string_of_test assum ^ " ==> " ^ string_of_test conseq ^ ")\n"
  | Iff (left, right) -> "(" ^ string_of_test left ^ " <==> " ^ string_of_test right ^ ")\n"
  | Or (left, right) -> "(" ^ string_of_test left ^ "\n || " ^ string_of_test right ^ ")"
  | And (left, right) -> "(" ^ string_of_test left ^ "&&" ^ string_of_test right ^ ")"
  | Neg (Lt(left, right)) ->
     Printf.sprintf "(%s <= %s)" (string_of_expr1 right) (string_of_expr1 left)
  | Neg t ->
     "~(" ^ string_of_test t ^ ")"

let rec sexp_string_of_test t =
  let binop opname left right recfun : string=
    opname ^ "(" ^ recfun left ^ "," ^ recfun right ^ ")" in
  match t with
  | True -> "True"
  | False -> "False"
  | Eq  (left, right) -> binop "Eq" left right sexp_string_of_expr1
  | Lt  (left, right) -> binop "Lt" left right sexp_string_of_expr1
  | Member (expr, set) -> "Member(" ^ string_of_expr1 expr ^ "," ^ string_of_expr2 set ^ ")"
  | Impl (assum, conseq) -> binop "Impl" assum conseq sexp_string_of_test
  | Iff (left, right) -> binop "Iff" left right sexp_string_of_test
  | Or  (left, right) -> binop "Or" left right sexp_string_of_test
  | And (left, right) -> binop "And" left right sexp_string_of_test
  | Neg t -> "Neg(" ^ sexp_string_of_test t ^ ")"

let rec num_nodes_in_test t =
  match t with
  | True | False -> 1
  | Eq (left, right) -> num_nodes_in_expr1 left + num_nodes_in_expr1 right + 1
  | Lt (left, right) -> num_nodes_in_expr1 left + num_nodes_in_expr1 right + 1
  | Member _ -> failwith "Membership not supported"
  | Iff (left, right) | Or (left, right) | And(left, right) | Impl(left, right)
    -> num_nodes_in_test left + 1 + num_nodes_in_test right
  | Neg t -> num_nodes_in_test t + 1
                          
           
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

let rec free_of_expr1 typ e : (string * size) list =
  match e, typ with
  | Value1 _, _ -> []
  | Var1 (x,sz), `Var  | Hole1 (x,sz), `Hole -> [x,sz]
  | Var1 _ , `Hole -> []
  | Hole1 _ , `Var -> []
  | Tuple es, _ -> concatMap es ~c:(@) ~f:(free_of_expr1 typ)
  | Plus(e,e'),_| Times (e,e'),_ | Minus(e,e'),_ ->
     free_of_expr1 typ e @ free_of_expr1 typ e'

let rec free_of_expr2 typ set =
  match set,typ with
  | Value2 _,_ -> []
  | Var2 (x,sz), `Var | Hole2 (x,sz), `Hole -> [x,sz]
  | Var2 _, `Hole | Hole2 _, `Var -> []
  | Single e, _ -> free_of_expr1 typ e
  | Union (e,e'),_ -> free_of_expr2 typ e @ free_of_expr2 typ e'
  
                
let rec free_of_test typ test : (string * size) list =
  begin match test with
  | True | False -> 
    []
  | Or (l,r) | And (l, r) | Impl (l,r) | Iff (l, r) ->
     free_of_test typ l @ free_of_test typ r
  | Neg t ->
     free_of_test typ t
  | Member (el, set) -> free_of_expr1 typ el @ free_of_expr2 typ set
  | Eq (e, e') | Lt (e, e') ->
     free_of_expr1 typ e @ free_of_expr1 typ e'
  end
  |> dedup

let free_vars_of_test t : (string * size) list=
  let vs = free_of_test `Var t in
  vs
            
                
let holes_of_test = free_of_test `Hole

let rec multi_ints_of_value1 e : (int * size) list =
  match e with
  | Int (i,sz) -> [i,sz]
  | VTuple vs -> concatMap vs ~c:(@) ~f:multi_ints_of_value1

let rec multi_ints_of_expr1 e : (int * size) list =
  match e with
  | Value1 v -> multi_ints_of_value1 v
  | Var1 _ | Hole1 _ -> []
  | Plus (e,e') | Times (e,e') | Minus (e,e')
    -> multi_ints_of_expr1 e @ multi_ints_of_expr1 e'
  | Tuple es ->
     concatMap es ~init:(Some []) ~c:(@) ~f:multi_ints_of_expr1

let rec multi_ints_of_value2 v2 : (int * size) list =
  match v2 with
  | Empty -> []
  | VSingle v -> multi_ints_of_value1 v
  | VUnion (v,v') -> multi_ints_of_value2 v @ multi_ints_of_value2 v'
    
let rec multi_ints_of_expr2 e2 : (int * size) list =
  match e2 with
  | Var2 _ | Hole2 _ -> []
  | Value2 v -> multi_ints_of_value2 v 
  | Single e -> multi_ints_of_expr1 e
  | Union (v,v') -> multi_ints_of_expr2 v @ multi_ints_of_expr2 v'
                  
let rec multi_ints_of_test test : (int * size) list =
  begin match test with
    | True | False -> 
      []
    | Or (l, r) | And (l, r)
      | Impl (l, r) | Iff (l,r)
      ->
      multi_ints_of_test l
      @ multi_ints_of_test r
    | Neg t ->
       multi_ints_of_test t
    | Member (expr, set) ->
       multi_ints_of_expr1 expr @ multi_ints_of_expr2 set
    | Eq (e, e') | Lt (e, e') ->
       multi_ints_of_expr1 e @ multi_ints_of_expr1 e'
  end

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
  | Assign of (string * expr1)
  | Assert of test
  | Assume of test
  | Seq of (cmd * cmd)
  | While of (test * cmd)
  | Select of (select_typ * ((test * cmd) list))
  | Apply of (string * (string * size) list * (((string * size) list * cmd) list) * cmd)

let clean_selects_list ss = 
  List.rev ss
  |> List.fold ~init:([], [])
    ~f:(fun (acc,seen) (c,a) ->
      if c = False || List.exists seen ~f:((=) c) then
        (acc,seen)
      else
        ((c,a) :: acc, c :: seen)
    )
  |> fst

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
    Assert False
  else
    Select (Total, selects)

let mkOrdered ss =
  if not enable_smart_constructors then Select(Ordered, ss) else
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
  if not enable_smart_constructors then Seq(first, scnd) else
  match first, scnd with
  | Skip, x | x, Skip
    | Assert True, x | x, Assert True
    | Assume True, x | x, Assume False
    -> x
  | Assert False, _ | _, Assert False
    -> Assert False
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
      "apply (" ^ name ^ ",("
      ^ List.fold_left keys ~init:""
          ~f:(fun str (k,sz) ->
            str ^ "," ^ k ^ "#" ^ string_of_int sz) ^ ")"
      ^ "," ^ List.fold_left acts ~init:""
                ~f:(fun str a ->
                  str ^ " | { fun (SOME ACTION DATA) -> " ^ string_of_cmd (snd a) ^ "}")
      ^ ", {" ^ string_of_cmd default ^ "})"
                                       
            
  
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
     ^ List.fold_left keys ~init:"" ~f:(fun str (k,sz) -> str ^ ";\"" ^ k ^ "\"," ^ string_of_int sz ^ "")
     ^ "],["
     ^ List.fold_left actions ~init:"" ~f:(fun str a -> str ^ ";((SOME ACTION DATA), " ^ sexp_string_of_cmd (snd a) ^")")
     ^ "]," ^ sexp_string_of_cmd default
     ^ ")"

let rec tables_of_cmd (c:cmd) : string list =
  match c with
  | Skip | Assign _  | Assume _ | Assert _  -> []
  | Seq (c,c') -> tables_of_cmd c @ tables_of_cmd c'
  | Select (_, cs) -> concatMap cs ~c:(@) ~init:(Some []) ~f:(fun (_, c) -> tables_of_cmd c)
  | While (_,c) -> tables_of_cmd c
  | Apply (s,_,_,_) -> [s]
                                                              

    
let rec free_of_cmd typ (c:cmd) : (string * size) list =
  begin match c with
  | Skip -> []
  | Assign (f, e) ->  (f,size_of_expr1 e) :: free_of_expr1 typ e
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
       ~f:(fun acc (data, a) ->
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
      
let rec multi_ints_of_cmd c : (int * size) list =
  match c with
  | Skip -> []
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
       ~f:(fun rst act -> rst @ multi_ints_of_cmd (snd act))


let rec holify_expr1 holes (e : expr1) : expr1 =
  match e with
  | Hole1 _ | Value1 _ -> e
  | Var1 (x,sz) ->
     begin match List.find holes ~f:(fun elem -> x = elem)  with
     | None -> e
     | Some _ -> Hole1 ((*"?" ^*) x, sz)
     end
  | Plus (e,e') -> Plus (holify_expr1 holes e, holify_expr1 holes  e')
  | Times (e,e') -> Times (holify_expr1 holes  e, holify_expr1 holes  e')
  | Minus (e,e') -> Minus (holify_expr1 holes  e, holify_expr1 holes  e')
  | Tuple es -> List.map es ~f:(holify_expr1 holes) |>  Tuple 

and holify_expr2 holes (e : expr2) : expr2 =
  match e with
  | Value2 _ -> e
  | Hole2 _ -> e
  | Var2 (x,sz) ->
     begin match List.find holes ~f:(fun elem -> x = elem) with
     | None -> e 
     | Some _ -> Hole2 ("?" ^ x,sz)
     end
  | Single e -> Single (holify_expr1 holes e)
  | Union (e,e') -> Union (holify_expr2 holes e, holify_expr2 holes e')
and holify_test holes b : test =
  match b with
  | True | False -> b
  | Eq (e, e') -> holify_expr1 holes  e %=% holify_expr1 holes  e'
  | Lt (e, e') -> holify_expr1 holes  e %<% holify_expr1 holes  e'
  | Member (expr, set) -> mkMember (holify_expr1 holes expr) (holify_expr2 holes set)
  | And (b, b') -> holify_test holes b %&% holify_test holes b'
  | Or (b, b')  -> holify_test holes b %+% holify_test holes b'
  | Impl(b,b') -> holify_test holes b %=>% holify_test holes b'
  | Iff(b, b') -> holify_test holes b %=>% holify_test holes b'
  | Neg b       -> !%(holify_test holes b)
and holify_cmd holes c : cmd=
  match c with
  | Skip -> c
  | Assign (f, e) -> f %<-% holify_expr1 holes e
  | Assert t -> Assert (holify_test holes t)
  | Assume t -> Assume (holify_test holes t)
  | Seq (c, c') ->
     holify_cmd holes c %:% holify_cmd holes c'
  | While (t, c) ->
     mkWhile (holify_test holes t) (holify_cmd holes c)
  | Select (styp, cases) ->
     List.map cases ~f:(fun (t, c) -> holify_test holes t, holify_cmd holes c)
     |> mkSelect styp
  | Apply (name,keys,acts,dflt)
    -> Apply(name, keys, List.map acts ~f:(fun (data, act) ->
                             let holes' = List.filter holes ~f:(fun h ->
                                              List.for_all (List.map data ~f:fst) ~f:((<>) h)
                                            ) in
                             (data, holify_cmd holes' act)),
             holify_cmd holes dflt)
              
    
(** replace all vars in cmd that are also in holes with holes having the same name*)
let holify holes c =  holify_cmd holes c
        
