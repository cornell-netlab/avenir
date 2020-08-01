open Core
open Util

let (=) = Stdlib.(=)
let (<) = Stdlib.(<)
let (<=) = Stdlib.(<=)
let (<>) = Stdlib.(<>)

let enable_smart_constructors = true

type size = int

type value =
  | Int of (Bigint.t * size)

let string_of_value (v : value) : string =
  match v with
  | Int (i,x) -> Printf.sprintf "%s#%d" (Bigint.Hex.to_string i) x

let veq v v' =
  match v,v' with
  | Int (i,sz), Int(i',sz') when sz = sz' ->  Bigint.equal i i'
  | Int (i,sz), Int(i',sz') -> failwith @@ Printf.sprintf "Ints are different sizes: %s#%d = %s#%d " (Bigint.Hex.to_string i) sz (Bigint.Hex.to_string i') sz'

let vleq v v' =
  match v, v' with
  | Int(i,sz), Int(i',sz') when sz = sz' -> Bigint.(<=) i i'
  | Int(i,sz), Int(i',sz') -> failwith @@ Printf.sprintf "Ints are different sizes: %s#%d <= %s#%d " (Bigint.Hex.to_string i) sz (Bigint.Hex.to_string i') sz'

type expr =
  | Value of value
  | Var of (string * size)
  | Hole of (string * size)
  | Plus of (expr * expr)
  | Times of (expr * expr)
  | Minus of (expr * expr)
  | Mask of (expr * expr)
  | Xor of (expr * expr)
  | BOr of (expr * expr)
  | Shl of (expr * expr)
  | Concat of (expr * expr)
  | Cast of (int * expr)
  | Slice of {hi : int; lo: int; bits: expr}

let mkInt (i,sz) =
  if i < 0 then failwith "Negative integers not representable" else
  Int (Bigint.of_int_exn i, sz)
let mkVInt i = Value (mkInt i)
let mkCast i e = Cast(i,e)
let mkPlus e e' = Plus(e,e')
let mkMinus e e' = Minus (e, e')
let mkTimes e e' = Times (e, e')
let mkMask e e' = Mask(e,e')
let mkXor e e' = Xor(e,e')
let mkBOr e e' = BOr (e,e')
let mkShl e e' = Shl (e,e')
let mkConcat e e' = Concat(e,e')
let mkSlice hi lo bits = Slice {hi; lo; bits}

let ctor_for_binexpr =
  function
  | Hole _ | Value _ | Var _ | Cast _ | Slice _ -> failwith "[bin_ctor_for_expr] received hole, value, var, cast, or slice"
  | Plus _ -> mkPlus
  | Times _ -> mkTimes
  | Minus _ -> mkMinus
  | Mask _ -> mkMask
  | Xor _ -> mkXor
  | BOr _ -> mkBOr
  | Shl _ -> mkShl
  | Concat _ -> mkConcat

let rec string_of_expr (e : expr) : string =
  let string_binop e1 op e2 = Printf.sprintf "(%s %s %s)" (string_of_expr e1) op (string_of_expr e2) in
  match e with
  | Value v -> string_of_value v
  | Var (x,s) -> x ^ "#" ^ string_of_int s
  | Hole (x,s) -> "?" ^ x ^ "#" ^ string_of_int s
  | Cast (i,e)    -> Printf.sprintf "((<%d>) %s)" i (string_of_expr e)
  | Plus (e, e')  -> string_binop e "+" e'
  | Times (e, e') -> string_binop e "*" e'
  | Minus (e, e') -> string_binop e "-" e'
  | Mask (e,e')   -> string_binop e "&" e'
  | Xor (e,e')    -> string_binop e "^" e'
  | BOr (e,e')    -> string_binop e "|" e'
  | Shl (e,e')    -> string_binop e "<<" e'
  | Concat (e,e') -> string_binop e "@" e'
  | Slice {hi; lo; bits} -> Printf.sprintf "%s[%d:%d]" (string_of_expr bits) hi lo


let sexp_string_of_value (v : value) =
  match v with
  | Int (i,sz) -> Printf.sprintf "Int(%s,%d)" (Bigint.to_string i) sz
                   
let rec sexp_string_of_expr (e : expr) =
  let string_binop ctor (e1, e2) = Printf.sprintf "%s(%s, %s)" ctor (sexp_string_of_expr e1) (sexp_string_of_expr e2) in
  match e with
  | Value v -> "Value(" ^ sexp_string_of_value v ^ ")"
  | Var (x, s) -> "Var(\"" ^ x ^ "\"," ^ string_of_int s ^ ")"
  | Hole (x, s) -> "Hole(\"" ^ x ^ "\"," ^ string_of_int s ^ ")"  
  | Cast (i,e) -> Printf.sprintf "Cast(%d, %s)" i (string_of_expr e)
  | Plus es -> string_binop "Plus" es
  | Times es -> string_binop "Times" es
  | Minus es -> string_binop "Minus" es
  | Mask es -> string_binop "Mask" es
  | Xor es -> string_binop "Xor" es
  | BOr es     -> string_binop "BOr" es
  | Shl es     -> string_binop "Shl" es
  | Concat es -> string_binop "Concat" es
  | Slice {hi;lo;bits} -> Printf.sprintf "Slice {hi=%d;lo=%d;bits=%s}" hi lo (sexp_string_of_expr bits)

let get_int (v : value) : Bigint.t =
  match v with
  | Int (x, _) -> x
                  
let size_of_value (v : value) : size =
  match v with
  | Int (_, s) -> s
                           
let rec size_of_expr (e : expr) : size =
  match e with
  | Value v -> size_of_value v
  | Var (_,s) -> s
  | Hole (_,s) -> s
  | Cast (i,_) -> i
  | Concat (e,e') -> size_of_expr e + size_of_expr e'
  | Plus (e, e') | Minus(e,e') | Times (e,e') | Mask(e,e') | Xor (e,e') | BOr (e,e') | Shl (e,e') ->
     let s = size_of_expr e in
     let s' = size_of_expr e' in
     if s = s' then s
     else
       if s = -1 || s' = -1
       then -1
       else
         failwith (Printf.sprintf "size of expressions: %s, and %s differs (%d and %d)"
                     (string_of_expr e)
                     (string_of_expr e')
                     s s')
  | Slice {hi;lo;_} -> let sz = hi - lo in
                       if sz < 0 then -1 else sz

let rec num_nodes_in_expr e =
  match e with
  | Value _ | Var _ | Hole _ -> 1
  | Cast (_,e) | Slice {bits=e;_}-> 1 + num_nodes_in_expr e
  | Plus (e,e') | Minus(e,e') | Times(e,e') | Mask(e,e') | Xor (e,e') | BOr (e,e') | Shl (e,e') | Concat (e,e') ->
     num_nodes_in_expr e
     + num_nodes_in_expr e'
     + 1


                   
let add_values (v : value) (v' : value) : value =
  match v, v' with
  | Int (x, sz), Int (x',sz') when sz = sz' -> Int (Bigint.(x + x'),sz)
  | Int (x, sz), Int(x',sz') ->
     failwith (Printf.sprintf "Type error %s#%d and %s#%d have different bitvec sizes" (Bigint.to_string x) sz (Bigint.to_string x') sz')

let multiply_values (v : value) (v' : value) : value =
  match v, v' with
  | Int (x, sz), Int (x',sz') when sz = sz' -> Int (Bigint.(x * x'), sz)
  | Int (x,sz), Int (x',sz') -> failwith (Printf.sprintf "Type error %s#%d and %s#%d have different bitvec sizes" (Bigint.to_string x) sz (Bigint.to_string x') sz')


let subtract_values (v : value) (v' : value) : value =
  match v, v' with
  | Int (x, sz), Int (x',sz') when sz = sz' ->
     (* if Bigint.(x-x' < zero)
      * then Int (Bigint.zero, sz)
      * else  *)Int (Bigint.(x - x'), sz)
  | Int (x, sz), Int (x', sz') ->
     failwith (Printf.sprintf "Type error %s#%d and %s#%d have different bitvec sizes" (Bigint.to_string x) sz (Bigint.to_string x') sz')

let mask_values (v : value) (v' : value) : value =
  match v,v' with
  | Int (x, sz), Int (x',sz') when sz = sz' -> Int (Bigint.(x land x'), sz)
  | Int (x, sz), Int (x', sz') ->
     failwith (Printf.sprintf "MASK: Type error %s#%d and %s#%d have different BV sizes" (Bigint.to_string x) sz (Bigint.to_string x') sz')

let xor_values (v : value) (v' : value) : value =
  match v,v' with
  | Int (x, sz), Int (x',sz') when sz = sz' -> Int (Bigint.(x lxor x'), sz)
  | Int (x, sz), Int (x', sz') ->
     failwith (Printf.sprintf "MASK: Type error %s#%d and %s#%d have different BV sizes" (Bigint.to_string x) sz (Bigint.to_string x') sz')

let or_values (v : value) (v' : value) : value =
  match v,v' with
  | Int (x, sz), Int (x',sz') when sz = sz' -> Int (Bigint.(x lor x'), sz)
  | Int (x, sz), Int (x', sz') ->
     failwith (Printf.sprintf "MASK: Type error %s#%d and %s#%d have different BV sizes" (Bigint.to_string x) sz (Bigint.to_string x') sz')

let shl_values (v : value) (v' : value) : value =
  match v,v' with
  | Int (x, sz), Int (x',_) ->
     Int (Bigint.(shift_left x @@ to_int_exn x'), sz)

let cast_value w (v : value) : value =
  match v with
  | Int(x,sz) ->
     if w < sz then
       Int(Bigint.(x land ((pow (one + one) (of_int w)) - one)), sz)
     else if w > sz then
       Int(x,w)
     else
       v

let slice_value (hi : size) (lo : size) (v : value) : value =
  match v with
  | Int(x,sz) ->
     if hi > sz || lo > sz then failwith "index out of range"
     else
       let sz' = hi - lo in
       let mask = Bigint.((pow (of_int 2) (of_int sz')) - one) in
       let x' = Bigint.((shift_right x lo) land mask) in
       Int (x', sz')

let concat_values (l : value) (r : value) : value =
  match l,r with
  | Int (lx,lsz), Int(rx, rsz) ->
     Int(Bigint.(shift_left lx rsz + rx)
       , lsz + rsz)

type test =
  | True | False
  | Eq of (expr * expr)
  | Le of (expr * expr)
  | And of (test * test)
  | Or of (test * test)
  | Impl of (test * test)
  | Iff of (test * test)
  | Neg of test       

let rec string_of_test t =
  match t with
  | True -> "true"
  | False -> "false"
  | Eq (left, right) -> string_of_expr left ^ " = " ^ string_of_expr right
  | Le (left, right) -> string_of_expr left ^ " <= " ^ string_of_expr right
  | Impl (assum, conseq) -> "(" ^ string_of_test assum ^ " ==> " ^ string_of_test conseq ^ ")\n"
  | Iff (left, right) -> "(" ^ string_of_test left ^ " <==> " ^ string_of_test right ^ ")\n"
  | Or (left, right) -> "(" ^ string_of_test left ^ " || " ^ string_of_test right ^ ")"
  | And (left, right) -> "(" ^ string_of_test left ^ " && " ^ string_of_test right ^ ")"
  | Neg (Le(left, right)) ->
     Printf.sprintf "(%s < %s)" (string_of_expr right) (string_of_expr left)
  | Neg(Eq(left,right)) ->
     Printf.sprintf "(%s <> %s)" (string_of_expr left) (string_of_expr right)
  | Neg t ->
     "~(" ^ string_of_test t ^ ")"

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
        | Eq(Var x,u), Neg(Eq(Var y,v)) when x = y
          -> if u = v then False
             else Eq(Var x,u)
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

             
let mkEq (e : expr) (e':expr) =
  if not enable_smart_constructors then Eq(e,e') else 
  if e = e' then
    ((*Printf.printf "[=] %s and %s are equal, so True\n" (string_of_expr e) (string_of_expr e');*)
    True) else
    let ord e = match e with
      | Var _ -> 0
      | Hole _ -> 1
      | Cast _ -> 2
      | Plus _ -> 3
      | Minus _ -> 4
      | Times _ -> 5
      | Value _ -> 7
      | Mask _ -> 8
      | Xor _ -> 9
      | BOr _ -> 10
      | Shl _ -> 11
      | Slice _ -> 12
      | Concat _ -> 13
    in
    let norm = match e, e' with
    | Value _, Value _
      ->
       (*Printf.printf "[=] %s and %s are unequal values so False\n" (string_of_expr e) (string_of_expr e');*)
       False
    | Hole x , Hole x'
    | Var x, Var x'
      -> if x < x'
         then Eq (e, e')
         else Eq (e', e)           
    | _, _ ->
       if ord e < ord e'
       then Eq (e, e')
       else Eq (e', e)
    in
    match norm with
    | Eq(Value(Int(_,_)), Mask(Var(_,_), Value(Int(m,_))))
         when Bigint.(m = zero) -> True
    | _ -> norm


let mkLe (e : expr) (e' : expr) : test = Le(e,e')
  (* if not enable_smart_constructors then Le(e,e') else
   * match e, e' with
   *   | Value(Int first), Value(Int second) ->
   *      if first <= second then True else False
   *   | _, _ -> Le(e, e') *)
            
let (%=%) = mkEq
let (%<>%) v v' = Neg(v %=% v') 

let (%<=%) = mkLe
let (%>=%) e e' = e' %<=% e
let (%<%) e e' = !%(e %>=% e')
let (%>%) e e' = !%(e %<=% e')

let mkImplies assum conseq =
  if not enable_smart_constructors then Impl(assum, conseq) else
    if assum = conseq then True else
    match assum, conseq with
    | True, _ -> conseq
    | _, True | False, _ -> True
    | _, False -> !%(assum)
    | _, _ -> Impl(assum, conseq)

let mkImpl = mkImplies

let (%=>%) = mkImpl

let mkIff lhs rhs =
  if enable_smart_constructors
  then if lhs = rhs then True else Iff(lhs, rhs)
  else Iff(lhs, rhs)
let (%<=>%) = mkIff


let rec sexp_string_of_test t =
  let binop opname left right recfun : string=
    opname ^ "(" ^ recfun left ^ "," ^ recfun right ^ ")" in
  match t with
  | True -> "True"
  | False -> "False"
  | Eq  (left, right) -> binop "Eq" left right sexp_string_of_expr
  | Le  (left, right) -> binop "Le" left right sexp_string_of_expr
  | Impl (assum, conseq) -> binop "Impl" assum conseq sexp_string_of_test
  | Iff (left, right) -> binop "Iff" left right sexp_string_of_test
  | Or  (left, right) -> binop "Or" left right sexp_string_of_test
  | And (left, right) -> binop "And" left right sexp_string_of_test
  | Neg t -> "Neg(" ^ sexp_string_of_test t ^ ")"

let rec num_nodes_in_test t =
  match t with
  | True | False -> 1
  | Eq (left, right) -> num_nodes_in_expr left + num_nodes_in_expr right + 1
  | Le (left, right) -> num_nodes_in_expr left + num_nodes_in_expr right + 1
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

let rec free_of_expr typ e : (string * size) list =
  match e, typ with
  | Value _, _ -> []
  | Var (x,sz), `Var  | Hole (x,sz), `Hole -> [x,sz]
  | Var _ , `Hole -> []
  | Hole _ , `Var -> []
  | Cast(_,e),_ | Slice {bits=e;_},_ -> free_of_expr typ e
  | Plus(e,e'),_ | Times (e,e'),_ | Minus(e,e'),_ | Mask (e,e'), _ | Xor (e,e'), _ | BOr (e,e'), _ | Shl (e,e'), _ | Concat (e,e'),_
    -> free_of_expr typ e @ free_of_expr typ e'

let rec free_of_test typ test : (string * size) list =
  begin match test with
  | True | False -> 
    []
  | Or (l,r) | And (l, r) | Impl (l,r) | Iff (l, r) ->
     free_of_test typ l @ free_of_test typ r
  | Neg t ->
     free_of_test typ t
  | Eq (e, e') | Le (e, e') ->
     free_of_expr typ e @ free_of_expr typ e'
  end
  |> dedup

let free_vars_of_test t : (string * size) list=
  let vs = free_of_test `Var t in
  vs
            
                
let holes_of_test = free_of_test `Hole

let rec has_hole_expr = function
  | Value _ | Var _  -> false
  | Hole _ -> true
  | Cast (_,e) | Slice {bits=e;_} -> has_hole_expr e
  | Plus (e1,e2) | Minus(e1,e2) | Times (e1,e2) | Mask (e1,e2) | Xor (e1,e2) | BOr (e1,e2) | Shl (e1,e2) | Concat (e1,e2)
    -> has_hole_expr e1 || has_hole_expr e2
                                 
let rec has_hole_test = function
  | True | False -> false
  | Neg b -> has_hole_test b
  | And (a,b) | Or(a,b) | Impl (a,b) | Iff(a,b) ->
     has_hole_test a || has_hole_test b
  | Eq (e1,e2) | Le (e1,e2) ->
     has_hole_expr e1 || has_hole_expr e2
                                 

let multi_ints_of_value e : (Bigint.t * size) list =
  match e with
  | Int (i,sz) -> [i,sz]

let rec multi_ints_of_expr e : (Bigint.t * size) list =
  match e with
  | Value v -> multi_ints_of_value v
  | Var _ | Hole _ -> []
  | Cast (_,e) | Slice {bits=e;_} -> multi_ints_of_expr e
  | Plus (e,e') | Times (e,e') | Minus (e,e')  | Mask (e,e') | Xor (e,e') | BOr (e,e') | Shl (e,e') | Concat (e,e')
    -> multi_ints_of_expr e @ multi_ints_of_expr e'
                  
let rec multi_ints_of_test test : (Bigint.t * size) list =
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
    | Eq (e, e') | Le (e, e') ->
       multi_ints_of_expr e @ multi_ints_of_expr e'
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
  | Assign of (string * expr)
  | Assert of test
  | Assume of test
  | Seq of (cmd * cmd)
  | While of (test * cmd)
  | Select of (select_typ * ((test * cmd) list))
  | Apply of {name:string;
              keys:(string * size) list;
              actions:(((string * size) list * cmd) list);
              default: cmd}


let clean_selects_list ss = ss
  (* List.filter ~f:(fun (c,a) -> c <> False) *)
  (* List.rev ss
   * |> List.fold ~init:([], [])
   *   ~f:(fun (acc,seen) (c,a) ->
   *     if c = False || List.exists seen ~f:((=) c) then
   *       (acc,seen)
   *     else
   *       ((c,a) :: acc, c :: seen)
   *   )
   * |> fst *)

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


let mkApply (name,keys,actions,default) = Apply{name;keys;actions;default}

let (%:%) = mkSeq

let mkAssn f v =
  match v with
  | Var(x, _) when x = f -> Skip
  | _ -> Assign (f, v)
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
  | Skip ->    repeat "\t" depth ^ "skip"
  | While (cond, body) ->
    "\n" ^ repeat "\t" depth ^
    "while(" ^ string_of_test cond ^ ") {\n"
      ^ repeat "\t" (depth+1)
      ^ string_of_cmd ~depth:(depth+1) body
      ^ "\n" ^ repeat "\t" depth
      ^ "}\n" ^ repeat "\t" depth
  | Seq (firstdo, thendo) ->
     string_of_cmd ~depth firstdo ^ ";\n "
    ^ string_of_cmd ~depth thendo
  | Assert t ->
    repeat "\t" depth ^
    "assert (" ^ string_of_test t ^ ")"
  | Assume t ->
    repeat "\t" depth ^
    "assume (" ^ string_of_test t ^ ")"
  | Assign (field, expr) ->
    repeat "\t" depth ^
    field ^ " := " ^ string_of_expr expr
  | Select (styp, es) ->
    let modifier = (string_of_select_typ styp) in
    repeat "\t" depth ^
      "if " ^ modifier ^
    List.fold es ~init:"" ~f:(fun str (cond, act)->
        str ^ "\n" ^
        repeat "\t" (depth + 1)
        ^ string_of_test cond  ^ " ->\n " ^ string_of_cmd ~depth:(depth+2) act ^ " []"
      )
    ^ "\n" ^ repeat "\t" depth ^ "fi"
  | Apply t ->
     repeat "\t" depth ^
       "apply (" ^ t.name ^ ",("
      ^ List.fold t.keys ~init:""
          ~f:(fun str (k,sz) ->
            str ^ k ^ "#" ^ string_of_int sz ^ ",") ^ ")"
      ^ ",(" ^ List.foldi t.actions ~init:""
                ~f:(fun i str a ->
                  str ^ (if i > 0 then " |" else "") ^ " { " ^
                    (if List.length (List.nth_exn t.actions i |> fst) > 0 then "\\ ("^
                    List.fold (List.nth_exn t.actions i |> fst) ~init:""
                    ~f:(fun acc (x,sz) -> Printf.sprintf "%s%s#%d," acc x sz)
                    ^") -> " else "")
                    ^ string_of_cmd (snd a) ^ "}")
      ^ "), {" ^ string_of_cmd t.default ^ "})"
                                       
            
  
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
  | Assign (f,e) -> "Assign(" ^ f ^ "," ^ string_of_expr e ^")"
  | Select (styp,es) ->
    let cases_string = match es with
      | [] -> "[]"
      | _  -> "[" ^ string_select es ^ "]"
    in
    "Select (" ^ sexp_string_of_select_typ styp ^ "," ^ cases_string ^ ")"
  | Apply t ->
     "Apply("
     ^ t.name ^ ",["
     ^ List.fold_left t.keys ~init:"" ~f:(fun str (k,sz) -> str ^ ";\"" ^ k ^ "\"," ^ string_of_int sz ^ "")
     ^ "],["
     ^ List.fold_left t.actions ~init:"" ~f:(fun str a -> str ^ ";((SOME ACTION DATA), " ^ sexp_string_of_cmd (snd a) ^")")
     ^ "]," ^ sexp_string_of_cmd t.default
     ^ ")"

let get_test_from_assume = function
  | Assume t -> t
  | c -> failwith @@ Printf.sprintf "tried to get test from command %s" (string_of_cmd c)

let rec tables_of_cmd (c:cmd) : string list =
  match c with
  | Skip | Assign _  | Assume _ | Assert _  -> []
  | Seq (c,c') -> tables_of_cmd c @ tables_of_cmd c'
  | Select (_, cs) -> concatMap cs ~c:(@) ~init:(Some []) ~f:(fun (_, c) -> tables_of_cmd c)
  | While (_,c) -> tables_of_cmd c
  | Apply t -> [t.name]
                                                              

    
let rec free_of_cmd typ (c:cmd) : (string * size) list =
  begin match c with
  | Skip -> []
  | Assign (f, e) ->  (f,size_of_expr e) :: free_of_expr typ e
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
  | Apply t ->
     t.keys
     @ List.fold t.actions
         ~init:(free_of_cmd typ t.default)
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
      
let rec multi_ints_of_cmd c : (Bigint.t * size) list =
  match c with
  | Skip -> []
  (* | Assign (_, Int i) ->
   *   [i] *)
  (* Only collect _tested_ inputs*)
  | Assign (_,e) -> multi_ints_of_expr e
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
  | Apply t ->
     List.fold t.actions ~init:(multi_ints_of_cmd t.default)
       ~f:(fun rst act -> rst @ multi_ints_of_cmd (snd act))

let rec holify_expr holes (e : expr) : expr =
  let binop ctor (e,e') = ctor (holify_expr holes e) (holify_expr holes e') in
  match e with
  | Hole _ | Value _ -> e
  | Var (x,sz) ->
     begin match List.find holes ~f:(fun elem -> x = elem)  with
     | None -> e
     | Some _ -> Hole ((*"?" ^*) x, sz)
     end
  | Cast (i,e) -> mkCast i @@ holify_expr holes e
  | Slice {hi;lo;bits} -> mkSlice hi lo @@ holify_expr holes bits
  | Plus es
    | Times es
    | Minus es
    | Mask es
    | Xor es
    | BOr es
    | Shl es
    | Concat es
    -> binop (ctor_for_binexpr e) es
                          
and holify_test holes b : test =
  match b with
  | True | False -> b
  | Eq (e, e') -> holify_expr holes  e %=% holify_expr holes  e'
  | Le (e, e') -> holify_expr holes  e %<=% holify_expr holes  e'
  | And (b, b') -> holify_test holes b %&% holify_test holes b'
  | Or (b, b')  -> holify_test holes b %+% holify_test holes b'
  | Impl(b,b') -> holify_test holes b %=>% holify_test holes b'
  | Iff(b, b') -> holify_test holes b %=>% holify_test holes b'
  | Neg b       -> !%(holify_test holes b)
                     
and holify_cmd holes c : cmd=
  match c with
  | Skip -> c
  | Assign (f, e) -> f %<-% holify_expr holes e
  | Assert t -> Assert (holify_test holes t)
  | Assume t -> Assume (holify_test holes t)
  | Seq (c, c') ->
     holify_cmd holes c %:% holify_cmd holes c'
  | While (t, c) ->
     mkWhile (holify_test holes t) (holify_cmd holes c)
  | Select (styp, cases) ->
     List.map cases ~f:(fun (t, c) -> holify_test holes t, holify_cmd holes c)
     |> mkSelect styp
  | Apply t
    -> Apply {name=t.name;
              keys=t.keys;
              actions = List.map t.actions ~f:(fun (data, act) ->
                            let holes' = List.filter holes ~f:(fun h ->
                                             List.for_all (List.map data ~f:fst) ~f:((<>) h)
                                           ) in
                            (data, holify_cmd holes' act));
             default = holify_cmd holes t.default}
              
    
(** replace all vars in cmd that are also in holes with holes having the same name*)
let holify holes c =  holify_cmd holes c
        
let sequence = List.reduce_exn ~f:(%:%)

let rec get_schema_of_table name phys =
  match phys with
  | Skip 
    | Assume _
    | Assert _
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
    -> if name = t.name then Some (t.keys, t.actions,t.default) else None
  | While (_, c) -> get_schema_of_table name c
                 

let rec get_tables_actsizes = function
  | Skip | Assume _ | Assert _ | Assign _ -> []
  | Seq (c1,c2) -> get_tables_actsizes c1 @ get_tables_actsizes c2
  | Select(_,cs) ->
     List.fold cs ~init:[] ~f:(fun acc (_,c) -> acc @ get_tables_actsizes c)
  | Apply t -> [t.name, List.length t.actions]
  | While(_,c) -> get_tables_actsizes c

let table_vars ?keys_only:(keys_only=false) (keys, acts, default) =
  let open List in
  keys
  @ if keys_only then
      []
    else
      (acts >>= fun (ad,c) ->
       free_vars_of_cmd c
       |> filter ~f:(fun (x,_) ->
              for_all ad ~f:(fun (y,_) -> x <> y)))
      @ free_vars_of_cmd default

let rec get_tables_vars ?keys_only:(keys_only=false) = function
  | Skip | Assume _ | Assert _ | Assign _ -> []
  | Seq (c1,c2) -> get_tables_vars ~keys_only c1 @ get_tables_vars ~keys_only c2
  | Select(_,cs) ->
     List.fold cs ~init:[] ~f:(fun acc (_,c) -> acc @ get_tables_vars ~keys_only c)
  | Apply t -> [t.name, table_vars ~keys_only (t.keys, t.actions, t.default)]
  | While(_,c) -> get_tables_vars ~keys_only c

let string_of_map m =
  StringMap.fold ~f:(fun ~key:k ~data:v acc -> ("(" ^ k ^ " -> " ^ (string_of_value v) ^ ") " ^ acc)) m ~init:""
