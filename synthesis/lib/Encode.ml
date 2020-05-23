open Core
open Util
open Petr4
open Types
open Ast
open Manip

let safe_name (d : Declaration.t) =
  let module D = Declaration in
  try Some (snd (D.name d))
  with _ -> None

let rec typ_safe_name (t : Type.t) =
  let module T = Type in
  match snd t with
    | T.HeaderStack {header} -> typ_safe_name header
    | T.TypeName n -> Some (snd n)
    | _ -> None
 
(* Type lookup *)

let rec lookup_type_width (type_ctx : Declaration.t list) (typ : Type.t) =
  let open Expression in
  let open Type in
  let open Declaration in
  match snd typ with
    | Bool -> Some 1
    | Error -> Some (-1)
    | BitType e ->
      begin match snd e with
        | Int (_, i) -> Some (Bigint.to_int_exn i.value)
        | _ -> failwith "lookup_type_width: Unimplemented expr"
      end

    | TypeName n ->
      let t = List.find type_ctx
                        ~f:(fun td -> match td with
                                        | (_, TypeDef d) -> snd d.name = snd n
                                        | (_, Header h) -> snd h.name = snd n
                                        (* | (_, Struct s) -> snd s.name = snd n *)
                                        | _ -> false ) in
      begin match t with
        | Some (_, TypeDef{typ_or_decl;_}) ->
          begin match typ_or_decl with
            | Util.Left t2 -> lookup_type_width type_ctx t2
            | Util.Right _ -> failwith "lookup_type_width: Unimplemented decl"
           end
        | Some (_, Header{fields;_}) ->
          Some (List.fold (List.map fields ~f:(fun (_, fl) -> lookup_field_width_exn type_ctx (snd fl.name))) ~f:(+) ~init:0)
        | Some (_, Struct{fields;_}) ->
            Some (List.fold (List.map fields ~f:(fun (_, fl) -> lookup_field_width_exn type_ctx (snd fl.name))) ~f:(+) ~init:0)
        | _ -> failwith ("lookup_type_width: 1 type not handled " 
                              ^ Sexp.to_string ([%sexp_of: Type.t] typ) ^ " "
                              ^ Sexp.to_string ([%sexp_of: Declaration.t option] t))
      end
    | HeaderStack { header; size} -> Option.map (lookup_type_width type_ctx header) ~f:(fun w -> w * encode_expr_to_int size)
    | _ -> failwith ("lookup_type_width: 2 type not handled " ^ Sexp.to_string ([%sexp_of: Type.t] typ))

and lookup_type_width_exn (type_ctx : Declaration.t list) typ : size =
  match lookup_type_width type_ctx typ with
    | Some w -> w
    | None -> failwith ("lookup_type_width_exn: lookup failed")

and gather_fields type_ctx =
  let open Declaration in
  List.filter_map type_ctx ~f:(fun d -> match d with
                                     | (_, Header {fields;_}) -> Some fields
                                     | (_, Struct {fields;_}) -> Some fields
                                     | _ -> None)
  |> List.concat

and check_typedef_widths type_ctx n =
  let open Declaration in
  List.find_map type_ctx ~f:(fun d -> match d with
                                    | (_, TypeDef {name; typ_or_decl = Left t;_}) ->
                                        if n = snd name then Some t else None
                                    | _ -> None)

(* and lookup_field_width (type_ctx : Declaration.t list) fn : size option =
  let open Declaration in
  match fn with
  | "ingress_port" -> Some 9 (* TODO: Is this right? *)
  | "parser_error" -> Some 1 (* TODO: Is this right? *)
  | "isValid" -> Some 1
  | _ -> 
    let flds = gather_fields type_ctx in
    match List.find flds ~f:(fun (_, f) -> snd f.name = fn) with
      | Some (_, f) -> lookup_type_width type_ctx f.typ
      | None -> Option.bind (check_typedef_widths type_ctx fn) ~f:(lookup_type_width type_ctx)
*)

and lookup_field_width (type_ctx : Declaration.t list) fn : size option =
  let open Declaration in
  let open Info in 
  let split = String.split fn ~on: '.' in
  let split_hd = List.hd_exn split  in
  let split_tl = List.tl_exn split in
  let split_last = List.last_exn split in
  match split_last with
  | "ingress_port" -> Some 9
  | "egress_spec" -> Some 9
  | "dst_vlan" -> Some 12
  | "parser_error" -> Some 1
  | "isValid" -> Some 1
  | _ -> begin
   (* Checking for "M v" here is a hack, to get around that update_typ_ctx_from_param doesn't update
    * the name field in the declaration.  It would be preferable to do this, but type inference seems to
    * fail for the record update (and I was unable to figure out how to explicitly write the type.) *)
    match List.find type_ctx ~f:(fun d -> Some split_hd = safe_name d || M split_hd = fst d) with
    | Some (_, TypeDef { typ_or_decl = Left t}) -> lookup_type_width type_ctx t
    | Some d ->
        if split_tl = []
          then Option.bind (check_typedef_widths type_ctx fn) ~f:(lookup_type_width type_ctx)
          else lookup_field_width' type_ctx d split_tl 
    | None -> None
    end

and lookup_field_width' (type_ctx : Declaration.t list) (decl : Declaration.t) fn : size option =
  let open Declaration in
  match fn with
  | f :: fs ->
    begin match snd decl with
    | Struct {fields} ->
      let fld = match List.find fields ~f:(fun fl -> snd (snd fl).name = f) with
                | Some fl -> fl
                | None -> failwith "lookup_field_width': field not found" in
      begin match List.find type_ctx ~f:(fun d -> safe_name d = typ_safe_name (snd fld).typ) with
       | Some d -> if fs = []
                        then lookup_type_width type_ctx (snd fld).typ
                        else lookup_field_width' type_ctx d fs
       | None -> failwith "lookup_field_width': decl not found"
      end
    | Header {fields} ->
       begin match List.find fields ~f:(fun fl -> snd (snd fl).name = f) with
                | Some fl -> lookup_type_width type_ctx (snd fl).typ
                (* Account for header stacks*)
                | None -> if is_int f
                            then lookup_field_width' type_ctx decl fs
                            else failwith "lookup_field_width': field not found"
       end
    | _ -> failwith ("f = " ^ f ^ "\ndecl = " ^ Sexp.to_string ([%sexp_of: Declaration.pre_t] (snd decl)))
    end
  | [] -> failwith "lookup_field_width': empty list"

and is_int n =
  try ignore (int_of_string n : int); true 
  with _ -> false

(*
and lookup_field_width' (type_ctx : Declaration.t list) (red_type_ctx : Declaration.t list)  fn : size option =
  let open Declaration in
  match fn with
  | ["ingress_port"] -> Some 9
  | ["parser_error"] -> Some 1
  | ["isValid"] -> Some 1
  | [f] ->
    let flds = gather_fields type_ctx in
    begin match List.find flds ~f:(fun (_, f') -> snd f'.name = f) with
    | Some (_, f) -> lookup_type_width red_type_ctx f.typ
    | None -> None (* Option.bind (check_typedef_widths type_ctx fn) ~f:(lookup_type_width type_ctx) *)
    end
  | f :: fs ->
    let red_type_ctx' = red_type_ctx in
    lookup_field_width' type_ctx red_type_ctx'  fs
*)

and lookup_field_width_exn (type_ctx : Declaration.t list) field : size =
(* let last_field = List.last_exn (String.split field ~on:'.') in *)
  match lookup_field_width type_ctx field with
    | Some i -> i
    | None -> failwith ("lookup_field_width_exn: field " ^ field)

and encode_expr_to_int (e : Expression.t) : int =
  let open Expression in
  match snd e with
    | Int (_, i) -> Bigint.to_int_exn i.value
    | _ -> failwith ("encode_express_to_int")

(* Expressions *)
let ctor_name_expression (e : Expression.t) : string =
  let module E = Expression in
  match snd e with
  | E.True -> "True"
  | E.False -> "False"
  | E.Int _ -> "Int"
  | E.String _ -> "String"
  | E.Name _ -> "Name"
  | E.TopLevel _ -> "TopLevel"
  | E.ArrayAccess _ -> "ArrayAccess"
  | E.BitStringAccess _ -> "BitStringAccess"
  | E.List _ -> "List"
  | E.UnaryOp _ -> "UnaryOp"
  | E.BinaryOp _ -> "BinaryOp"
  | E.Cast _ -> "Cast"
  | E.TypeMember _ -> "TypeMember"
  | E.ErrorMember _ -> "ErrorMember"
  | E.ExpressionMember _ -> "ExpressionMember"
  | E.Ternary _ -> "Ternary"
  | E.FunctionCall _ -> "FunctionCall"
  | E.NamelessInstantiation _ -> "NamelessInstantiation"
  | E.Mask _ -> "Mask"
  | E.Range _ -> "Range"

let string_of_binop (e : Op.bin) : string =
  let open Op in
  match snd e with
  | Plus -> "Plus"
  | PlusSat -> "PlusSat"
  | Minus -> "Minus"
  | MinusSat -> "MinusSat"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Shl -> "Shl"
  | Shr -> "Shr"
  | Le -> "Le"
  | Ge -> "Ge"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Eq -> "Eq"
  | NotEq -> "NotEq"
  | BitAnd -> "BitAnd"
  | BitXor -> "BitXor"
  | BitOr -> "BitOr"
  | PlusPlus -> "PlusPlus"
  | And -> "And"
  | Or -> "Or"

let rec dispatch_list ((info,expr) : Expression.t) : P4String.t list =
  let module E = Expression in
  let type_error name =
    failwith ("[TypeError] Tried to call a " ^ name ^ " as a function at " ^ Petr4.Info.to_string info)
  in
  match expr with
  | E.Name s -> [s]
  | E.ExpressionMember {expr; name} -> dispatch_list expr @ [name]
  | E.FunctionCall {func; type_args=[];_} -> dispatch_list func
  | E.ArrayAccess {array; index} -> header_stack_name array index
  | _ ->  ctor_name_expression (info,expr) |> type_error

and string_of_memberlist =
  concatMap ~f:(snd) ~c:(fun x y -> x ^ "." ^ y) 
  
and header_stack_name (name:Expression.t) (size:Expression.t) =
  let open Expression in
  match snd name with
    | ExpressionMember _ ->
      let members = dispatch_list name in
      let i = encode_expr_to_int size in
      members @ [(M "encoding", string_of_int i)]
    | _ -> failwith "header_stack_name"

let dispatch_name e = string_of_memberlist (dispatch_list e)

let validity_bit_no_removal members =
  string_of_memberlist members ^ "_valid"

let validity_bit members =
  validity_bit_no_removal (List.take members (List.length members - 1))

let hit_bit members =
  string_of_memberlist members ^ "_hit"

let return_bit i =
  "return" ^ string_of_int i

let exit_bit = "exit"

let action_run_suffix = "_action_run"

let action_run_field members =
  string_of_memberlist members ^ action_run_suffix

let rec encode_expression_to_value (type_ctx : Declaration.t list) (e : Expression.t) : expr =
  encode_expression_to_value_with_width (-1) type_ctx e

and encode_expression_to_value_with_width width (type_ctx : Declaration.t list) (e : Expression.t) : expr =
  let module E= Expression in
  let unimplemented name =
    failwith ("[Unimplemented Expression->Value Encoding for " ^ name ^"] at " ^ Petr4.Info.to_string (fst e))
  in
  let type_error name =
    failwith ("[TypeError] Expected integer value (or operation), but got " ^ name ^ " at " ^ Petr4.Info.to_string (fst e))
  in
  let binop op e e' =
    let w = get_width type_ctx e in
    let w' = get_width type_ctx e' in
    let fw, fw' = match w, w' with
                    | -1, tw
                    | tw, -1 -> tw, tw
                    | tw, tw' -> tw, tw'
    in
    op (encode_expression_to_value_with_width fw type_ctx e) (encode_expression_to_value_with_width fw' type_ctx e')
  in
  match snd e with
  | E.True -> mkVInt(1, 1)
  | E.False -> mkVInt(0, 1)
  | E.Int (_,i) ->
      begin match i.width_signed with
        | Some (w, _) -> mkVInt(i.value |> Petr4.Bigint.to_int_exn, w)
        | None -> mkVInt(i.value |> Petr4.Bigint.to_int_exn, width)
      end
  | E.Name (_,s) ->
    let w = get_width type_ctx e in
    Var (s, w)
  | E.ExpressionMember _ ->
    let dn = dispatch_name e in
    let w = lookup_field_width_exn type_ctx dn in
    Var (dn, w)
  | E.BinaryOp {op;args=(e, e')} ->
     begin match snd op with
     | Plus -> binop mkPlus e e'
     | Minus -> binop mkMinus e e'
     | Mul -> binop mkTimes e e'
     | Div | Mod
       -> unimplemented (string_of_binop op)
     | Le | Ge | Lt | Gt | Eq | NotEq | And | Or
       -> type_error (string_of_binop op)
     | Shl | Shr | PlusSat | MinusSat | BitAnd | BitXor | BitOr | PlusPlus
       -> unimplemented (string_of_binop op)
     end
  | E.FunctionCall {func; type_args=[]; args=[]} ->
     let members = dispatch_list func in
     begin match List.last members with
     | None -> unimplemented "Function Call with nothing to dispatch"
     | Some (_,"isValid") ->
        Var (validity_bit members, 1)
     | Some _ -> unimplemented ("FunctionCall for members " ^ string_of_memberlist members)
     end
  | _ -> unimplemented (ctor_name_expression e)

and get_width (type_ctx : Declaration.t list) (e : Expression.t) : size =
  let module E= Expression in
  match snd e with
    | E.True -> 1
    | E.False -> 1
    | E.Int (_,i) ->
        begin match i.width_signed with
          | Some (w, _) -> w
          | None -> -1
        end
    | E.Name (_, s) -> lookup_field_width_exn type_ctx s
    | E.ExpressionMember _ ->
      let dn = dispatch_name e in
      lookup_field_width_exn type_ctx dn
    | E.BinaryOp {op;args=(e, e')} ->
      let w1 = get_width type_ctx e in
      let w2 = get_width type_ctx e' in
      if w1 > 0 then w1 else w2
    | _ -> -1

let get_type_decl_from_decl d =
  let open Declaration in
  let open Declaration in
  match d with
    | (i, Constant c) -> Some (i, TypeDef { annotations = c.annotations; name = c.name; typ_or_decl = Left c.typ; })
    | _ -> Some d

let get_type_decls : Declaration.t list -> Declaration.t list =
  let open Declaration in 
  List.filter_map
    ~f:(fun top_decl -> get_type_decl_from_decl top_decl)

let rec encode_expression_to_test (type_ctx : Declaration.t list) (e: Expression.t) : test =
  let module E = Expression in
  let unimplemented (name : string) : test =
    failwith ("[Unimplemented Expression->Value Encoding for " ^ name ^"] at " ^ Petr4.Info.to_string (fst e))
  in
  let type_error (got : string) : test =
    failwith ("[TypeError] Expected boolean expression but got " ^ got ^ " at " ^ Petr4.Info.to_string (fst e))
  in
  match snd e with
  | E.True -> True
  | E.False -> False
  | E.Int _ -> type_error "an integer"
  | E.String (_,s) -> type_error ("a string \"" ^ s ^ "\"")
  | E.Name (_,s) -> Eq(Var(s, 1), mkVInt(1, 1)) (* This must be a boolean value *)
  | E.TopLevel (_,s) -> type_error ("a TopLevel " ^ s)
  | E.ArrayAccess _ -> type_error ("an array access")
  | E.BitStringAccess _ -> type_error ("a bitstring access")
  | E.List _ -> type_error "a list"
  | E.UnaryOp {op; arg} ->
    let open Op in
    let unop x = match snd op with
      | Not -> !%(x)
      | BitNot -> type_error ("a bit-operation")
      | UMinus -> unimplemented ("UMinus")
    in
    unop (encode_expression_to_test type_ctx arg)
  | E.BinaryOp {op; args=(l,r)} ->
    let open Op in
    let l_w = get_width type_ctx l in
    let r_w = get_width type_ctx r in
    let to_value_l = encode_expression_to_value_with_width r_w type_ctx in
    let to_value_r = encode_expression_to_value_with_width l_w type_ctx in
    let to_test = encode_expression_to_test type_ctx in
    begin match snd op with
      | Lt -> to_value_l l %<% to_value_r r
      | Le -> !%(to_value_r r %<% to_value_l l)
      | Gt -> to_value_r r %<% to_value_l l
      | Ge -> !%(to_value_l l %<% to_value_r r)
      | Eq -> to_value_l l %=% to_value_r r
      | NotEq -> to_value_l l %<>% to_value_r r
      (* homomorphic cases *)
      | And -> to_test l %&% to_test r
      | Or -> to_test l %+% to_test r
      (* unimplemented cases *)
      | Plus | PlusSat | Minus
      | MinusSat | Mul | Div | Mod
        -> type_error "an arithmetic expression"
      | Shl | Shr | BitAnd | BitXor | BitOr
        -> type_error "a bit-shift operation"
      | PlusPlus
        -> type_error "an array operation"
    end
  | E.FunctionCall {func; type_args=[]; args=[]} ->
     let members = dispatch_list func in
     begin match List.last members with
     | None -> unimplemented "Function Call with nothing to dispatch"
     | Some (_,"isValid") ->
        Var (validity_bit members, 1) %=% mkVInt(1, 1)
        (* Var (string_of_memberlist members ^ "()", -1) %=% Value(Int (1, 1) ) *)
     | Some _ -> unimplemented ("FunctionCall for members " ^ string_of_memberlist members)
     end
  | E.FunctionCall _ ->
     unimplemented ("FunctionCall with (type?) arguments")
  | E.ExpressionMember {expr;name} ->
    let members = dispatch_list expr in
     begin match name with
     | (_,"hit") ->
        Var (hit_bit members, 1) %=% mkVInt(1, 1)
     | _ -> unimplemented ("ExpressionMember for members " ^ string_of_memberlist members)
     end
  | _ -> unimplemented (ctor_name_expression e)

let lookup_exn (Program(top_decls) : program) (ctx : Declaration.t list) (ident : P4String.t) : Declaration.t =
  let find name =
    let module D = Declaration in 
    List.find ~f:(fun d -> safe_name d = Some (snd name)) in
  match find ident ctx with
  | None -> begin match find ident top_decls with
      | None -> failwith ("[Error: UseBeforeDef] Couldn't find " ^ snd ident ^ " from " ^ Petr4.Info.to_string (fst ident))
      | Some d -> d
    end
  | Some d -> d

let lookup_string (Program(top_decls) : program) (ctx : Declaration.t list) (ident : string) : Declaration.t option =
  let find name =
    let module D = Declaration in 
    List.find ~f:(fun d -> safe_name d = Some name) in
  match find ident ctx with
  | None -> find ident top_decls
  | Some d -> Some d

let lookup_string_exn (prog : program) (ctx : Declaration.t list) (ident : string) : Declaration.t =
  match lookup_string prog ctx ident with
    | Some d -> d
    | None -> failwith ("[Error: UseBeforeDef] Couldnt find " ^ ident)
            
let lookup_action_exn prog ctx action_name =
  let open Declaration in
  match lookup_exn prog ctx action_name with
  | _, Action a -> (a.body, a.params)
  | _ -> failwith ("[TypeError] Expecting \""^ snd action_name ^ "\" to be an action, "
                   ^ "but it resolved to something else at " ^ Petr4.Info.to_string (fst action_name))

let lookup_string_action_exn prog ctx action_name =
  let open Declaration in
  match lookup_string_exn prog ctx action_name with
  | _, Action a -> (a.body, List.map ~f:(fun (_,p) -> Parameter.(p.variable)) a.params)
  | _ -> failwith ("[TypeError] Expecting \""^ action_name ^ "\" to be an action")

let get_return_value =
  let return_val = ref 0 in
  fun () -> return_val := !return_val + 1; !return_val
                     
let rec dispatch prog ctx type_ctx rv members =
  match members with
  | [] -> failwith "[RuntimeException] Tried to Dispatch an empty list of names"
  | [(_,"mark_to_drop")] -> `Motley ("drop" %<-% mkVInt(1,1), false, false)
  | [(_,"clone3")] -> `Motley (Skip, false, false) (* TODO: Anything we can do with this?  Seems out of scope for now... *)

  | _ when Option.map (List.last members) ~f:snd = Some "setInvalid" ->
    `Motley ((validity_bit members) %<-% mkVInt(0,1), false, false)
  | _ when Option.map (List.last members) ~f:snd = Some "setValid" ->
    `Motley ((validity_bit members) %<-% mkVInt(1,1), false, false)


  | [member] ->
    let act, xs = lookup_action_exn prog ctx member in
    let xs' = List.map ~f:(fun (_,p) -> Parameter.(p.variable)) xs in
      `Motley (encode_action3 ~action_data:xs' prog ctx type_ctx rv act)
  | member :: members' ->
    let open Declaration in
    match lookup_exn prog ctx member with
    | (_, Table _ ) as tbl ->
      if List.map ~f:snd members' = ["apply"] then
         `Petr4 tbl
      else
        "[UndefinedMethod] Tied to call methods " ^ string_of_memberlist members ^ "() on a table" |> failwith
    | (_, Instantiation _) as inst -> `Petr4 inst
    | x -> failwith ("unimplemented" ^ Sexp.to_string (sexp_of_t x))

(* Returns a cmd and two bools. These bits correspond to the return and exit bit's respectively.
False means that the command does not flip the bit, True means the cmd may or may not flip the bit. 
If the bit is flipped, we have to either return or exit, and so we must insert an if to ensure this happens. *)
and encode_statement prog (ctx : Declaration.t list) (type_ctx : Declaration.t list) (rv : int) ((info, stmt) : Statement.t) : cmd * bool * bool =
  let open Statement in
  let unimplemented name =
    failwith ("[Unimplemented Statement " ^ name ^"] at " ^ Petr4.Info.to_string info)
  in
  match stmt with
  | MethodCall {func; type_args=_; args } ->
    let dispList = dispatch_list func in 
    (* unimplemented (concatMap dispList ~f:(fun x -> snd x) ~c:(fun x y -> x ^ "." ^ y) ^ "()") *)
    begin match dispatch prog ctx type_ctx rv dispList with
    | `Petr4 (_,Declaration.Table t) ->
      encode_table prog ctx type_ctx rv t.name t.properties, true, true
    | `Petr4 (_,Declaration.Instantiation {typ;_}) -> 
      dispatch_direct_app type_ctx prog typ args
    | `Motley c -> c
    | _ -> failwith "unimplemented, only know how to resolve table dispatches"
    end
  | Assignment {lhs=lhs; rhs=rhs} ->
    begin match encode_expression_to_value type_ctx lhs with
      | Var (f,s) -> f %<-% encode_expression_to_value_with_width s type_ctx rhs, false, false
      | _ -> failwith ("[TypeError] lhs of assignment must be a field, at " ^ Petr4.Info.to_string info)
    end
  | DirectApplication {typ; args} ->
    dispatch_direct_app type_ctx prog typ args
    (* unimplemented ("DirectApplication" ^ Sexp.to_string ([%sexp_of: Statement.pre_t] stmt)) *)
  | Conditional {cond; tru; fls} ->
    let fls_case, fls_rb, fls_eb =
      match fls with
        | None -> [ True, Skip ], false, false
        | Some fls ->
          let stmt, rb1, eb1 = encode_statement prog ctx type_ctx rv fls in
          [ !%(encode_expression_to_test type_ctx cond), stmt ], rb1, eb1
    in
    let expr_to_test = encode_expression_to_test type_ctx cond in
    let tr_stmt, tr_rb, tr_eb = encode_statement prog ctx type_ctx rv tru in
    mkOrdered ([ expr_to_test, tr_stmt] @ fls_case), fls_rb || tr_rb, fls_eb || tr_eb
  | BlockStatement {block} ->
    encode_block3 prog ctx type_ctx rv block
  | Exit ->
    mkAssn exit_bit  (mkVInt(1, 1)), false, true
  | EmptyStatement ->
    Skip, false, false
  | Return {expr} ->
    (match expr with
      | None ->
        mkAssn (return_bit rv) (mkVInt(1, 1)), true, false
      | _ -> unimplemented "Return")
  | Switch {expr; cases} ->
    mkOrdered (encode_switch prog ctx type_ctx expr rv cases), true, true
  | DeclarationStatement _ ->
    unimplemented "DeclarationStatement"

and dispatch_direct_app type_ctx prog ((_, ident) : Type.t) (args : Argument.t list) =
  let open Type in
  match ident with
    | TypeName (_, "counter") -> Skip, false, false (* TODO: out of scope *)
    | TypeName (_, "direct_counter") -> Skip, false, false (* TODO: out of scope *)
    | TypeName (_, "meter") -> Skip, false, false (* TODO: out of scope *)
    | SpecializedType _ -> Skip, false, false (* TODO: out of scope *)
    | TypeName n -> dispatch_control type_ctx prog n args
    | _ -> failwith ("[Unimplemented Type]" ^ (Sexp.to_string ([%sexp_of: Type.pre_t] ident)))

and dispatch_control (type_ctx : Declaration.t list) (Program(top_decls) as prog : program) (ident : P4String.t) (args : Argument.t list) =
  let open Declaration in
  match List.find top_decls ~f:(fun d -> safe_name d = Some (snd ident)) with
  | None -> failwith ("Could not find module " ^ snd ident)
  | Some (_, Control c) ->
    (match List.zip (c.params) args with
      | Ok param_args ->
        let rv = get_return_value () in
        let assign_rv = mkAssn (return_bit rv) (mkVInt(0, 1)) in
        let new_typ_ctx = get_type_decls top_decls @ type_ctx in
        let b = List.concat_map param_args ~f:(assign_param new_typ_ctx) in
        let new_typ_ctx2 = List.map c.params ~f:(update_typ_ctx_from_param new_typ_ctx) @ new_typ_ctx in
        let r = List.concat_map param_args ~f:(return_args new_typ_ctx2) in

        let c, _, eb = encode_control3 prog c.locals new_typ_ctx2 rv c.apply in

        let added_b = List.fold b ~init:c
                      ~f:(fun assgn prog -> prog %:% assgn ) in
        let added_r = List.fold r ~init:(added_b)
                      ~f:(fun assgn prog -> assgn %:% prog ) in
        assign_rv %:% added_r, false, eb
      | Unequal_lengths -> failwith "Parameters and arguments don't match")
  | Some (_, ExternFunction _) -> Skip, false, false
  | Some _ -> failwith ("Found a module called " ^ snd ident ^ ", but it wasn't a control module")


and get_rel_variables (type_ctx : Declaration.t list) (param : Parameter.t) =
  let open Parameter in
  let open Type in
  let open Declaration in
  let type_name t = match t with
                      | (_, TypeName t) -> t
                      | _ -> failwith "type_name: unhandled" in
  let rel_field = List.find type_ctx
                    ~f:(fun (_, d) ->
                      match d with
                        | Struct {name;_} -> snd name = snd (type_name (snd param).typ)
                        | _ -> false
                    )
  in
  match rel_field with
    | Some (_, Struct s) -> s.fields
    | _ -> failwith "get_rel_variables: unhandled"

and assign_param (type_ctx : Declaration.t list) (param_arg : Parameter.t * Argument.t) =
  let open Argument in
  let open Direction in
  let open Expression in
  let open Field in
  let open Parameter in
  let param = snd (fst param_arg) in
  let arg = snd (snd param_arg) in
  let flds = get_rel_variables type_ctx (fst param_arg) in
  match arg with
    | Expression {value} ->
      let val_assgn = List.concat_map flds ~f:(assign_fields type_ctx param value) in
      begin match param.direction with
        | Some (_, In)
        | None -> val_assgn
        | Some (_, Out) -> []
        | Some (_, InOut) ->
          let n = dispatch_list value in
          mkAssn(validity_bit_no_removal [param.variable]) (Var(validity_bit_no_removal n, 1)) :: val_assgn
      end
    | _ -> failwith "Unhandled argument"

and update_typ_ctx_from_param (type_ctx : Declaration.t list) (param : Parameter.t) : Declaration.t =
  let open Parameter in
  let open Declaration in
  let open Type in
  let v = snd (snd param).variable in
  let decl = List.find type_ctx ~f:(fun d -> is_some (safe_name d) && safe_name d = typ_safe_name (snd param).typ) in
  match decl with
  | Some (_, d) -> M v, d
  | None ->
    begin match snd (snd param).typ with
          | BitType _ -> M v, TypeDef { annotations = []; name = M v, v; typ_or_decl = Left ((snd param).typ)}
          | _ -> failwith "update_typ_ctx_from_param"
    end
  (* M v, Header { annotations = [];
                name = M v, v;
                fields = [ M v, { annotations = []; typ = (snd param).typ; name = (snd param).variable } ]
              }
*)

and return_args (type_ctx : Declaration.t list) (param_arg : Parameter.t * Argument.t) =
  let open Direction in
  let open Parameter in
  let open Argument in
  let open Expression in
  let param = snd (fst param_arg) in
  let arg = snd (snd param_arg) in
  match arg with
    | Expression {value} ->
      begin match param.direction with
        | Some (_, In)
        | None -> []
        | Some (_, Out) -> []
        | Some (_, InOut) ->
          let n = dispatch_list value in
          let flds = get_rel_variables type_ctx (fst param_arg) in
          let val_assgn = List.concat_map flds ~f:(assign_fields type_ctx param value) in
          mkAssn (validity_bit_no_removal n) (Var(validity_bit_no_removal [param.variable], 1)) :: val_assgn
      end
    | _ -> failwith "Unhandled argument"

and assign_fields type_ctx param value fld =
  let open Expression in
  let add_to_expr f v = match v with
                              | (i, Name (ni, n)) -> (i, Name (ni, n ^ f))
                              | _ -> failwith "HERE" in
  let flds = get_all_fields type_ctx "" fld in
  List.map flds ~f:(fun f -> mkAssn (snd param.variable ^ f)
                                    (encode_expression_to_value type_ctx (add_to_expr f value)))

and get_all_fields type_ctx h (fld : Declaration.field)  =
  let open Declaration in
  let decl = List.find type_ctx
                    ~f:(fun d -> is_some (safe_name d) && safe_name d = typ_safe_name (snd fld).typ) in
  match decl with
    | Some (_, Header {name;fields})
    | Some (_, Struct {name;fields}) ->
      List.concat_map fields ~f:(get_all_fields type_ctx (h ^ "." ^ snd (snd fld).name))
    | Some (_, TypeDef _)
    | Some (_, Error _) ->  [h ^ "." ^ snd (snd fld).name]
    | _ ->  [h ^ "." ^ snd (snd fld).name]

and encode_control prog (ctx : Declaration.t list) (type_ctx : Declaration.t list) (rv:int) ( body : Block.t ) =
  encode_block prog ctx type_ctx rv body

and encode_control3 prog (ctx : Declaration.t list) (type_ctx : Declaration.t list) (rv:int) ( body : Block.t ) =
  encode_block3 prog ctx type_ctx rv body

and encode_block : program -> Declaration.t list -> Declaration.t list -> int -> Block.t -> cmd =
  encode_action ~action_data:[]

and encode_block3 : program -> Declaration.t list -> Declaration.t list -> int -> Block.t -> cmd * bool * bool =
  encode_action3 ~action_data:[]

and encode_action prog (ctx : Declaration.t list) (type_ctx : Declaration.t list) (rv:int) ( b : Block.t ) ~action_data : cmd =
  fst3 (encode_action3 prog ctx type_ctx rv b ~action_data)

(** Takes a block representing an action and the action_data variables, replacing them with controller holes *)
and encode_action3 prog (ctx : Declaration.t list) (type_ctx : Declaration.t list) (rv:int) ( (_,act) : Block.t ) ~action_data : cmd * bool * bool =
 let open Block in
 let (c, rb, eb) = 
    List.fold act.statements ~init:(Skip, false, false)
      ~f:(fun (rst, in_rb, in_eb) stmt ->
            let tstmt, ret_rb, ret_eb = encode_statement prog ctx type_ctx rv stmt in
            let tstmt2 = if in_rb
                            then mkOrdered [ mkEq (Var(return_bit rv, 1)) (mkVInt(0, 1)), tstmt
                                           ; True , Skip ]
                            else tstmt in
            let tstmt3 = if in_eb
                            then mkOrdered [ mkEq (Var(exit_bit, 1)) (mkVInt(0, 1)), tstmt2
                                           ; True, Skip ]
                            else tstmt in
            (rst %:% tstmt3, ret_rb, ret_eb))
  in
  c, rb, eb

and encode_switch prog (ctx : Declaration.t list) (type_ctx : Declaration.t list) (expr : Expression.t) (rv : int) ( cases : Statement.switch_case list) : (test * cmd) list =
  let e, acs = encode_switch_expr prog ctx type_ctx expr in
  (List.fold_map cases ~f:(encode_switch_case prog ctx type_ctx e acs rv) ~init:True)
  |> snd |> List.filter_map ~f:(fun x -> x)


and encode_switch_expr prog (ctx : Declaration.t list) (type_ctx : Declaration.t list) (e : Expression.t) : expr * Table.action_ref list =
  let dispList = dispatch_list e in
  match dispList with
  | [] -> failwith "[RuntimeException] Tried to encode an empty list of names"
  | member :: _ ->
    let open Declaration in
    match lookup_exn prog ctx member with
    | (_, Table t ) -> let p4actions = List.fold_left t.properties ~init:[]
      ~f:(fun acc_actions prop ->
          match snd prop with
          | Actions {actions} -> acc_actions @ actions
          | _ -> acc_actions) in
      encode_expression_to_value type_ctx e, p4actions
    | _ -> failwith "Table not found"


 and encode_switch_case prog (ctx : Declaration.t list) (type_ctx : Declaration.t list) (expr : expr)
                      (ts : Table.action_ref list) (rv : int) (fall_test : test)
                      (case : Statement.switch_case) : test * ((test * cmd) option) =
    let open Statement in
    match snd case with
      | Action {label;code} ->
        begin match snd label with
          | Default -> failwith "Default in switch"
          | Name lbl_name ->
            let act_i = List.findi ts ~f:(fun _ a -> (snd (snd a).name) = snd lbl_name) in
            let block = encode_block prog ctx type_ctx rv code in
            begin match act_i with
              | Some (i, _) ->
                let test = Or(fall_test, Eq(expr, mkVInt(i + 1, -1))) in
                True, Some (test, block)
              | None -> failwith ("Action not found when encoding switch statement")
            end
        end
      | FallThrough {label} ->
        begin match snd label with
          | Default -> failwith "Default in switch" (* True, None *)
          | Name _ -> failwith "encode_switch_case"
        end

and assign_constants (type_ctx : Declaration.t list) (decl : Declaration.t list) =
  let es = List.filter_map decl
              ~f:(fun t -> match t with
                              | (_, Constant { name = n; typ = t; value = e}) ->
                                  let w = lookup_type_width_exn type_ctx t in
                                  Some(mkAssn (snd n) (encode_expression_to_value_with_width w type_ctx e))
                              | _ -> None ) in
  List.fold es ~f:mkSeq ~init:Skip

and gather_constants (type_ctx : Declaration.t list) (decl : Declaration.t list) =
  let es = List.filter_map decl
              ~f:(fun t -> match t with
                              | (_, Constant { name = n; typ = t; value = e}) ->
                                  let w = lookup_type_width_exn type_ctx t in
                                  Some(snd n, encode_expression_to_value_with_width w type_ctx e)
                              | _ -> None ) in
  es  

and encode_program (Program(top_decls) as prog : program ) =
  let type_cxt = get_type_decls top_decls in
  encode_pipeline type_cxt prog "MyIngress" %:% encode_pipeline type_cxt prog "MyEgress"

and encode_pipeline (type_cxt : Declaration.t list) (Program(top_decls) as prog:program ) (pn : string) =
  let open Declaration in
  match List.find top_decls ~f:(fun d -> match snd d with
                                            | Control{name;_} -> snd name = pn
                                            | _ -> false) with
  | None -> failwith ("Could not find control module " ^ pn)
  | Some (_, Control c) ->
    let rv = get_return_value () in
    let assign_rv = mkAssn (return_bit rv) (mkVInt(1, 1)) in
    let type_cxt2 = List.map c.params ~f:(update_typ_ctx_from_param type_cxt) @ type_cxt in
    (* let _ = printf "type_cxt\n%s\n" (Sexp.to_string ([%sexp_of: Declaration.t list] type_cxt)) in *)
    let assign_consts = assign_constants type_cxt2 top_decls in
    let consts = gather_constants type_cxt2 top_decls in
    replace_consts consts (assign_rv %:% encode_control prog c.locals type_cxt2 rv c.apply)
  | Some _ -> failwith "Found a module called MyIngress, but it wasn't a control module"
 
and encode_table prog (ctx : Declaration.t list) (type_ctx : Declaration.t list) (rv : int) (name : P4String.t) (props : Table.property list) : cmd =
  let open Table in
  let p4keys, p4actions, p4customs = List.fold_left props ~init:([], [], [])
      ~f:(fun (acc_keys, acc_actions, acc_customs) prop ->
          match snd prop with
          | Key {keys} -> (acc_keys @ keys, acc_actions, acc_customs)
          | Actions {actions} -> (acc_keys, acc_actions @ actions, acc_customs)
          | Custom {name;value;_} -> (acc_keys, acc_actions, (name, value) :: acc_customs)
          | _ -> (acc_keys, acc_actions, acc_customs)
         ) in
  let str_keys = List.map p4keys ~f:(fun k -> let kn = dispatch_name (snd k).key in
                                              (kn, lookup_field_width_exn type_ctx kn)) in
  
  let lookup_and_encode_action i (_,a) =
    let (body, ad) = lookup_action_exn prog ctx a.name in
    let action_data = List.map ad ~f:(fun (_,p) -> Parameter.(p.variable)) in
    (* Set up an action run variable so we can use it to figure out which action ran in switch statements *)
    let set_action_run = mkAssn (snd name ^ action_run_suffix) (mkVInt(i + 1, 1)) in
    let add_tctx = List.map ad ~f:(update_typ_ctx_from_param type_ctx) in 
    let type_ctx2 = add_tctx @ type_ctx in
    List.map action_data ~f:(fun (_, ad) -> ad, -1), set_action_run %:% encode_action prog ctx type_ctx2 rv body ~action_data
  in
  let action_cmds = List.mapi p4actions ~f:lookup_and_encode_action in

  let def_act = List.find_map p4customs
        ~f:(fun (n, e) -> if snd n = "default_action" then Some e else None) in
  let enc_def_act = match def_act with
                      | Some da ->
                        let da_name = string_of_memberlist (dispatch_list da) in
                        let (def_act_body, ad) = lookup_action_exn prog ctx (M "", da_name) in
                        let action_data = List.map ad ~f:(fun (_,p) -> Parameter.(p.variable)) in
                        let add_tctx = List.map ad ~f:(update_typ_ctx_from_param type_ctx) in
                        let type_ctx2 = add_tctx @ type_ctx in
                        encode_action prog ctx type_ctx2 rv def_act_body ~action_data
                      | None -> Skip
  in

  let init_action_run = mkAssn (snd name ^ action_run_suffix) (mkVInt(0, 1)) in

  let xxx_keys = printf "keys = %s\n" (Sexp.to_string ([%sexp_of: (string * int) list] str_keys)) in
  init_action_run %:% Apply { name = snd name; keys = str_keys; actions =  action_cmds; default =  enc_def_act }

and replace_consts (consts : (string * expr)  list) (prog : cmd) =
  match prog with
  | Skip -> Skip
  | Assign(v, e) -> Assign(v, replace_consts_expr consts e)
  | Assert t -> Assert(replace_consts_test consts t)
  | Assume t -> Assume(replace_consts_test consts t)
  | Seq(c1, c2) -> Seq(replace_consts consts c1, replace_consts consts c2)
  | While(t, c) -> While(replace_consts_test consts t, replace_consts consts c)
  | Select(st, tc) ->
    Select(st,  List.map tc ~f:(fun (t, c) -> replace_consts_test consts t, replace_consts consts c))
  | Apply{name; keys; actions; default} ->
    let actions' = List.map actions ~f:(fun (s, c) -> (s, replace_consts consts c)) in
    let default' = replace_consts consts default in
    Apply{name; keys; actions = actions'; default = default'}

and replace_consts_expr (consts : (string * expr)  list) (e : expr) =
  match e with
  | Value v -> Value v
  | Var(v, w) ->
    begin match List.find consts ~f:(fun (n, _) -> n = v) with
    | Some (_, e) -> e
    | None -> Var(v, w)
    end
  | Hole h -> Hole h
  | Plus(e1, e2) -> Plus(replace_consts_expr consts e1, replace_consts_expr consts e2)
  | Times(e1, e2) -> Times(replace_consts_expr consts e1, replace_consts_expr consts e2)
  | Minus(e1, e2) -> Minus(replace_consts_expr consts e1, replace_consts_expr consts e2)
  | Mask(e1, e2) -> Mask(replace_consts_expr consts e1, replace_consts_expr consts e2)

and replace_consts_test (consts : (string * expr) list) (t : test) =
  match t with
  | True -> True
  | False -> False
  | Eq(e1, e2) -> Eq(replace_consts_expr consts e1, replace_consts_expr consts e2)
  | Le(e1, e2) -> Le(replace_consts_expr consts e1, replace_consts_expr consts e2)
  | And(t1, t2) -> And(replace_consts_test consts t1, replace_consts_test consts t2)
  | Or(t1, t2) -> Or(replace_consts_test consts t1, replace_consts_test consts t2)
  | Impl(t1, t2) -> Impl(replace_consts_test consts t1, replace_consts_test consts t2)
  | Iff(t1, t2) -> Iff(replace_consts_test consts t1, replace_consts_test consts t2)
  | Neg t1 -> Neg (replace_consts_test consts t1)

let read_lines filename =
  let chan = In_channel.create filename in
  Std.input_list chan

let apply_model_from_file (c : cmd) (model_file : string) : cmd =
  let lines = read_lines model_file in
  let parse_line l =
    let words = String.split_on_chars l ~on:[' '] in
    let nonempty_words = List.filter ~f:(fun f -> String.length f > 0) words in
    match nonempty_words  with
    | [] -> None
    | [_] -> None
    | [x;y] -> Some (x, int_of_string y)
    | _ -> None
  in
  List.fold lines ~init:c
    ~f:(fun c line ->
      match parse_line line with
      | None -> c
      | Some (hole, data) ->
         StringMap.singleton hole (mkInt (data, -1))
         |> fill_holes c)

(* P4-PARSING *)
let colorize colors s = ANSITerminal.sprintf colors "%s" s
let red s = colorize [ANSITerminal.red] s
let green s = colorize [ANSITerminal.green] s

let preprocess include_dirs p4file =
  let cmd =
    String.concat ~sep:" "
      (["cc"] @
       (List.map include_dirs ~f:(Printf.sprintf "-I%s") @
       ["-undef"; "-nostdinc"; "-E"; "-x"; "c"; p4file])) in
  let in_chan = Unix.open_process_in cmd in
  let str = In_channel.input_all in_chan in
  (*let _ : Core.Unix.Exit_or_signal.t = try Unix.close_process_in in_chan with _ -> failwith "bad close" in*)
  str

let parse_p4 include_dirs p4_file verbose =
  let () = Lexer.reset () in
  let () = Lexer.set_filename p4_file in
  let p4_string = preprocess include_dirs p4_file in
  let lexbuf = Lexing.from_string p4_string in
  try
    let prog = Petr4.Parser.p4program Lexer.lexer lexbuf in
    if verbose then Format.eprintf "[%s] %s@\n%!" (green "Passed") p4_file;
    `Ok prog
  with
  | err ->
    if verbose then Format.eprintf "[%s] %s@\n%!" (red "Failed") p4_file;
    `Error (Lexer.info lexbuf, err)


let encode_from_p4 include_dirs p4_file verbose : Ast.cmd =
  Format.printf "encoding file %s\n%!" p4_file;
  match parse_p4 include_dirs p4_file verbose with
  | `Error (info, _) ->
     raise (Failure (Format.sprintf "at %s @\n%!" (Info.to_string info)))
  | `Ok p4_program ->
     let cmd = encode_program p4_program in
    Format.printf "Encoded Program: \n%!\n %s%! \n%!" (string_of_cmd cmd);
    cmd
