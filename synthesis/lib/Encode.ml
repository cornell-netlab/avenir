open Core
open Util
open Petr4
open Types
open Ast
open Manip

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
    failwith ("[TypeError] Tried to call a " ^ name ^ "as a function at " ^ Petr4.Info.to_string info)
  in
  match expr with
  | E.Name s -> [s]
  | E.ExpressionMember {expr; name} -> dispatch_list expr @ [name]
  | _ ->  ctor_name_expression (info,expr) |> type_error

let string_of_memberlist =
  concatMap ~f:(snd) ~c:(fun x y -> x ^ "." ^ y) 
   
let rec encode_expression_to_value (e : Expression.t) : expr =
  let module E= Expression in
  let unimplemented name =
    failwith ("[Unimplemented Expression->Value Encoding for " ^ name ^"] at " ^ Petr4.Info.to_string (fst e))
  in
  let type_error name =
    failwith ("[TypeError] Expected integer value (or operation), but got " ^ name ^ " at " ^ Petr4.Info.to_string (fst e))
  in
  let binop op e e' =
    op (encode_expression_to_value e) (encode_expression_to_value e')
  in
  match snd e with
  | E.True -> type_error "True"
  | E.False -> type_error "False"
  | E.Int (_,i) -> Value (Int (Bigint.to_int_exn i.value, -1))
  | E.Name (_,s) -> Var (s,-1)
  | E.ExpressionMember _ -> Var (dispatch_list e |> string_of_memberlist, -1)
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
  | _ -> unimplemented (ctor_name_expression e)

let get_decls : TopDeclaration.t list -> Declaration.t list =
  let open TopDeclaration in 
  List.filter_map
    ~f:(fun top_decl -> match top_decl with
        | TypeDeclaration _ -> None
        | Declaration d -> Some d
      )

let rec encode_expression_to_test (e: Expression.t) : test =
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
  | E.Name (_,s) -> type_error ("a name "^ s)
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
    unop (encode_expression_to_test arg)
  | E.BinaryOp {op; args=(l,r)} ->
    let open Op in
    let to_value = encode_expression_to_value in
    let to_test = encode_expression_to_test in
    begin match snd op with
      | Lt -> to_value l %<% to_value r
      | Le -> !%(to_value r %<% to_value l)
      | Gt -> to_value r %<% to_value l
      | Ge -> !%(to_value l %<% to_value r)
      | Eq -> to_value l %=% to_value r
      | NotEq -> to_value l %<>% to_value r
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
        Var (string_of_memberlist members ^ "()", -1) %=% Value(Int (1, 1) )
     | Some _ -> unimplemented ("FunctionCall for members " ^ string_of_memberlist members)
     end
  | E.FunctionCall _ ->
     unimplemented ("FunctionCall with (type?) arguments")
  | _ -> unimplemented (ctor_name_expression e)


let lookup_exn (Program(top_decls) : program) (ctx : Declaration.t list) (ident : P4String.t) : Declaration.t =
  let find name =
    let module D = Declaration in 
    List.find ~f:(fun d -> snd (D.name d) = snd name) in
  match find ident ctx with
  | None -> begin match find ident (get_decls top_decls) with
      | None -> failwith ("[Error: UseBeforeDef] Couldnt find " ^ snd ident)
      | Some d -> d
    end
  | Some d -> d

            
let lookup_action_exn prog ctx action_name =
  let open Declaration in
  match lookup_exn prog ctx action_name with
  | _, Action a -> (a.body, List.map ~f:(fun (_,p) -> Parameter.(p.variable)) a.params)
  | _ -> failwith ("[TypeError] Expecting \""^ snd action_name ^ "\" to be an action, "
                   ^ "but it resolved to something else at " ^ Petr4.Info.to_string (fst action_name))
          

let dispatch prog ctx members =
  match members with
  | [] -> failwith "[RuntimeException] Tried to Dispatch an empty list of names"
  | [(_,"mark_to_drop")] -> `Motley ("drop" %<-% mkVInt(1,1))
  | [member] ->
    failwith ("[Unimplemented] Tried to apply " ^ snd member ^ " as a function, but i'm not sure what it is")
  | member :: members' ->
    let open Declaration in 
    match lookup_exn prog ctx member with
    | (_, Table _ ) as tbl ->
      if List.map ~f:snd members' = ["apply"] then
        `Petr4 tbl
      else
        "[UndefinedMethod] Tied to call methods " ^ string_of_memberlist members ^ "() on a table" |> failwith
    | _ -> failwith "unimplemented"

let rec encode_statement prog (ctx : Declaration.t list) ((info, stmt) : Statement.t) : cmd =
  let open Statement in
  let unimplemented name =
    failwith ("[Unimplemented Statement " ^ name ^"] at " ^ Petr4.Info.to_string info)
  in
  match stmt with
  | MethodCall {func; type_args=_; args=_ } ->
    let dispList = dispatch_list func in
    (* unimplemented (concatMap dispList ~f:(fun x -> snd x) ~c:(fun x y -> x ^ "." ^ y) ^ "()") *)
    begin match dispatch prog ctx dispList with
    | `Petr4 (_,Declaration.Table t) -> encode_table prog ctx t.properties
    | `Motley c -> c
    | _ -> failwith "unimplemented, only know how to resolve table dispatches"
    end
  | Assignment {lhs=lhs; rhs=rhs} ->
    begin match encode_expression_to_value lhs with
      | Var (f,_) -> f %<-% encode_expression_to_value rhs
      | _ -> failwith ("[TypeError] lhs of assignment must be a field, at " ^ Petr4.Info.to_string info)
    end
  | DirectApplication _ ->
    unimplemented "DirectApplication"
  | Conditional {cond; tru; fls} ->
    let fls_case = match fls with
      | None -> []
      | Some fls -> [ !%(encode_expression_to_test cond), encode_statement prog ctx fls ]
    in
    [ encode_expression_to_test cond, encode_statement prog ctx tru] @ fls_case
    |> mkSelect Partial
  | BlockStatement {block} ->
    encode_block prog ctx block
  | Exit ->
    Assert False (* TODO :: is this right? *)
  | EmptyStatement ->
    Skip
  | Return _ ->
    unimplemented "Return"
  | Switch _ ->
    unimplemented "Switch"
  | DeclarationStatement _ ->
    unimplemented "DeclarationStatement"

and encode_block : program -> Declaration.t list ->  Block.t -> cmd =
  encode_action ~action_data:[]
    
(** Takes a block representing an action and the action_data variables, replacing them with controller holes *)
and encode_action prog (ctx : Declaration.t list) ( (_,act) : Block.t ) ~action_data : cmd =
 let open Block in
  List.fold act.statements ~init:Skip
    ~f:(fun rst stmt -> encode_statement prog ctx stmt %:% rst )
  |> holify (List.map ~f:snd action_data)

and encode_control prog (ctx : Declaration.t list) ( body : Block.t ) =
  encode_block prog ctx body

and encode_program (Program(top_decls) as prog : program ) =
  let open Declaration in
  match List.find (get_decls top_decls) ~f:(fun d -> snd (Declaration.name d) = "MyIngress") with
  | None -> failwith "Could not find control module MyIngress"
  | Some (_, Control c) -> encode_control prog c.locals c.apply
  | Some _ -> failwith "Found a module called MyIngress, but it wasn't a control module"
  
  
and encode_match ((_, m) : Table.key) : test =
  match m.match_kind with
  | _, "exact" -> encode_expression_to_value m.key %=% Hole ("?",-1)
  | info, x -> failwith ("[Unimplemented MatchKind " ^ x ^ "] Cannot handle match kind "
                   ^ x ^ " at " ^ Petr4.Info.to_string info )

and encode_table prog (ctx : Declaration.t list) (props : Table.property list) : cmd =
  let open Table in
  let p4keys, p4actions = List.fold_left props ~init:([],[])
      ~f:(fun (acc_keys, acc_actions) prop ->
          match snd prop with
          | Key {keys} -> (acc_keys @ keys, acc_actions)
          | Actions {actions} -> (acc_keys, acc_actions @ actions)
          | _ -> (acc_keys, acc_actions)
         ) in
  let lookup_and_encode_action (_,a) =
    let (body, action_data) = lookup_action_exn prog ctx a.name in
    encode_action prog ctx body ~action_data
  in
  let action_cmds = List.map p4actions ~f:lookup_and_encode_action in
  let key_test = concatMap p4keys ~f:(encode_match) ~c:(%&%) ~init:(Some True)in
  (List.map action_cmds ~f:(fun act -> key_test, act) ) @ [(True, Skip)]
  |> mkSelect Ordered

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
         StringMap.singleton hole (Int (data, -1))
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
  let _ = Unix.close_process_in in_chan in
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
      
                 
