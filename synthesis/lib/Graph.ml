open Core
open Ast
open Manip

module Int = struct
  type t = int
  let compare = compare
  let sexp_of_t = sexp_of_int
  let t_of_sexp = int_of_sexp
end
   
module IntMap = Map.Make (Int)

type graph = (((test * expr) list) IntMap.t) IntMap.t
type path = int list

let string_of_graph (g : graph) =
	let buf = Buffer.create 200 in
  IntMap.iter ~f:(fun n ->
		IntMap.iter ~f:(fun li ->
			List.iter ~f:(fun (t,e) -> Printf.bprintf buf "%s \n" ("(" ^ (string_of_test t) ^ "," ^ (string_of_expr e) ^ ")") ) li ) n ) g;
	Buffer.contents buf;;
									                  

let (%.) f g x = f (g x)
             
let rec split_test_on_loc test =
  match test with
  | Or _ -> failwith ("malformed test, || not allowed in if statement, please make a separate case (" ^ (string_of_test test) ^")")
  | True -> (None, True)
  | False -> (None, False)
  | Eq (v, v') ->
     begin match v, v' with
     | Var "loc", Int l | Int l , Var "loc" -> (Some l, True)
     | _, _ -> (None, mkEq v v')
     end
  | Neg ((Eq (_, _)) as t) -> (None, t)
  | Neg _ -> failwith "malformed test, must be in NNF"
  | And (a, b) ->
     let loc_opt_a, test_a = split_test_on_loc a in
     let loc_opt_b, test_b = split_test_on_loc b in
     let test = mkAnd test_a test_b in
     match loc_opt_a, loc_opt_b with
     | None, None -> (None, test)
     | None, loc_opt | loc_opt, None -> (loc_opt, test)
     | Some l, Some l' ->
        if l = l' then (Some l , test)
        else failwith "malformed test, cannot have multiple locs in if statement"

let rec split_expr_on_loc (expr:expr) : (expr * int option) =
  match expr with
  | While _ -> failwith "cannot handle while nested under select"
  | SelectFrom _ -> failwith "Cannot handle nested selects"
  | Assign ("loc", Int l) -> (Skip, Some l)
  | Assign _
    | Skip
    | Assert _ -> (expr, None)
  | Seq (p, q) ->
     let (expr_p, loc_p) = split_expr_on_loc p in
     let (expr_q, loc_q) = split_expr_on_loc q in
     let expr = mkSeq expr_p expr_q in
     match loc_p, loc_q with
     | None, None -> (expr, None)
     | None,  l_opt | l_opt, None -> (expr, l_opt)
     | Some _, Some lq ->
        (* Take the latter update *)
        (expr, Some lq)


let normalize_selects (ss : (test * expr) list) : (test * expr) list =
  List.fold ss ~init:[] ~f:(fun ss' (test, expr) ->
      List.fold (dnf test) ~init:ss' ~f:(fun ss' test ->
          (test, expr) :: ss'
        )
    )

       
let rec get_selects (e : Ast.expr) =
  match e with
  | Skip | Assign _ | Assert _  -> []
  | While (_, body) -> get_selects body
  | Seq (firstdo, thendo) -> get_selects firstdo @ get_selects thendo
  | SelectFrom ss ->
     List.map ss ~f:(fun (test, act) ->
         let loc, test = split_test_on_loc test in
         let act, loc' = split_expr_on_loc act in
         (loc, test, act, loc')
       )
              

let add_edge graph (src,test,act,dst) =
  IntMap.update graph src
    ~f:(fun x -> IntMap.update (Option.value ~default:IntMap.empty x) dst
                   ~f:(fun y -> (test, act) :: Option.value ~default:[] y))
  
let make_graph (e : Ast.expr) : graph =
  let selects = get_selects e in
  List.fold selects ~init:IntMap.empty
    ~f:(fun g (src_loc_opt, test, action, dst_loc_opt) ->
      match src_loc_opt, dst_loc_opt with
      | Some src_loc, Some dst_loc ->
         add_edge g (src_loc, test, action, dst_loc)
      | _, _ -> failwith "Could not find the location for a select statement"
    )



let get_neighbors (graph:graph) location =
  match IntMap.find graph location with
  | None -> IntMap.empty
  | Some nbrs -> nbrs
  
  
let rec get_all_paths_between (graph:graph) (rev_path:path) (current:int) (final:int) : path list =
  if current = final then (* reached destination *)
    [current :: rev_path]
  else if List.mem rev_path current ~equal:(=) then (* Found a loop *)
    []
  else
    let nbrs = get_neighbors graph current |> IntMap.keys in
    List.fold nbrs ~init:[] ~f:(fun paths nbr ->
        (get_all_paths_between graph (current :: rev_path) nbr final)
        @ paths
      )



    
let all_locations graph : int list =
  let init = IntMap.keys graph in
  let f acc current = 
    let nbrs = get_neighbors graph current |> IntMap.keys in
    acc @ nbrs
    |> List.dedup_and_sort ~compare
  in
  List.fold init ~init ~f
  
  
let get_all_paths ?ingress:(ingress=None) ?egress:(egress=None) graph : path list =
  let nodes = all_locations graph in
  let loop = List.fold nodes ~init:[] in
  let skip x_opt y : bool = match x_opt with | None -> false | Some x -> x = y in
  loop ~f:(fun paths ing ->
      if skip ingress ing then
        paths
      else 
        loop ~f:(fun paths' egr ->
            if skip egress egr then
              paths
            else
              get_all_paths_between graph [] ing egr
              @ paths'
          )
        @ paths
    )

let get_edges (graph:graph) src dst =
  let succs = IntMap.find_exn graph src in
  let edges = IntMap.find_exn succs dst in
  edges

let rec get_program_of_rev_path graph rev_path : expr =
  match rev_path with
  | [] -> Skip
  | [final] -> mkAssn "loc" (Int final)
  | after :: before :: rest -> (* path is reversed so packet traveling from before -> ater  *)
     let edges = get_edges graph before after |> SelectFrom in
     get_program_of_rev_path graph rest %:% edges     

     
