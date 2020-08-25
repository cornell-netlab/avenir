open Core
open Ast
open Synthesis
open Util
open Packet
open Tables
open Classbenching
module IntMap = Map.Make(Int)

let parse_file (filename : string) : Ast.cmd =
  let cts = In_channel.read_all filename in
  let lexbuf = Lexing.from_string cts in
  Parser.main Lexer.tokens lexbuf

let parse_fvs fp =
  In_channel.read_lines fp
  |> List.map ~f:(fun line ->
         match String.lsplit2 line ~on:',' with
         | None -> Printf.sprintf "Malformed FV line %s" line
                   |> failwith
         | Some (abs, phys_cont) ->
            match String.lsplit2 phys_cont ~on:',' with
            | None -> Printf.sprintf "Malformed FV line %s" line
                      |> failwith
            | Some (phys, sz_str) ->
               let sz = try int_of_string sz_str
                        with _ -> failwith @@ Printf.sprintf "Couldn't parse int from %s" sz_str
               in
               ((abs, sz), (phys, sz))
       )

let rec run_experiment iter seq (phys_seq : Edit.t list) (params : Parameters.t) hints (problem : Problem.t) =
  match seq with
  | [] -> Some phys_seq
  | edit::edits ->
     (* Printf.printf "==== BENCHMARKING INSERTION OF (%s) =====\n%!"
      *   (string_of_edit edit); *)
     let data = ProfData.zero () in
     let problem_inner = Problem.(replace_log_edits problem edit) in
     let st = Time.now () in
     assert (List.length (Problem.phys_edits problem_inner) = 0);
     match cegis_math params data problem_inner with
     | None -> None
        (* (\* let _ : Edit.t list option = cegis_math {params with debug = true} data problem_inner in *\)
         * failwith "example failed" *)
     | Some pedits ->
        !data.time := Time.(diff (now()) st);
        !data.log_inst_size :=  Problem.log_inst problem |> Instance.size ;
        !data.phys_inst_size := Problem.phys_inst problem |> Instance.size;
        Printf.printf "%s\n%!" (ProfData.to_string !data);
        (* if iter > 531 then None else *)
        run_experiment (iter + 1)
          edits
          (phys_seq @ pedits)
          (if false then {params with debug = true} else params )
          hints
          Problem.(problem
                   |> flip (apply_edits_to_log params) edit
                   |> flip (apply_edits_to_phys params) pedits
                   |> delete_phys_edits)
                        
let measure params hints problem insertions =
  Printf.printf "%s\n%!" ProfData.header_string;
  run_experiment 0 insertions [] params hints problem

                                            
let permute l =
  List.map l ~f:(inj_r (Random.int (List.length l)))
  |> List.sort ~compare:(fun (i, _) (j,_) -> compare i j)
  |> List.map ~f:(snd) 
              
let tbl = Printf.sprintf "tbl%d"
    
let rec mk_pipeline varsize =
  fun n ->
  if n = 0 then [] else
    (tbl n
    , [("k_" ^tbl n, varsize)]
    , [([Printf.sprintf "v%d" n,varsize],("x_"^tbl n) %<-% Var(Printf.sprintf "v%d" n,varsize))]
    , ("x_"^tbl n) %<-% mkVInt (0,varsize)
    ) :: mk_pipeline varsize (n-1)


let rec generate_n_insertions varsize length n avail_tables maxes : Edit.t list =
  if n = 0 then
    let _ : unit = Printf.printf "--generated--\n%!"in
    []
  else if avail_tables = [] then
    let _ : unit = Printf.printf "--filled up --\n%!" in
    []
  else
    let _ : unit = Printf.printf "generating %d \n%!" n in
    let rec loop_free_match avail_tables =
      if avail_tables = []
      then None
      else
        let i = Random.int (List.length avail_tables) |> List.nth_exn avail_tables in
        let max_i = StringMap.find maxes (tbl i) |> Option.value ~default:0 in
        Printf.printf "%s max : %d\n%!" (tbl i) max_i;
        if max_i >= pow 2 varsize
        then loop_free_match (List.filter avail_tables ~f:((<>) i))
        else
          let (max', mtch) =
            if Random.int 6 < 1 then
              (max_i + 1, Match.Exact (mkInt(max_i, varsize)))
            else
              let lo = max_i in
              let hi = min (lo + Random.int 3) (pow 2 varsize - 1) in
              if lo = hi then
                (hi + 1, Match.Exact (mkInt(hi, varsize)))
              else
                (hi + 1, Match.Between (mkInt(lo, varsize), mkInt(hi, varsize)))
          in
          let maxes' = StringMap.set maxes ~key:(tbl i) ~data:max' in
          let act_data = mkInt(Random.int (pow 2 varsize),varsize) in
          let row = ([mtch], [act_data], 0) in
          Some (maxes', avail_tables, tbl i, row)
    in
    match loop_free_match avail_tables with
    | None ->
       let _ : unit = Printf.printf "--filled up --\n%!" in
       []
    | Some (maxes', avail_tables', name, row) ->
       let _ : unit = Printf.printf "Inserting\n%!" in
       Add (name, row)
       :: generate_n_insertions varsize length (n-1) avail_tables' maxes'
                                  
let reorder_benchmark varsize length max_inserts params =
  Random.init 99;
  let logical_pipeline = mk_pipeline varsize length in
  let physical_pipeline = permute logical_pipeline in
  let mk_empty_inst acc (name,_,_,_) = Map.set acc ~key:name ~data:[]  in
  let log_inst = List.fold logical_pipeline ~init:StringMap.empty ~f:mk_empty_inst in
  let phys_inst = List.fold logical_pipeline ~init:StringMap.empty ~f:mk_empty_inst in
  let to_cmd line =  List.((line >>| fun t -> mkApply t)
                           |> reduce_exn ~f:(%:%)) in
  let log = to_cmd logical_pipeline in
  let phys = to_cmd physical_pipeline in
  let insertion_sequence =
    generate_n_insertions varsize length max_inserts (range_ex 1 (length +1)) StringMap.empty
    |> List.(map ~f:return)
  in
  let fvs = range_ex 1 (length + 1)
            |> List.map ~f:(fun i ->
                   [("k_" ^tbl i, varsize)
                   ; ("x_" ^tbl i, varsize)
                       (* ; ("?ActIn"^tbl i, 8) *)
                 ])
            |> List.join
  in
  let problem = Problem.make ~log ~phys ~fvs ~log_inst ~phys_inst ~log_edits:[] () in
  measure params (Some (List.return)) problem insertion_sequence


  
            
(** ONF BENCHMARK **)
    
let onos_to_edits filename tbl_nm =
  let lines = In_channel.read_lines filename in
  let make_edit data : Edit.t =
    match data with
    | [_; "ADD"; _; ipv6; id] ->
       Add (tbl_nm, ([Match.mk_ipv6_match ipv6], [Int(Bigint.of_string id, 32)], 0))
    | [_; "REMOVE"; _; _; _] ->
       failwith "cannot yet handle removes"
    | _ ->
       Printf.sprintf "Unrecognized row: %s\n%!"
         (List.intersperse data ~sep:"---" |> List.reduce_exn ~f:(^))
       |> failwith
  in
  let edits =
    List.map lines ~f:(fun line ->
        [String.split line ~on:','
         |> make_edit]
      ) in
  edits    


let (%>) c c' = ignore c; c'


let basic_onf_ipv4 params filename = 
  let log =
    sequence [
        "class_id" %<-% mkVInt(0,32)
      ; mkApply("ipv6",
              [("ipv6_dst", 128)],
              [([("next_id",32)], "class_id"%<-% Var("next_id",32))],
              Skip)
      ;
        mkOrdered [
            Var("class_id",32) %=% mkVInt(1017,32), "out_port" %<-% mkVInt(17,9) ;
            Var("class_id",32) %=% mkVInt(1018,32), "out_port" %<-% mkVInt(18,9) ;
            Var("class_id",32) %=% mkVInt(1010,32), "out_port" %<-% mkVInt(10,9) ;
            Var("class_id",32) %=% mkVInt(1012,32), "out_port" %<-% mkVInt(12,9) ;
            Var("class_id",32) %=% mkVInt(1011,32), "out_port" %<-% mkVInt(11,9) ;
            Var("class_id",32) %=% mkVInt(1008,32), "out_port" %<-% mkVInt(08,9) ;
            Var("class_id",32) %=% mkVInt(1003,32), "out_port" %<-% mkVInt(03,9) ;
            Var("class_id",32) %=% mkVInt(1019,32), "out_port" %<-% mkVInt(19,9) ;
            True, "out_port" %<-% mkVInt(0,9)
          ]
      ] in
  let phys =
    mkApply("l3_fwd"
        , [ ("ipv6_dst", 128) (*; ("ipv4_src", 32); ("ipv4_proto", 16)*) ]
        , [ ([("port",9)], "out_port"%<-% Var("port",9))]
        , "out_port" %<-% mkVInt(0,9)) in
  let fvs = [("ipv6_dst", 128); ("out_port", 9); (*("ipv4_src", 32); ("ipv4_proto", 16)*)] in
  (* synthesize_edit  ~gas ~iter ~fvs p 
   *   logical
   *   physical
   *   StringMap.empty
   *   StringMap.empty
   *   ("next", ([Exact (1,32)], [(1,9)],0))
   * %> *)
  let problem =
    Problem.make ~log ~phys ~fvs
      ~log_inst:StringMap.(set empty ~key:"ipv6" ~data:[])
      ~log_edits:[]
      ~phys_inst:StringMap.(set empty ~key:"l3_fwd" ~data:[]) ()
  in
  measure params None problem (onos_to_edits filename "ipv6")

let rec basic_onf_ipv4_real params data_file log_p4 phys_p4 log_edits_file phys_edits_file fvs_file assume_file log_inc phys_inc =
  let var_mapping = parse_fvs fvs_file in
  let fvs = List.map var_mapping ~f:snd in
  let assume = parse_file assume_file in

  (* let print_fvs = printf "fvs = %s" (Sexp.to_string ([%sexp_of: (string * int) list] fvs)) in *)

  let log = (assume %:% Encode.encode_from_p4 log_inc log_p4 false)
            |> Encode.unify_names var_mapping |> zero_init fvs |> drop_handle fvs in

  let phys = (assume %:% Encode.encode_from_p4 phys_inc phys_p4 false)
             |> Encode.unify_names var_mapping |> zero_init fvs |> drop_handle fvs
             |> CompilerOpts.optimize fvs
  in

  (* let maxN n = Bigint.(of_int_exn n ** of_int_exn 2 - one) in *)
  (* let fvs = parse_fvs fvs in *)
  let log_edits = Runtime.parse log_edits_file in
  let phys_edits = Runtime.parse phys_edits_file in

  let problem =
    Problem.make
      ~log  ~phys ~fvs
      ~log_inst:Instance.(update_list params empty log_edits)
      ~phys_inst:Instance.(update_list params empty phys_edits)
      ~log_edits:[] ()
  in
  measure params None problem (log_edits :: onos_to_edits data_file "routing_v6")

and zero_init fvs cmd =
  let vs = variables cmd |> List.dedup_and_sort ~compare:(fun (v1, _) (v2, _) -> String.compare v1 v2) in
  let zi = List.filter vs ~f:(fun (v,_) -> List.(mem (fvs >>| fst) v ~equal:(=) |> not)) in
  sequence @@
    List.map zi ~f:(fun (v, w) ->  mkAssn v  (mkVInt(0, w)))
    @ [cmd]


and drop_handle fvs cmd =
  let c_end = List.map fvs ~f:(fun (v, w) -> mkAssn v (mkVInt(0, w)))
              |> List.fold ~init:Skip ~f:(fun c1 c2 -> c1 %:% c2)
  in
  cmd %:% mkOrdered [ Var("standard_metadata.egress_spec", 9) %=% mkVInt(0, 9), c_end;
                      True, Skip]

and variables cmd =
  match cmd with
  | Skip -> []
  | Assign(s, e) -> (s, get_width e) :: variables_expr e
  | Seq (c1, c2) -> variables c1 @ variables c2
  | While(_, c1) -> variables c1
  | Select(_, tc) -> List.concat_map tc ~f:(fun (t, c) -> variables_test t @  variables c)
  | Apply {keys;_} -> keys
  | _ -> []

and variables_expr e =
  match e with
  | Value _ -> []
  | Var(n, w) -> [(n, w)]
  | Hole _ -> []
  | Cast (_,e) | Slice {bits=e;_} -> variables_expr e
  | Plus(e1, e2) | Times (e1,e2) | Minus (e1,e2) | Mask (e1,e2) | Xor (e1,e2) | BOr (e1,e2) | Shl (e1,e2) | Concat (e1,e2)
    -> variables_expr e1 @ variables_expr e2

and variables_test t =
  match t with
  | True
  | False -> []
  | Eq(e1, e2)
  | Le(e1, e2) -> variables_expr e1 @ variables_expr e2
  | And(t1, t2)
  | Or(t1, t2)
  | Impl(t1, t2)
  | Iff(t1, t2) -> variables_test t1 @ variables_test t2
  | Neg(t1) -> variables_test t1

and get_width e =
  match e with
  | Value(Int(_, w)) | Var(_, w) | Hole(_, w) | Cast(w,_)
    -> w
  | Slice {hi;lo;_} -> let sz = hi - lo in
                       if sz < 0 then -1 else sz
  | Plus es | Times es | Minus es | Mask es | Xor es | BOr es | Shl es | Concat es
    -> get_width (fst es)

(**** BEGIN CLASSBENCH ***)

let rec to_int (bytes : int list) =
  match bytes with
  | [] -> 0
  | x::xs -> Int.shift_left x (8 * List.length xs) + to_int xs


let parse_ip_mask str =
  let addr, mask =
    String.substr_replace_all str ~pattern:"@" ~with_:""
    |> String.rsplit2_exn ~on:'/' in
  let addr_ints =
    String.split addr ~on:'.'
    |> List.map ~f:(fun i -> int_of_string i)
  in
  let mask_idx =
    let f_idx = float_of_string(mask) /. 4.0 in
    if Float.round_up f_idx = Float.round_down f_idx then
      int_of_float f_idx
    else failwith ("unknown mask " ^ mask)
  in
  let lo, hi = List.foldi addr_ints ~init:([],[])
                 ~f:(fun i (lo, hi) char ->
                   if i < mask_idx then
                     (lo @ [char], hi @ [char])
                   else
                     (lo @ [0], hi @ [255])) in
  let lo_int = to_int lo in
  let hi_int = to_int hi in
  if lo_int = hi_int then
    Match.Exact(mkInt(lo_int, 32))
  else
    Match.Between(mkInt(lo_int, 32), mkInt(hi_int, 32))
  
    
let parse_port_range str =
  let lo,hi = String.lsplit2_exn str ~on:':' in
  let () = Printf.printf "(%s:%s)\n%!" lo hi in
  let lo_int = String.strip lo |> int_of_string in
  let hi_int = String.strip hi |> int_of_string in
  if lo = hi
  then Match.Exact(mkInt(lo_int, 9))
  else Match.Between(mkInt(lo_int, 9), mkInt(hi_int, 9))

let parse_proto str =
  let proto,_ = String.lsplit2_exn str ~on:'/' in
  Match.Exact(Int(Bigint.of_string proto, 8))
              
let generate_edits cb_rules =
  let drop_table = List.fold (pow 2 9 |> range_ex 0) ~init:IntMap.empty
                     ~f:(fun acc port -> IntMap.set acc ~key:port ~data:(Random.int 2)) in
  List.fold cb_rules ~init:[]
    ~f:(fun acc (_, ip_dst, _,_,_) ->
      let out_port = Random.int (pow 2 9) in
      acc @ [
          IntMap.fold drop_table ~init:[]
            ~f:(fun ~key ~data acc ->
                acc @ [("of", ([ip_dst; Match.Exact(mkInt(key, 9))], [mkInt(out_port, 9)], data))]
            )
        ]
    )

let generate_pipe1_edits cb_rules : Edit.t list list =
  let drop_table = List.fold (pow 2 9 |> range_ex 0) ~init:IntMap.empty
                     ~f:(fun acc port -> IntMap.set acc ~key:port ~data:(Random.int 2)) in
  let ip_table =
    List.fold cb_rules ~init:[]
    ~f:(fun acc (_, ip_dst, _,_,_) ->
      let out_port = Random.int (pow 2 9) in
      acc
      @ (if Random.int 2 = 0
          then []
          else let pt = Random.int (pow 2 9) in
               [[Tables.Edit.Add ("ingress", ([Match.Exact(mkInt(pt,9))], [], IntMap.find_exn drop_table pt))]]
        )
      @ [[Tables.Edit.Add ("ipv4_fwd", ([ip_dst], [mkInt(out_port, 9)], 0))]]
    )
  in
  ip_table

let restart_timer (params : Parameters.t) st =
  match params.timeout with
  | None -> params
  | Some (_,dur) -> {params with timeout = Some(st,dur)}

let mk_ith_meta i = Printf.sprintf "m%d" i
let mk_ith_var i =  Printf.sprintf "x%d" i


let mk_normal_keys sz num_xs =
  List.map (range_ex 0 (num_xs+1)) ~f:(fun i ->
      mk_ith_var i, sz)

let mk_ith_keys sz num_xs ith_meta =
  List.map (range_ex 0 (num_xs+1))
    ~f:(fun i ->
      if i <= ith_meta then
        if ith_meta > num_xs then
          (mk_ith_meta ((ith_meta - num_xs) + i), sz)
        else
          (mk_ith_meta i, sz)
      else
        (mk_ith_var i,sz)
    )


let mk_ith_table sz num_tables tbl_idx num_xs num_ms =
  let idx_of_min_mtbl = num_tables - num_ms -1 in
  mkApply(Printf.sprintf "physical%d" tbl_idx,
          (if tbl_idx > idx_of_min_mtbl then
             mk_ith_keys sz num_xs (tbl_idx - idx_of_min_mtbl - 1)
          else
            mk_normal_keys sz num_xs),
          (if tbl_idx >= num_tables - num_ms - 1 && tbl_idx < num_tables - 1
           then [[Printf.sprintf "d%i" tbl_idx, sz], mk_ith_meta (tbl_idx - idx_of_min_mtbl) %<-% Var(Printf.sprintf "d%i" tbl_idx, sz)]
           else [[Printf.sprintf "d%i" tbl_idx, 9], "out" %<-% Var(Printf.sprintf "d%i" tbl_idx, 9)]),
          Skip)



let initialize_ms sz num_ms =
  if num_ms = 0 then Skip else
  List.map (range_ex 0 num_ms)
    ~f:(fun i -> mk_ith_meta i %<-% mkVInt (0,sz))
  |> sequence


let create_bench sz num_tables num_xs num_ms =
  sequence [ initialize_ms sz num_ms
           ; sequence @@
               List.map (range_ex 0 num_tables)
                 ~f:(fun tbl_idx ->
                   mk_ith_table sz num_tables tbl_idx num_xs num_ms
                 )
    ]


let match_row sz num_xs ~ith ~has_value =
  let open Match in
  let wildcard =  mask_ (mkInt(0,32)) (mkInt(0,32)) in
  let matches =
    Util.repeat (ith) wildcard
    @ [ Exact(mkInt(has_value,sz)) ]
    @ Util.repeat (num_xs - ith) wildcard
  in
  Edit.Add("logical",(matches, [mkInt(has_value,9)], 0))

let match_row_easier sz num_xs ~has_value =
  let open Match in
  let wildcard =  mask_ (mkInt(0,32)) (mkInt(0,32)) in
  let matches =
    Exact(mkInt(has_value,sz)) :: Util.repeat (num_xs) wildcard
  in
  Edit.Add("logical",(matches, [mkInt(has_value,9)], 0))


let rec create_log_edits_easier sz i max_edits num_xs =
  if i = max_edits then []
  else
    match_row_easier sz num_xs ~has_value:i
    :: create_log_edits_easier sz (i + 1) max_edits num_xs


let rec create_log_edits sz i max_edits num_xs =
  if i = max_edits then []
  else
    match_row sz num_xs ~ith:(i mod num_xs) ~has_value:(i / num_xs)
    :: create_log_edits sz (i + 1) max_edits num_xs

let square_bench params sz n max_edits =
  let fvs = ("out",9) :: mk_normal_keys sz n in
  let logical_table =
    mkApply("logical",
            mk_normal_keys sz n,
            [["o", 9], "out" %<-% Var("o", 9)],
            Skip)
  in
  Printf.printf "Logical table: \n %s\n\n" (string_of_cmd logical_table);
  let physical_tables =
    List.fold (range_ex 0 n) ~init:[]
    ~f:(fun init num_xs ->
      List.fold(range_ex 0 n) ~init
        ~f:(fun acc num_ms ->
          let p = create_bench sz n num_xs num_ms in
          Printf.printf "\n\n-------%d---%d----------\n%s\n--------------\n\n%!"num_xs num_ms (string_of_cmd p);
          acc @ [(num_xs, num_ms), p]))
  in
  let log_edits =
    create_log_edits_easier 32 0 max_edits n |> List.map ~f:(List.return)
  in
  let problem phys = Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
  List.fold physical_tables ~init:"numxs,num_ms,time"
    ~f:(fun acc ((xs,ms),phys) ->
      Printf.printf "\n------%d,%d-----\n" xs ms;
      (* if xs = 10 && ms = 8 then acc else *)
      Synthesis.edit_cache := EAbstr.make ();
      let st = Time.now () in
      let es = measure (restart_timer params st) None (problem phys) log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None ->
         Printf.sprintf "%s\n%d,%d,TIMEOUT" acc xs ms
      | Some _ ->
         Printf.sprintf "%s\n%d,%d,%f" acc xs ms (Time.Span.to_ms dur)
    )
  |> Printf.printf "%s\n"



(****************************************
 *     Representative pipeline          *
 ****************************************)

let cb_to_matches fvs cb_row =
  List.map fvs ~f:(fun (f,sz) ->
                    get cb_row f
                    |> Option.value ~default:(Match.mask_ (mkInt(0,sz)) (mkInt(0,sz))))

let generate_out acc  =
  let open Edit in
  let biggest = List.fold acc ~init:Bigint.one ~f:(fun max_so_far curr ->
                    match curr with
                    | Add(_,(_,[Int(i,_)],_)) when Bigint.(i > max_so_far) -> i
                    | _ -> max_so_far
                  ) in
  Bigint.(if (biggest + one) % (of_int 512) = zero
          then biggest + one + one
          else biggest + one)

  
let rep params data nrules =
  let fvs = ["ip_src",32; "ip_dst",32] in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    List.fold cb_rows ~init:([] : Edit.t list)
         ~f:(fun acc cb_row ->
           let open Edit in
           let fields = cb_fvs in
           let matches = project cb_row fields |> (cb_to_matches fvs) in
           if List.exists acc ~f:(function
                  | Add (_,(ms,_,_)) -> ms = matches
                  | _ -> false)
              || List.for_all matches ~f:(function
                     | Mask(_,m) -> Bigint.(get_int m = zero)
                     | _ -> false)
           then acc
           else
             let outp = generate_out acc in
             acc @ [Add("obt", (matches, [Int(outp,9)], 0))]
         )
  in
  Printf.printf "there are cleaned rules %d\n%!" (List.length gen_data);
  let gen_data = List.filteri gen_data ~f:(fun i _ -> i < nrules) in
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply ("obt",
                 ["ip_src",32;"ip_dst",32],
                 [["o",9], "out" %<-% Var("o",9)],
                 Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let phys =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply("validation",
                ["ip_src",32],
                [["ov",9], "out" %<-% Var("ov",9)],
                Skip) ;
        mkApply("fwd",
                ["ip_dst",32],
                [["of",9], "out" %<-% Var("of",9) ],
                Skip);
        mkApply("acl",
                ["ip_src",32; "ip_dst",32],
                [["oa",9], "out" %<-% Var("oa", 9)],
                Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let problem = Problem.make ~log:log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
  measure (restart_timer params (Time.now())) None problem (List.(gen_data >>| return))


let rep_middle params data nrules =
  let fvs = ["ip_src",32; "ip_dst",32; "proto",8; "tcp_sport",16; "tcp_dport",16] in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    List.fold cb_rows ~init:([] : Edit.t list)
         ~f:(fun acc cb_row ->
           let open Edit in
           let fields = cb_fvs in
           let matches = project cb_row fields |> (cb_to_matches fvs) in
           if List.exists acc ~f:(function
                  | Add (_,(ms,_,_)) -> ms = matches
                  | _ -> false)
              || List.for_all matches ~f:(function
                     | Mask(_,m) -> Bigint.(get_int m = zero)
                     | _ -> false)
           then acc
           else
             let outp = generate_out acc in
             acc @ [Add("obt", (matches, [Int(outp,9)], 0))]
         )
  in
  Printf.printf "there are cleaned rules %d\n%!" (List.length gen_data);
  let gen_data = List.filteri gen_data ~f:(fun i _ -> i < nrules) in
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply ("obt",
                 ["ip_src",32;"ip_dst",32; "proto",8; "tcp_sport", 16; "tcp_dport", 16],
                 [["o",9], "out" %<-% Var("o",9)],
                 Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let phys =
    sequence [
        "out" %<-% mkVInt(0,9);
        (* mkOrdered [
         *     Var("proto",8) %=% mkVInt(6,8),
         *     sequence [
         *         "tcp_sport" %<-% mkVInt(0,16);
         *         "tcp_dport" %<-% mkVInt(0,16)
         *       ];
         *     True , Skip
         *   ]; *)
        mkApply("validation",
                ["ip_src",32],
                [["ov",9], "out" %<-% Var("ov",9)],
                Skip) ;
        mkApply("fwd",
                ["ip_dst",32; "proto",8; "tcp_dport",16],
                [["of",9], "out" %<-% Var("of",9) ],
                Skip);
        mkApply("acl",
                ["ip_src",32; "ip_dst",32; "proto",8; "tcp_sport", 16; "tcp_dport", 16],
                [["oa",9], "out" %<-% Var("oa", 9)],
                Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let problem = Problem.make ~log:log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
  measure (restart_timer params (Time.now())) None problem (List.(gen_data >>| return))



let exactify_matches =
  let open Tables in
  List.map ~f:(fun m ->
      if Match.is_wildcard m then m else
      match m with
      | Mask (v,_) | Exact (v) -> Exact v
      | Between (lo,hi) ->
         if Bigint.(get_int lo = zero)
         then Exact hi
         else Exact lo
    )


let rep_of params exactify data nrules =
  let fvs = ["in_port",9;"eth_src",48;"eth_dst",48;"eth_typ",16;"ip_src",32; "ip_dst",32; "proto",8; "tcp_sport",16; "tcp_dport",16;"vlan", 12; "pcp", 3] in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    Printf.printf "There are %d rows \n%!" (List.length cb_rows);
    List.fold cb_rows ~init:([] : Edit.t list)
         ~f:(fun acc cb_row ->
           let open Edit in
           let fields = cb_fvs in
           let matches = project cb_row fields |> (cb_to_matches fvs) in
           let matches = if exactify then exactify_matches matches else matches in
           if List.exists acc ~f:(function
                  | Add (_,(ms,_,_)) -> ms = matches
                  | _ -> false)
              || List.for_all matches ~f:(function
                     | Mask(_,m) -> Bigint.(get_int m = zero)
                     | _ -> false)
           then
             (* let () = Printf.printf "\tthrowing out %s\n" (Edit.to_string (Add("obt",(matches, [], -1)))) in *)
             acc
           else
             let outp = generate_out acc in
             let e =  Add("obt", (matches, [Int(outp,9)], 0)) in
             (* let () = Printf.printf "Keeping %s\n" (Edit.to_string e) in *)
             acc @ [e]
         )
  in
  Printf.printf "there are %d cleaned rules\n%!" (List.length gen_data);
  let gen_data = List.filteri gen_data ~f:(fun i _ -> i < nrules) in
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply ("obt",
                 ["in_port",9;"eth_src",48;"eth_dst",48;"eth_typ",16;"ip_src",32; "ip_dst",32; "proto",8; "tcp_sport",16; "tcp_dport",16; "vlan", 12; "pcp", 3],
                 [["o",9], "out" %<-% Var("o",9)],
                 Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let phys =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply("validation",
                ["in_port", 9;"eth_src",48;"eth_typ",16;"ip_src",32],
                [["ov",9], "out" %<-% Var("ov",9)],
                Skip) ;
        mkApply("fwd",
                ["eth_dst",48;"ip_dst",32; "proto",8; "tcp_dport",16],
                [["of",9], "out" %<-% Var("of",9) ],
                Skip);
        mkApply("acl",
                ["in_port", 9;"eth_src",48; "eth_dst",48; "eth_typ",16; "ip_src",32; "ip_dst",32; "proto",8; "tcp_sport", 16; "tcp_dport", 16; "vlan", 12; "pcp", 3],
                [["oa",9], "out" %<-% Var("oa", 9)],
                Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let problem = Problem.make ~log:log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
  measure (restart_timer params (Time.now())) None problem (List.(gen_data >>| return))



let rep_par params data nrules =
  let fvs = ["in_port",9;"eth_src",48;"eth_dst",48;"eth_typ",16;"ip_src",32; "ip_dst",32; "proto",8; "tcp_sport",16; "tcp_dport",16] in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    Printf.printf "There are %d rows \n%!" (List.length cb_rows);
    List.fold cb_rows ~init:([] : Edit.t list)
      ~f:(fun acc cb_row ->
        let open Edit in
        let fields = cb_fvs in
        let matches = project cb_row fields |> (cb_to_matches fvs) in
        if List.exists acc ~f:(function
               | Add (_,(ms,_,_)) -> ms = matches
               | _ -> false)
           || List.for_all matches ~f:(function
                  | Mask(_,m) -> Bigint.(get_int m = zero)
                  | _ -> false)
        then
          (* let () = Printf.printf "\tthrowing out %s\n" (Edit.to_string (Add("obt",(matches, [], -1)))) in *)
          acc
        else
          let outp = generate_out acc in
          let e =  Add("obt", (matches, [Int(outp,9)], 0)) in
          (* let () = Printf.printf "Keeping %s\n" (Edit.to_string e) in *)
          acc @ [e]
      )
  in
  Printf.printf "there are %d cleaned rules\n%!" (List.length gen_data);
  let gen_data = List.filteri gen_data ~f:(fun i _ -> i < nrules) in
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply ("obt",
                 ["in_port",9;"eth_src",48;"eth_dst",48;"eth_typ",16;"ip_src",32; "ip_dst",32; "proto",8; "tcp_sport",16; "tcp_dport",16],
                 [["o",9], "out" %<-% Var("o",9)],
                 Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let phys =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply("station",
                ["in_port", 9;"eth_src",48],
                [["n",9], "next_tbl" %<-% Var("ov",2)],
                "next_tbl" %<-% mkVInt(0,2)) ;
        mkOrdered [
            Var("next_tbl",2) %=% mkVInt(1,2), mkApply("l3",
                                                       ["ip_dst",32],
                                                       [["of",9], "out" %<-% Var("of",9) ],
                                                       Skip);
            Var("next_tbl",2) %=% mkVInt(2,2), mkApply("l2",
                                                       ["eth_dst",32],
                                                       [["o2",9], "out" %<-% Var("o2",9) ],
                                                       Skip);

            Var("next_tbl",2) %=% mkVInt(3,2), mkApply("l4",
                                                       ["tcp_dst",32],
                                                       [["o4",9], "out" %<-% Var("o4",9)],
                                                       Skip);
            True , Skip
          ];
        mkApply("acl",
                ["in_port", 9;"eth_src",48; "eth_dst",48; "eth_typ",16; "ip_src",32; "ip_dst",32; "proto",8; "tcp_sport", 16; "tcp_dport", 16],
                [["oa",9], "out" %<-% Var("oa", 9)],
                Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let problem = Problem.make ~log:log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
  measure (restart_timer params (Time.now())) None problem (List.(gen_data >>| return))



(********************************
 *     Benchmark # headers
 *********************************)

let headers params sz ntables max_headers max_edits =
  let physical_tables =
    List.fold (range_ex 0 max_headers) ~init:[]
      ~f:(fun acc num_xs ->
        let fvs = ("out",9) :: mk_normal_keys sz ntables in
        let logical_table =
          mkApply("logical",
                  mk_normal_keys sz ntables,
                  [["o", 9], "out" %<-% Var("o", 9)],
                  Skip)
        in
        let phys = create_bench sz ntables num_xs 0 in
        let log_edits =
          create_log_edits 32 0 max_edits ntables |> List.map ~f:(List.return)
        in
        let problem = Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
        acc @ [num_xs,problem,log_edits ])
  in
  List.fold physical_tables ~init:"numxs,num_ms,time"
    ~f:(fun acc (num_xs,problem,log_edits) ->
      Synthesis.edit_cache := EAbstr.make ();
      let st = Time.now () in
      let es = measure (restart_timer params st) None problem log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None ->
         Printf.sprintf "%s\n%d,TIMEOUT" acc num_xs
      | Some _ ->
         Printf.sprintf "%s\n%d,%f" acc num_xs (Time.Span.to_ms dur)
    )
  |> Printf.printf "%s\n"



(***************************************
 *        Benchmark Metadata           *
 ***************************************)

let metadata params sz nmeta nedits =
  let problems =
    List.fold (range_ex 0 nmeta) ~init:[]
      ~f:(fun acc num_ms ->
        let fvs = [("out",9) ; ("x",sz)] in
        let logical_table =
          sequence [
              "out" %<-% mkVInt(0,9);
              mkApply("logical",
                      ["x", sz],
                      [["o", 9], "out" %<-% Var("o", 9)],
                      Skip)
            ]
        in
        let phys =
          "out" %<-% mkVInt(0,9) ::
            List.map (range_ex 0 nmeta)
              ~f:(fun i -> Printf.sprintf "m%d" i %<-% mkVInt(0,sz))
          @
          List.map (range_ex 0 nmeta)
            ~f:(fun table_idx ->
              if table_idx = 0 then
                mkApply("phys0", ["x",sz], [["d0",sz], "m0" %<-% Var("d0",sz) ], Skip)
              else if table_idx + 1 = nmeta then
                  mkApply(Printf.sprintf "phys%d"  table_idx,
                          [Printf.sprintf "m%d" (table_idx - 1), sz],
                          [[Printf.sprintf "d%d" table_idx, 9],
                           "out" %<-% Var(Printf.sprintf "d%d" table_idx, 9)
                          ],
                          Skip)
              else
                mkApply(Printf.sprintf "phys%d"  table_idx,
                        [Printf.sprintf "m%d" (table_idx - 1), sz],
                        [[Printf.sprintf "d%d" table_idx, sz],
                         Printf.sprintf "m%d" table_idx %<-% Var(Printf.sprintf "d%d" table_idx, sz)
                        ],
                        Skip)

            ) |> sequence
        in
        let log_edits =
          List.map (range_ex 0 nedits)
            ~f:(fun i ->
              [Edit.Add("logical", ([Match.Exact(mkInt(i,sz))], [mkInt(i,9)],0)) ]
            )
        in
        Printf.printf "Log:\n%s\n%!" (string_of_cmd logical_table);
        Printf.printf "Phys:\n%s\n%!" (string_of_cmd phys);
        let problem = Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
        acc @ [ num_ms, problem, log_edits ])
  in
  List.fold problems ~init:"num_ms,time"
    ~f:(fun acc (num_xs,problem,log_edits) ->
      Synthesis.edit_cache := EAbstr.make ();
      let st = Time.now () in
      let es = measure (restart_timer params st) None problem log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None ->
         Printf.sprintf "%s\n%d,TIMEOUT" acc num_xs
      | Some _ ->
         Printf.sprintf "%s\n%d,%f" acc num_xs (Time.Span.to_ms dur)
    )
  |> Printf.printf "%s\n"


(***************************************
 *        Benchmark Length           *
 ***************************************)

let tables params sz max_tables nheaders max_edits =
  let physical_tables =
    List.fold (range_ex 1 max_tables) ~init:[]
      ~f:(fun acc ntables ->
        let fvs = ("out",9) :: mk_normal_keys sz nheaders in
        let logical_table =
          mkApply("logical",
                  mk_normal_keys sz nheaders,
                  [["o", 9], "out" %<-% Var("o", 9)],
                  Skip)
        in
        let phys = create_bench sz ntables nheaders 0   in
        Printf.printf "phys \n%s\n%!" (string_of_cmd phys);
        let log_edits =
          create_log_edits 32 0 max_edits nheaders |> List.map ~f:(List.return)
        in
        let problem = Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
        acc @ [ntables,problem,log_edits ])
  in
  List.fold physical_tables ~init:"ntables,time"
    ~f:(fun acc (num_xs,problem,log_edits) ->
      Synthesis.edit_cache := EAbstr.make ();
      let st = Time.now () in
      let es = measure (restart_timer params st) None problem log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None ->
         Printf.sprintf "%s\n%d,TIMEOUT" acc num_xs
      | Some _ ->
         Printf.sprintf "%s\n%d,%f" acc num_xs (Time.Span.to_ms dur)
    )
  |> Printf.printf "%s\n"



(***************************************
 *        Benchmark Breadth           *
 ***************************************)

let create_par_bench sz num_tables num_xs num_ms =
  let tblsize = max (log2 (num_tables + 2)) 1 in
  sequence [ initialize_ms sz num_ms
           ; mkApply("stage",
                     mk_normal_keys sz num_xs,
                     [["stg",tblsize], "table_id" %<-% Var("stg",tblsize)],
                     "table_id" %<-% mkVInt(num_tables + 1, tblsize))
           ; mkOrdered @@
               List.map (range_ex 0 num_tables)
                 ~f:(fun tbl_idx ->
                   mkVInt(tbl_idx,tblsize) %=% Var("table_id",tblsize),
                   mk_ith_table sz num_tables tbl_idx num_xs num_ms
                 )
    ]


let breadth params sz max_tables nheaders max_edits =
  let physical_tables =
    List.fold (range_ex 1 max_tables) ~init:[]
      ~f:(fun acc ntables ->
        let fvs = ("out",9) :: mk_normal_keys sz nheaders in
        let logical_table =
          mkApply("logical",
                  mk_normal_keys sz nheaders,
                  [["o", 9], "out" %<-% Var("o", 9)],
                  Skip)
        in
        let phys = create_par_bench sz ntables nheaders 0 in
        Printf.printf "phys \n%s\n%!" (string_of_cmd phys);
        let log_edits =
          create_log_edits 32 0 max_edits nheaders |> List.map ~f:(List.return)
        in
        let problem = Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
        acc @ [ntables,problem,log_edits ])
  in
  List.fold physical_tables ~init:"ntables,time"
    ~f:(fun acc (num_xs,problem,log_edits) ->
      Synthesis.edit_cache := EAbstr.make ();
      let st = Time.now () in
      let es = measure (restart_timer params st) None problem log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None ->
         Printf.sprintf "%s\n%d,TIMEOUT" acc num_xs
      | Some _ ->
         Printf.sprintf "%s\n%d,%f" acc num_xs (Time.Span.to_ms dur)
    )
  |> Printf.printf "%s\n"
