open Core
open Synthesis
open Util
open Classbenching
module IntMap = Map.Make (Int)

let parse_file (filename : string) : Cmd.t =
  let cts = In_channel.read_all filename in
  let lexbuf = Lexing.from_string cts in
  Parser.main Lexer.tokens lexbuf

let parse_fvs fp : ((string * int) * (string * int)) list =
  In_channel.read_lines fp
  |> List.map ~f:(fun line ->
         match String.lsplit2 line ~on:',' with
         | None -> Printf.sprintf "Malformed FV line %s" line |> failwith
         | Some (abs, phys_cont) -> (
           match String.lsplit2 phys_cont ~on:',' with
           | None -> Printf.sprintf "Malformed FV line %s" line |> failwith
           | Some (phys, sz_str) ->
               let sz =
                 try int_of_string sz_str
                 with _ ->
                   failwith
                   @@ Printf.sprintf "Couldn't parse int from %s" sz_str
               in
               ((abs, sz), (phys, sz)) ))

let rec run_experiment iter seq (phys_seq : Edit.t list)
    (params : Parameters.t) hints (problem : Problem.t) =
  match seq with
  | [] -> Some phys_seq
  | edit :: edits -> (
      (* Printf.printf "==== BENCHMARKING INSERTION OF (%s) =====\n%!"
       *   (string_of_edit edit); *)
      let data = ProfData.zero () in
      let problem_inner = Problem.(replace_log_edits problem edit) in
      let st = Time.now () in
      assert (List.length (Problem.phys_edits problem_inner) = 0) ;
      match cegis_math params data problem_inner with
      | None ->
          None
          (* (\* let _ : Edit.t list option = cegis_math {params with debug = true} data problem_inner in *\)
           * failwith "example failed" *)
      | Some pedits ->
          (!data.time := Time.(diff (now ()) st)) ;
          !data.log_inst_size := Problem.log_inst problem |> Instance.size ;
          !data.phys_inst_size := Problem.phys_inst problem |> Instance.size ;
          if params.hot_start then
            Printf.eprintf "\t%s\n%!" (ProfData.to_string !data)
          else Printf.printf "%s\n%!" (ProfData.to_string !data) ;
          run_experiment (iter + 1) edits (phys_seq @ pedits) params hints
            Problem.(
              problem
              |> flip (apply_edits_to_log params) edit
              |> flip (apply_edits_to_phys params) pedits
              |> delete_phys_edits) )

let rec measure (params : Parameters.t) hints problem insertions =
  if params.hot_start then (
    Printf.eprintf "\t%s\n%!" ProfData.header_string ;
    ignore
      ( run_experiment 0 insertions [] params hints (problem ())
        : Edit.t list option ) ;
    measure {params with hot_start= false} hints problem insertions )
  else (
    Printf.printf "%s\n%!" ProfData.header_string ;
    run_experiment 0 insertions [] params hints (problem ()) )

let permute l =
  List.map l ~f:(inj_r (Random.int (List.length l)))
  |> List.sort ~compare:(fun (i, _) (j, _) -> compare i j)
  |> List.map ~f:snd

let tbl = Printf.sprintf "tbl%d"

let rec mk_pipeline varsize n =
  let open Cmd in
  if n = 0 then []
  else
    ( tbl n
    , [("k_" ^ tbl n, varsize)]
    , [ ( "Action"
        , [(Printf.sprintf "v%d" n, varsize)]
        , ("x_" ^ tbl n) %<-% Var (Printf.sprintf "v%d" n, varsize) ) ]
    , ("x_" ^ tbl n) %<-% Expr.value (0, varsize) )
    :: mk_pipeline varsize (n - 1)

let rec generate_n_insertions varsize length n avail_tables maxes :
    Edit.t list =
  if n = 0 then
    let (_ : unit) = Printf.printf "--generated--\n%!" in
    []
  else if List.is_empty avail_tables then
    let (_ : unit) = Printf.printf "--filled up --\n%!" in
    []
  else
    let (_ : unit) = Printf.printf "generating %d \n%!" n in
    let rec loop_free_match avail_tables =
      if List.is_empty avail_tables then None
      else
        let i =
          Random.int (List.length avail_tables) |> List.nth_exn avail_tables
        in
        let max_i =
          StringMap.find maxes (tbl i) |> Option.value ~default:0
        in
        let k = Printf.sprintf "k_%d" i in
        Printf.printf "%s max : %d\n%!" (tbl i) max_i ;
        if max_i >= pow 2 varsize then
          loop_free_match (List.filter avail_tables ~f:(( <> ) i))
        else
          let max', mtch =
            if Random.int 6 < 1 then
              (max_i + 1, Match.exact_ k (Value.make (max_i, varsize)))
            else
              let lo = max_i in
              let hi = min (lo + Random.int 3) (pow 2 varsize - 1) in
              if lo = hi then
                (hi + 1, Match.exact_ k (Value.make (hi, varsize)))
              else
                ( hi + 1
                , Match.between_ k
                    (Value.make (lo, varsize))
                    (Value.make (hi, varsize)) )
          in
          let maxes' = StringMap.set maxes ~key:(tbl i) ~data:max' in
          let act_data = Value.make (Random.int (pow 2 varsize), varsize) in
          let row = ([mtch], [act_data], 0) in
          Some (maxes', avail_tables, tbl i, row)
    in
    match loop_free_match avail_tables with
    | None ->
       let () = Printf.printf "--filled up --\n%!" in
       []
    | Some (maxes', avail_tables', name, row) ->
       let () = Printf.printf "Inserting\n%!" in
       Add (name, row)
       :: generate_n_insertions varsize length (n - 1) avail_tables' maxes'

let reorder_benchmark varsize length max_inserts params =
  let open Cmd in
  Random.init 99 ;
  let logical_pipeline = mk_pipeline varsize length in
  let physical_pipeline = permute logical_pipeline in
  let log_inst = Instance.empty in
  let phys_inst = Instance.empty in
  let to_cmd line =
    List.(line >>| (fun t -> apply t) |> reduce_exn ~f:( %:% ))
  in
  let log = to_cmd logical_pipeline in
  let phys = to_cmd physical_pipeline in
  let insertion_sequence =
    generate_n_insertions varsize length max_inserts
      (range_ex 1 (length + 1))
      StringMap.empty
    |> List.(map ~f:return)
  in
  let fvs =
    range_ex 1 (length + 1)
    |> List.map ~f:(fun i ->
           [ ("k_" ^ tbl i, varsize)
           ; ("x_" ^ tbl i, varsize)
             (* ; ("?ActIn"^tbl i, 8) *) ])
    |> List.join
  in
  let problem =
    Problem.make ~log ~phys ~fvs ~log_inst ~phys_inst ~log_edits:[]
  in
  measure params (Some List.return) problem insertion_sequence

(** ONF BENCHMARK **)

let translate_key (var_mapping : ((string * int) * (string * int)) list)
    (key : string) =
  match
    List.find_map var_mapping ~f:(fun ((k, _), (k', _)) ->
        Option.some_if String.(k = key) k')
  with
  | Some key -> key
  | None -> key

let onos_to_edits (var_mapping : ((string * int) * (string * int)) list)
    filename tbl_nm (key : string) =
  let key = translate_key var_mapping key in
  let lines = In_channel.read_lines filename in
  let make_edit data : Edit.t =
    match data with
    | [_; "ADD"; _; ipv6; id] ->
        Add
          ( tbl_nm
          , ( [Match.mk_ipv6_match key ipv6]
            , [Value.big_make (Bigint.of_string id, 32)]
            , 0 ) )
    | [_; "REMOVE"; _; _; _] -> failwith "cannot yet handle removes"
    | _ ->
        Printf.sprintf "Unrecognized row: %s\n%!"
          (List.intersperse data ~sep:"---" |> List.reduce_exn ~f:( ^ ))
        |> failwith
  in
  let edits =
    List.map lines ~f:(fun line -> [String.split line ~on:',' |> make_edit])
  in
  edits

let ( %> ) c c' = ignore c ; c'

let rec basic_onf_ipv4_real params data_file log_p4 phys_p4 log_edits_file
    phys_edits_file fvs_file assume_file log_inc phys_inc =
  let var_mapping = parse_fvs fvs_file in
  let fvs = List.map var_mapping ~f:snd in
  let assm = parse_file assume_file in
  (* let print_fvs = printf "fvs = %s" (Sexp.to_string ([%sexp_of: (string *
     int) list] fvs)) in *)
  let log =
    Cmd.(assm %:% Encode.encode_from_p4 log_inc log_p4 false)
    |> Encode.unify_names var_mapping
    |> zero_init fvs |> drop_handle fvs
  in
  let phys =
    Cmd.(assm %:% Encode.encode_from_p4 phys_inc phys_p4 false)
    |> Encode.unify_names var_mapping
    |> zero_init fvs |> drop_handle fvs
    (* |> CompilerOpts.optimize fvs *)
  in
  (* let maxN n = Bigint.(of_int_exn n ** of_int_exn 2 - one) in *)
  (* let fvs = parse_fvs fvs in *)
  let log_edits = Runtime.parse log log_edits_file in
  let phys_edits = Runtime.parse phys phys_edits_file in
  let problem =
    Problem.make ~log ~phys ~fvs
      ~log_inst:Instance.(update_list params empty log_edits)
      ~phys_inst:Instance.(update_list params empty phys_edits)
      ~log_edits:[]
  in
  assert (implements params (ProfData.zero ()) (problem ()) |> Option.is_none) ;
  measure params None problem
    (onos_to_edits var_mapping data_file "routing_v6" "hdr.ipv6.dst_addr")

and zero_init fvs cmd =
  let fvs = StringSet.of_list @@ fsts @@ fvs in
  let vs =
    Cmd.vars cmd
    |> List.dedup_and_sort ~compare:(fun (v1, _) (v2, _) ->
           String.compare v1 v2)
  in
  let zi = List.filter vs ~f:(fun (v, _) -> StringSet.(mem fvs v) |> not) in
  Cmd.(
    sequence
    @@ List.map zi ~f:(fun (v, w) -> v %<-% Expr.value (0, w))
    @ [cmd])

and drop_handle fvs cmd =
  let open Test in
  let open Cmd in
  let c_end =
    List.map fvs ~f:(fun (v, w) -> v %<-% Expr.value (0, w))
    |> List.fold ~init:Skip ~f:(fun c1 c2 -> c1 %:% c2)
  in
  cmd
  %:% ordered
        [ ( Var ("standard_metadata.egress_spec", 9) %=% Expr.value (0, 9)
          , c_end )
        ; (True, Skip) ]

and variables cmd =
  let open Cmd in
  match cmd with
  | Skip -> []
  | Assign (s, e) -> (s, get_width e) :: variables_expr e
  | Seq (c1, c2) -> variables c1 @ variables c2
  | Select (_, tc) ->
      List.concat_map tc ~f:(fun (t, c) -> variables_test t @ variables c)
  | Apply {keys; _} -> free_keys keys
  | _ -> []

and variables_expr e =
  match e with
  | Value _ -> []
  | Var (n, w) -> [(n, w)]
  | Hole _ -> []
  | Cast (_, e) | Slice {bits= e; _} -> variables_expr e
  | Plus (e1, e2)
   |Times (e1, e2)
   |Minus (e1, e2)
   |Mask (e1, e2)
   |Xor (e1, e2)
   |BOr (e1, e2)
   |Shl (e1, e2)
   |Concat (e1, e2)
   |SatPlus (e1, e2)
   |SatMinus (e1, e2) ->
      variables_expr e1 @ variables_expr e2

and variables_test t =
  match t with
  | True | False -> []
  | Eq (e1, e2) | Le (e1, e2) -> variables_expr e1 @ variables_expr e2
  | And (t1, t2) | Or (t1, t2) | Impl (t1, t2) | Iff (t1, t2) ->
      variables_test t1 @ variables_test t2
  | Neg t1 -> variables_test t1

and get_width e =
  match e with
  | Value v -> Value.size v
  | Var (_, w) | Hole (_, w) | Cast (w, _) -> w
  | Slice {hi; lo; _} ->
      let sz = hi - lo in
      if sz < 0 then -1 else sz
  | Plus es
   |Times es
   |Minus es
   |Mask es
   |Xor es
   |BOr es
   |Shl es
   |Concat es
   |SatPlus es
   |SatMinus es ->
      get_width (fst es)

(**** BEGIN CLASSBENCH ***)

let rec to_int (bytes : int list) =
  match bytes with
  | [] -> 0
  | x :: xs -> Int.shift_left x (8 * List.length xs) + to_int xs

let restart_timer (params : Parameters.t) =
  {params with timeout= Timeout.restart params.timeout}

let mk_ith_meta i = Printf.sprintf "m%d" i

let mk_ith_var i = Printf.sprintf "x%d" i

let mk_normal_keys sz num_xs =
  List.map (range_ex 0 (num_xs + 1)) ~f:(fun i -> (mk_ith_var i, sz))

let mk_ith_keys sz num_xs ith_meta =
  List.map
    (range_ex 0 (num_xs + 1))
    ~f:(fun i ->
      if i <= ith_meta then
        if ith_meta > num_xs then (mk_ith_meta (ith_meta - num_xs + i), sz)
        else (mk_ith_meta i, sz)
      else (mk_ith_var i, sz))

let mk_ith_table sz num_tables tbl_idx num_xs num_ms =
  let open Cmd in
  let idx_of_min_mtbl = num_tables - num_ms - 1 in
  apply
    ( Printf.sprintf "physical%d" tbl_idx
    , ( if tbl_idx > idx_of_min_mtbl then
        mk_ith_keys sz num_xs (tbl_idx - idx_of_min_mtbl - 1)
      else mk_normal_keys sz num_xs )
    , ( if tbl_idx >= num_tables - num_ms - 1 && tbl_idx < num_tables - 1 then
        [ ( "action"
          , [(Printf.sprintf "d%i" tbl_idx, sz)]
          , mk_ith_meta (tbl_idx - idx_of_min_mtbl)
            %<-% Var (Printf.sprintf "d%i" tbl_idx, sz) ) ]
      else
        [ ( "action"
          , [(Printf.sprintf "d%i" tbl_idx, 9)]
          , "out" %<-% Var (Printf.sprintf "d%i" tbl_idx, 9) ) ] )
    , Skip )

let initialize_ms sz num_ms =
  let open Cmd in
  if num_ms = 0 then Skip
  else
    List.map (range_ex 0 num_ms) ~f:(fun i ->
        mk_ith_meta i %<-% Expr.value (0, sz))
    |> sequence

let create_bench sz num_tables num_xs num_ms =
  let open Cmd in
  sequence
    [ initialize_ms sz num_ms
    ; sequence
      @@ List.map (range_ex 0 num_tables) ~f:(fun tbl_idx ->
             mk_ith_table sz num_tables tbl_idx num_xs num_ms) ]

let wildcard k = Match.wildcard k 32

let wildcards_between lo hi =
  List.map (range_ex lo hi) ~f:(fun k -> wildcard (Printf.sprintf "x%d" k))

let match_row sz num_xs ~ith ~has_value : Edit.t =
  let matches =
    wildcards_between 0 (ith - 1)
    @ [Match.exact_ (Printf.sprintf "x%d" ith) (Value.make (has_value, sz))]
    @ wildcards_between (num_xs - ith) num_xs
  in
  Edit.Add ("logical", (matches, [Value.make (has_value, 9)], 0))

let match_row_easier sz num_xs ~has_value =
  let open Match in
  let matches =
    exact_ "x0" (Value.make (has_value, sz)) :: wildcards_between 1 num_xs
  in
  Edit.Add ("logical", (matches, [Value.make (has_value, 9)], 0))

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
  let open Cmd in
  let fvs = ("out", 9) :: mk_normal_keys sz n in
  let logical_table =
    apply
      ( "logical"
      , mk_normal_keys sz n
      , [("action", [("o", 9)], "out" %<-% Var ("o", 9))]
      , Skip )
  in
  Printf.printf "Logical table: \n %s\n\n" (Cmd.to_string logical_table) ;
  let physical_tables =
    List.fold (range_ex 0 n) ~init:[] ~f:(fun init num_xs ->
        List.fold (range_ex 0 n) ~init ~f:(fun acc num_ms ->
            let p = create_bench sz n num_xs num_ms in
            Printf.printf
              "\n\n-------%d---%d----------\n%s\n--------------\n\n%!" num_xs
              num_ms (Cmd.to_string p) ;
            acc @ [((num_xs, num_ms), p)]))
  in
  let log_edits =
    create_log_edits_easier 32 0 max_edits n |> List.map ~f:List.return
  in
  let problem phys =
    Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[]
      ~log_inst:Instance.empty ~phys_inst:Instance.empty
  in
  List.fold physical_tables ~init:"numxs,num_ms,time"
    ~f:(fun acc ((xs, ms), phys) ->
      Printf.printf "\n------%d,%d-----\n" xs ms ;
      (* if xs = 10 && ms = 8 then acc else *)
      Synthesis.edit_cache := EAbstr.make () ;
      let st = Time.now () in
      let es =
        measure (restart_timer params) None (problem phys) log_edits
      in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None -> Printf.sprintf "%s\n%d,%d,TIMEOUT" acc xs ms
      | Some _ ->
          Printf.sprintf "%s\n%d,%d,%f" acc xs ms (Time.Span.to_ms dur))
  |> Printf.printf "%s\n"

(****************************************
 *     Representative pipeline          *
 ****************************************)

let cb_to_matches fvs cb_row =
  List.map fvs ~f:(fun (f, sz) ->
      get cb_row f
      |> Option.value
           ~default:(Match.mask_ f (Value.make (0, sz)) (Value.make (0, sz))))

let generate_out acc =
  let open Edit in
  let biggest =
    List.fold acc ~init:Bigint.one ~f:(fun max_so_far curr ->
        match curr with
        | Add (_, (_, [v], _)) when Bigint.(Value.get_bigint v > max_so_far)
          ->
            Value.get_bigint v
        | _ -> max_so_far)
  in
  Bigint.(
    if (biggest + one) % of_int 512 = zero then biggest + one + one
    else biggest + one)

let rep params data nrules =
  let open Test in
  let open Cmd in
  let fvs = [("ip_src", 32); ("ip_dst", 32)] in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    List.fold cb_rows
      ~init:([] : Edit.t list)
      ~f:(fun acc cb_row ->
        let open Edit in
        let fields = cb_fvs in
        let matches = project cb_row fields |> cb_to_matches fvs in
        if
          List.exists acc ~f:(function
            | Add (_, (ms, _, _)) -> List.equal Match.equal ms matches
            | _ -> false)
          || List.for_all matches ~f:Match.is_wildcard
        then acc
        else
          let outp = generate_out acc in
          acc @ [Add ("obt", (matches, [Value.big_make (outp, 9)], 0))])
  in
  Printf.printf "there are cleaned rules %d\n%!" (List.length gen_data) ;
  let gen_data = List.filteri gen_data ~f:(fun i _ -> i < nrules) in
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence
      [ "out" %<-% Expr.value (0, 9)
      ; apply
          ( "obt"
          , [("ip_src", 32); ("ip_dst", 32)]
          , [("action", [("o", 9)], "out" %<-% Var ("o", 9))]
          , Skip )
      ; ordered
          [ ( Var ("out", 9) %=% Expr.value (0, 9)
            , List.fold fvs ~init:Skip ~f:(fun acc (fv, sz) ->
                  acc %:% (fv %<-% Expr.value (0, sz))) )
          ; (True, Skip) ] ]
  in
  let phys =
    sequence
      [ "out" %<-% Expr.value (0, 9)
      ; apply
          ( "validation"
          , [("ip_src", 32)]
          , [("action", [("ov", 9)], "out" %<-% Var ("ov", 9))]
          , Skip )
      ; apply
          ( "fwd"
          , [("ip_dst", 32)]
          , [("action", [("of", 9)], "out" %<-% Var ("of", 9))]
          , Skip )
      ; apply
          ( "acl"
          , [("ip_src", 32); ("ip_dst", 32)]
          , [("action", [("oa", 9)], "out" %<-% Var ("oa", 9))]
          , Skip )
      ; ordered
          [ ( Var ("out", 9) %=% Expr.value (0, 9)
            , List.fold fvs ~init:Skip ~f:(fun acc (fv, sz) ->
                  acc %:% (fv %<-% Expr.value (0, sz))) )
          ; (True, Skip) ] ]
  in
  let problem =
    Problem.make ~log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty
      ~phys_inst:Instance.empty
  in
  measure (restart_timer params) None problem List.(gen_data >>| return)

let rep_middle params data nrules =
  let open Test in
  let open Cmd in
  let fvs =
    [ ("ip_src", 32)
    ; ("ip_dst", 32)
    ; ("proto", 8)
    ; ("tcp_sport", 16)
    ; ("tcp_dport", 16) ]
  in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    List.fold cb_rows
      ~init:([] : Edit.t list)
      ~f:(fun acc cb_row ->
        let open Edit in
        let fields = cb_fvs in
        let matches = project cb_row fields |> cb_to_matches fvs in
        if
          List.exists acc ~f:(function
            | Add (_, (ms, _, _)) -> List.equal Match.equal ms matches
            | _ -> false)
          || List.for_all matches ~f:Match.is_wildcard
        then acc
        else
          let outp = generate_out acc in
          acc @ [Add ("obt", (matches, [Value.big_make (outp, 9)], 0))])
  in
  Printf.printf "there are cleaned rules %d\n%!" (List.length gen_data) ;
  let gen_data = List.filteri gen_data ~f:(fun i _ -> i < nrules) in
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence
      [ "out" %<-% Expr.value (0, 9)
      ; apply
          ( "obt"
          , [ ("ip_src", 32)
            ; ("ip_dst", 32)
            ; ("proto", 8)
            ; ("tcp_sport", 16)
            ; ("tcp_dport", 16) ]
          , [("action", [("o", 9)], "out" %<-% Var ("o", 9))]
          , Skip )
      ; ordered
          [ ( Var ("out", 9) %=% Expr.value (0, 9)
            , List.fold fvs ~init:Skip ~f:(fun acc (fv, sz) ->
                  acc %:% (fv %<-% Expr.value (0, sz))) )
          ; (True, Skip) ] ]
  in
  let phys =
    sequence
      [ "out" %<-% Expr.value (0, 9)
      ; (* mkOrdered [
         *     Var("proto",8) %=% Expr.value (6,8),
         *     sequence [
         *         "tcp_sport" %<-% Expr.value (0,16);
         *         "tcp_dport" %<-% Expr.value (0,16)
         *       ];
         *     True , Skip
         *   ]; *)
        apply
          ( "validation"
          , [("ip_src", 32)]
          , [("action", [("ov", 9)], "out" %<-% Var ("ov", 9))]
          , Skip )
      ; apply
          ( "fwd"
          , [("ip_dst", 32); ("proto", 8); ("tcp_dport", 16)]
          , [("action", [("of", 9)], "out" %<-% Var ("of", 9))]
          , Skip )
      ; apply
          ( "acl"
          , [ ("ip_src", 32)
            ; ("ip_dst", 32)
            ; ("proto", 8)
            ; ("tcp_sport", 16)
            ; ("tcp_dport", 16) ]
          , [("action", [("oa", 9)], "out" %<-% Var ("oa", 9))]
          , Skip )
      ; ordered
          [ ( Var ("out", 9) %=% Expr.value (0, 9)
            , List.fold fvs ~init:Skip ~f:(fun acc (fv, sz) ->
                  acc %:% (fv %<-% Expr.value (0, sz))) )
          ; (True, Skip) ] ]
  in
  let problem =
    Problem.make ~log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty
      ~phys_inst:Instance.empty
  in
  measure (restart_timer params) None problem List.(gen_data >>| return)

let rep_of params exactify data nrules =
  let open Test in
  let open Cmd in
  let fvs =
    [ ("in_port", 9)
    ; ("eth_src", 48)
    ; ("eth_dst", 48)
    ; ("eth_typ", 16)
    ; ("ip_src", 32)
    ; ("ip_dst", 32)
    ; ("proto", 8)
    ; ("tcp_sport", 16)
    ; ("tcp_dport", 16)
    ; ("vlan", 12)
    ; ("pcp", 3) ]
  in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    Printf.printf "There are %d rows \n%!" (List.length cb_rows) ;
    List.fold cb_rows
      ~init:([] : Edit.t list)
      ~f:(fun acc cb_row ->
        let open Edit in
        let fields = cb_fvs in
        let matches = project cb_row fields |> cb_to_matches fvs in
        let matches =
          if exactify then List.map ~f:Match.exactify matches else matches
        in
        if
          List.exists acc ~f:(function
            | Add (_, (ms, _, _)) -> List.equal Match.equal ms matches
            | _ -> false)
          || List.for_all matches ~f:Match.is_wildcard
        then
          (* let () = Printf.printf "\tthrowing out %s\n" (Edit.to_string
             (Add("obt",(matches, [], -1)))) in *)
          acc
        else
          let outp = generate_out acc in
          let e = Add ("obt", (matches, [Value.big_make (outp, 9)], 0)) in
          (* let () = Printf.printf "Keeping %s\n" (Edit.to_string e) in *)
          acc @ [e])
  in
  Printf.printf "there are %d cleaned rules\n%!" (List.length gen_data) ;
  let gen_data = List.filteri gen_data ~f:(fun i _ -> i < nrules) in
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence
      [ "out" %<-% Expr.value (0, 9)
      ; apply
          ( "obt"
          , [ ("in_port", 9)
            ; ("eth_src", 48)
            ; ("eth_dst", 48)
            ; ("eth_typ", 16)
            ; ("ip_src", 32)
            ; ("ip_dst", 32)
            ; ("proto", 8)
            ; ("tcp_sport", 16)
            ; ("tcp_dport", 16)
            ; ("vlan", 12)
            ; ("pcp", 3) ]
          , [("action", [("o", 9)], "out" %<-% Var ("o", 9))]
          , Skip )
      ; ordered
          [ ( Var ("out", 9) %=% Expr.value (0, 9)
            , List.fold fvs ~init:Skip ~f:(fun acc (fv, sz) ->
                  acc %:% (fv %<-% Expr.value (0, sz))) )
          ; (True, Skip) ] ]
  in
  let phys =
    sequence
      [ "out" %<-% Expr.value (0, 9)
      ; apply
          ( "validation"
          , [("in_port", 9); ("eth_src", 48); ("eth_typ", 16); ("ip_src", 32)]
          , [("action", [("ov", 9)], "out" %<-% Var ("ov", 9))]
          , Skip )
      ; apply
          ( "fwd"
          , [("eth_dst", 48); ("ip_dst", 32); ("proto", 8); ("tcp_dport", 16)]
          , [("action", [("of", 9)], "out" %<-% Var ("of", 9))]
          , Skip )
      ; apply
          ( "acl"
          , [ ("in_port", 9)
            ; ("eth_src", 48)
            ; ("eth_dst", 48)
            ; ("eth_typ", 16)
            ; ("ip_src", 32)
            ; ("ip_dst", 32)
            ; ("proto", 8)
            ; ("tcp_sport", 16)
            ; ("tcp_dport", 16)
            ; ("vlan", 12)
            ; ("pcp", 3) ]
          , [("action", [("oa", 9)], "out" %<-% Var ("oa", 9))]
          , Skip )
      ; ordered
          [ ( Var ("out", 9) %=% Expr.value (0, 9)
            , List.fold fvs ~init:Skip ~f:(fun acc (fv, sz) ->
                  acc %:% (fv %<-% Expr.value (0, sz))) )
          ; (True, Skip) ] ]
  in
  let problem =
    Problem.make ~log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty
      ~phys_inst:Instance.empty
  in
  measure (restart_timer params) None problem List.(gen_data >>| return)

let rep_par params data nrules =
  let open Cmd in
  let open Test in
  let fvs =
    [ ("in_port", 9)
    ; ("eth_src", 48)
    ; ("eth_dst", 48)
    ; ("eth_typ", 16)
    ; ("ip_src", 32)
    ; ("ip_dst", 32)
    ; ("proto", 8)
    ; ("tcp_sport", 16)
    ; ("tcp_dport", 16) ]
  in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    Printf.printf "There are %d rows \n%!" (List.length cb_rows) ;
    List.fold cb_rows
      ~init:([] : Edit.t list)
      ~f:(fun acc cb_row ->
        let open Edit in
        let fields = cb_fvs in
        let matches = project cb_row fields |> cb_to_matches fvs in
        if
          List.exists acc ~f:(function
            | Add (_, (ms, _, _)) -> List.equal Match.equal ms matches
            | _ -> false)
          || List.for_all matches ~f:Match.is_wildcard
        then
          (* let () = Printf.printf "\tthrowing out %s\n" (Edit.to_string
             (Add("obt",(matches, [], -1)))) in *)
          acc
        else
          let outp = generate_out acc in
          let e = Add ("obt", (matches, [Value.big_make (outp, 9)], 0)) in
          (* let () = Printf.printf "Keeping %s\n" (Edit.to_string e) in *)
          acc @ [e])
  in
  Printf.printf "there are %d cleaned rules\n%!" (List.length gen_data) ;
  let gen_data = List.filteri gen_data ~f:(fun i _ -> i < nrules) in
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence
      [ "out" %<-% Expr.value (0, 9)
      ; apply
          ( "obt"
          , [ ("in_port", 9)
            ; ("eth_src", 48)
            ; ("eth_dst", 48)
            ; ("eth_typ", 16)
            ; ("ip_src", 32)
            ; ("ip_dst", 32)
            ; ("proto", 8)
            ; ("tcp_sport", 16)
            ; ("tcp_dport", 16) ]
          , [("action", [("o", 9)], "out" %<-% Var ("o", 9))]
          , Skip )
      ; ordered
          [ ( Var ("out", 9) %=% Expr.value (0, 9)
            , List.fold fvs ~init:Skip ~f:(fun acc (fv, sz) ->
                  acc %:% (fv %<-% Expr.value (0, sz))) )
          ; (True, Skip) ] ]
  in
  let phys =
    sequence
      [ "out" %<-% Expr.value (0, 9)
      ; apply
          ( "station"
          , [("in_port", 9); ("eth_src", 48)]
          , [("action", [("n", 9)], "next_tbl" %<-% Var ("ov", 2))]
          , "next_tbl" %<-% Expr.value (0, 2) )
      ; ordered
          [ ( Var ("next_tbl", 2) %=% Expr.value (1, 2)
            , apply
                ( "l3"
                , [("ip_dst", 32)]
                , [("action", [("of", 9)], "out" %<-% Var ("of", 9))]
                , Skip ) )
          ; ( Var ("next_tbl", 2) %=% Expr.value (2, 2)
            , apply
                ( "l2"
                , [("eth_dst", 32)]
                , [("action", [("o2", 9)], "out" %<-% Var ("o2", 9))]
                , Skip ) )
          ; ( Var ("next_tbl", 2) %=% Expr.value (3, 2)
            , apply
                ( "l4"
                , [("tcp_dst", 32)]
                , [("action", [("o4", 9)], "out" %<-% Var ("o4", 9))]
                , Skip ) )
          ; (True, Skip) ]
      ; apply
          ( "acl"
          , [ ("in_port", 9)
            ; ("eth_src", 48)
            ; ("eth_dst", 48)
            ; ("eth_typ", 16)
            ; ("ip_src", 32)
            ; ("ip_dst", 32)
            ; ("proto", 8)
            ; ("tcp_sport", 16)
            ; ("tcp_dport", 16) ]
          , [("action", [("oa", 9)], "out" %<-% Var ("oa", 9))]
          , Skip )
      ; ordered
          [ ( Var ("out", 9) %=% Expr.value (0, 9)
            , List.fold fvs ~init:Skip ~f:(fun acc (fv, sz) ->
                  acc %:% (fv %<-% Expr.value (0, sz))) )
          ; (True, Skip) ] ]
  in
  let problem =
    Problem.make ~log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty
      ~phys_inst:Instance.empty
  in
  measure (restart_timer params) None problem List.(gen_data >>| return)

(********************************
 *     Benchmark # headers
 *********************************)

let headers params sz ntables max_headers max_edits =
  let open Cmd in
  let physical_tables =
    List.fold (range_ex 0 max_headers) ~init:[] ~f:(fun acc num_xs ->
        let fvs = ("out", 9) :: mk_normal_keys sz ntables in
        let logical_table =
          apply
            ( "logical"
            , mk_normal_keys sz ntables
            , [("action", [("o", 9)], "out" %<-% Var ("o", 9))]
            , Skip )
        in
        let phys = create_bench sz ntables num_xs 0 in
        let log_edits =
          create_log_edits 32 0 max_edits ntables |> List.map ~f:List.return
        in
        let problem =
          Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[]
            ~log_inst:Instance.empty ~phys_inst:Instance.empty
        in
        acc @ [(num_xs, problem, log_edits)])
  in
  List.fold physical_tables ~init:"numxs,num_ms,time"
    ~f:(fun acc (num_xs, problem, log_edits) ->
      Synthesis.edit_cache := EAbstr.make () ;
      let st = Time.now () in
      let es = measure (restart_timer params) None problem log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None -> Printf.sprintf "%s\n%d,TIMEOUT" acc num_xs
      | Some _ -> Printf.sprintf "%s\n%d,%f" acc num_xs (Time.Span.to_ms dur))
  |> Printf.printf "%s\n"

(***************************************
 *        Benchmark Metadata           *
 ***************************************)

let metadata params sz nmeta nedits =
  let open Cmd in
  let problems =
    List.fold (range_ex 0 nmeta) ~init:[] ~f:(fun acc num_ms ->
        let fvs = [("out", 9); ("x", sz)] in
        let logical_table =
          sequence
            [ "out" %<-% Expr.value (0, 9)
            ; apply
                ( "logical"
                , [("x", sz)]
                , [("action", [("o", 9)], "out" %<-% Var ("o", 9))]
                , Skip ) ]
        in
        let phys =
          ("out" %<-% Expr.value (0, 9))
          :: List.map (range_ex 0 nmeta) ~f:(fun i ->
                 Printf.sprintf "m%d" i %<-% Expr.value (0, sz))
          @ List.map (range_ex 0 nmeta) ~f:(fun table_idx ->
                if table_idx = 0 then
                  apply
                    ( "phys0"
                    , [("x", sz)]
                    , [("action", [("d0", sz)], "m0" %<-% Var ("d0", sz))]
                    , Skip )
                else if table_idx + 1 = nmeta then
                  apply
                    ( Printf.sprintf "phys%d" table_idx
                    , [(Printf.sprintf "m%d" (table_idx - 1), sz)]
                    , [ ( "action"
                        , [(Printf.sprintf "d%d" table_idx, 9)]
                        , "out" %<-% Var (Printf.sprintf "d%d" table_idx, 9)
                        ) ]
                    , Skip )
                else
                  apply
                    ( Printf.sprintf "phys%d" table_idx
                    , [(Printf.sprintf "m%d" (table_idx - 1), sz)]
                    , [ ( "action"
                        , [(Printf.sprintf "d%d" table_idx, sz)]
                        , Printf.sprintf "m%d" table_idx
                          %<-% Var (Printf.sprintf "d%d" table_idx, sz) ) ]
                    , Skip ))
          |> sequence
        in
        let log_edits =
          List.map (range_ex 0 nedits) ~f:(fun i ->
              [ Edit.Add
                  ( "logical"
                  , ( [Match.exact_ "x" (Value.make (i, sz))]
                    , [Value.make (i, 9)]
                    , 0 ) ) ])
        in
        Printf.printf "Log:\n%s\n%!" (to_string logical_table) ;
        Printf.printf "Phys:\n%s\n%!" (to_string phys) ;
        let problem =
          Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[]
            ~log_inst:Instance.empty ~phys_inst:Instance.empty
        in
        acc @ [(num_ms, problem, log_edits)])
  in
  List.fold problems ~init:"num_ms,time"
    ~f:(fun acc (num_xs, problem, log_edits) ->
      Synthesis.edit_cache := EAbstr.make () ;
      let st = Time.now () in
      let es = measure (restart_timer params) None problem log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None -> Printf.sprintf "%s\n%d,TIMEOUT" acc num_xs
      | Some _ -> Printf.sprintf "%s\n%d,%f" acc num_xs (Time.Span.to_ms dur))
  |> Printf.printf "%s\n"

(***************************************
 *        Benchmark Length           *
 ***************************************)

let tables params sz max_tables nheaders max_edits =
  let open Cmd in
  let physical_tables =
    List.fold (range_ex 1 max_tables) ~init:[] ~f:(fun acc ntables ->
        let fvs = ("out", 9) :: mk_normal_keys sz nheaders in
        let logical_table =
          apply
            ( "logical"
            , mk_normal_keys sz nheaders
            , [("action", [("o", 9)], "out" %<-% Var ("o", 9))]
            , Skip )
        in
        let phys = create_bench sz ntables nheaders 0 in
        Printf.printf "phys \n%s\n%!" (to_string phys) ;
        let log_edits =
          create_log_edits 32 0 max_edits nheaders |> List.map ~f:List.return
        in
        let problem =
          Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[]
            ~log_inst:Instance.empty ~phys_inst:Instance.empty
        in
        acc @ [(ntables, problem, log_edits)])
  in
  List.fold physical_tables ~init:"ntables,time"
    ~f:(fun acc (num_xs, problem, log_edits) ->
      Synthesis.edit_cache := EAbstr.make () ;
      let st = Time.now () in
      let es = measure (restart_timer params) None problem log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None -> Printf.sprintf "%s\n%d,TIMEOUT" acc num_xs
      | Some _ -> Printf.sprintf "%s\n%d,%f" acc num_xs (Time.Span.to_ms dur))
  |> Printf.printf "%s\n"

(***************************************
 *        Benchmark Breadth           *
 ***************************************)

let create_par_bench sz num_tables num_xs num_ms =
  let open Test in
  let open Cmd in
  let tblsize = max (log2 (num_tables + 2)) 1 in
  sequence
    [ initialize_ms sz num_ms
    ; apply
        ( "stage"
        , mk_normal_keys sz num_xs
        , [ ( "action"
            , [("stg", tblsize)]
            , "table_id" %<-% Var ("stg", tblsize) ) ]
        , "table_id" %<-% Expr.value (num_tables + 1, tblsize) )
    ; ordered
      @@ List.map (range_ex 0 num_tables) ~f:(fun tbl_idx ->
             ( Expr.value (tbl_idx, tblsize) %=% Var ("table_id", tblsize)
             , mk_ith_table sz num_tables tbl_idx num_xs num_ms )) ]

let breadth params sz max_tables nheaders max_edits =
  let open Cmd in
  let physical_tables =
    List.fold (range_ex 1 max_tables) ~init:[] ~f:(fun acc ntables ->
        let fvs = ("out", 9) :: mk_normal_keys sz nheaders in
        let logical_table =
          apply
            ( "logical"
            , mk_normal_keys sz nheaders
            , [("action", [("o", 9)], "out" %<-% Var ("o", 9))]
            , Skip )
        in
        let phys = create_par_bench sz ntables nheaders 0 in
        Printf.printf "phys \n%s\n%!" (Cmd.to_string phys) ;
        let log_edits =
          create_log_edits 32 0 max_edits nheaders |> List.map ~f:List.return
        in
        let problem =
          Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[]
            ~log_inst:Instance.empty ~phys_inst:Instance.empty
        in
        acc @ [(ntables, problem, log_edits)])
  in
  List.fold physical_tables ~init:"ntables,time"
    ~f:(fun acc (num_xs, problem, log_edits) ->
      Synthesis.edit_cache := EAbstr.make () ;
      let st = Time.now () in
      let es = measure (restart_timer params) None problem log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None -> Printf.sprintf "%s\n%d,TIMEOUT" acc num_xs
      | Some _ -> Printf.sprintf "%s\n%d,%f" acc num_xs (Time.Span.to_ms dur))
  |> Printf.printf "%s\n"
