open Core
(* Timing data : lots of subsets, eq time in check valid time in impl time  *)
type t = {
    log_inst_size : int;
    phys_inst_size : int;
    time : Time.Span.t;
    impl_time : Time.Span.t;
    check_valid_time : Time.Span.t;
    eq_time : Time.Span.t;
    make_vc_time: Time.Span.t;
    eq_num_z3_calls : int;
    model_search_time : Time.Span.t;
    cand_time : Time.Span.t;
    model_z3_time : Time.Span.t;
    model_cond_time : Time.Span.t;
    model_holes_time : Time.Span.t;
    interp_time : Time.Span.t;
    model_z3_calls: int;
    fixup_time : Time.Span.t;
    search_wp_time: Time.Span.t;
    tree_sizes: int list;
  }


let headers =
  ["log_inst_size";
   "phys_inst_size";
   "time";
   "impl_time";
   "check_valid_time";
   "eq_time";
   "make_vc_time";
   "eq_num_z3_calls";
   "model_search_time";
   "cand_time";
   "model_z3_time";
   "model_cond_time";
   "model_holes_time";
   "interp_time";
   "model_z3_calls";
   "fixup_time";
   "search_wp_time";
   "mean_tree_size";
   "min_tree_size";
   "max_tree_size"]


let header_string =
  List.reduce_exn headers ~f:(fun x y -> x ^ "," ^ y)

let mean_tree_size data =
  if List.length data.tree_sizes = 0 then 0 else List.fold (data.tree_sizes) ~init:0 ~f:((+)) / List.length (data.tree_sizes)
let max_tree_size data = List.fold (data.tree_sizes) ~init:0 ~f:(max)
let min_tree_size data = List.fold (data.tree_sizes) ~init:(max_tree_size data) ~f:(min)


let to_string (data : t) =
  Printf.sprintf "%d,%f,%f,%f,%f,%f,%d,%f,%f,%f,%f,%f,%f"
    data.log_inst_size
    (* data.phys_inst_size *)
    (data.time |> Time.Span.to_ms)
    (data.impl_time |> Time.Span.to_ms)
    (data.check_valid_time |> Time.Span.to_ms)
    (data.eq_time |> Time.Span.to_ms)
    (data.make_vc_time |> Time.Span.to_ms)
    data.eq_num_z3_calls
    (data.model_search_time |> Time.Span.to_ms)
    (data.cand_time |> Time.Span.to_ms)
    (data.model_z3_time |> Time.Span.to_ms)
    (data.model_cond_time |> Time.Span.to_ms)
    (data.model_holes_time |> Time.Span.to_ms)
    (data.interp_time |> Time.Span.to_ms)
    (* data.model_z3_calls *)
    (* (data.fixup_time |> Time.Span.to_ms)
     * (data.search_wp_time |> Time.Span.to_ms)
     * (mean_tree_size data)
     * (min_tree_size data)
     * (max_tree_size data) *)


let to_csv (dataset : t list) =
  Printf.sprintf "%s\n%s" header_string
    (List.fold dataset ~init:"" ~f:(fun acc dataline -> Printf.sprintf "%s%s\n" acc (to_string dataline)))

let zero _ : t ref =
  ref { log_inst_size = 0;
        phys_inst_size = 0;
        time = Time.Span.zero;
        impl_time = Time.Span.zero;
        check_valid_time = Time.Span.zero;
        eq_time = Time.Span.zero;
        make_vc_time = Time.Span.zero;
        eq_num_z3_calls = 0;
        model_search_time = Time.Span.zero;
        cand_time = Time.Span.zero;
        model_z3_time = Time.Span.zero;
        model_cond_time = Time.Span.zero;
        model_holes_time = Time.Span.zero;
        interp_time = Time.Span.zero;
        model_z3_calls = 0;
        fixup_time = Time.Span.zero;
        search_wp_time = Time.Span.zero;
        tree_sizes = [];
    }
