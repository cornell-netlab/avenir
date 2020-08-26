open Core

(* Timing data : lots of subsets, eq time in check valid time in impl time  *)
type t = {
    log_inst_size : int ref;
    phys_inst_size : int ref;
    time : Time.Span.t ref;
    fast_cex_time : Time.Span.t ref;
    impl_time : Time.Span.t ref;
    check_valid_time : Time.Span.t ref;
    eq_time : Time.Span.t ref;
    make_vc_time: Time.Span.t ref;
    good_execs_time : Time.Span.t ref;
    ingress_egress_time : Time.Span.t ref;
    normalize_packet_time : Time.Span.t ref;
    check_sliceable_time : Time.Span.t ref;
    prefixing_time : Time.Span.t ref;
    eq_num_z3_calls : int ref;
    model_search_time : Time.Span.t ref;
    cand_time : Time.Span.t ref;
    model_z3_time : Time.Span.t ref;
    model_cond_time : Time.Span.t ref;
    model_holes_time : Time.Span.t ref;
    interp_time : Time.Span.t ref;
    model_z3_calls: int ref;
    fixup_time : Time.Span.t ref;
    search_wp_time: Time.Span.t ref;
    num_backtracks : int ref;
    tree_sizes: int list ref;
  }


let headers =
  ["log_inst_size";
   (* "phys_inst_size"; *)
   "time";
   "fast_cex_time";
   "impl_time";
   "check_valid_time";
   "normalize_packet_time";
   "eq_time";
   "make_vc_time";
   "good_execs_time";
   "prefixing_time";
   "ingress_packet_time";
   "check_sliceable_time";
   "eq_num_z3_calls";
   "model_search_time";
   "model_holes_time";
   "model_cond_time";
   "search_wp_time";
   "model_z3_time";
   "model_z3_calls";
   "num_backtracks"
(*;
   ( "model_cond_time";
    * "model_holes_time";
    * "interp_time";
    * "model_z3_calls";
    * "fixup_time";
    * "search_wp_time";
    * "mean_tree_size";
    * "min_tree_size";
    * "max_tree_size"*)]


let header_string =
  List.reduce_exn headers ~f:(fun x y -> x ^ "," ^ y)

let mean_tree_size data =
  if List.length !(data.tree_sizes) = 0
  then 0
  else List.fold !(data.tree_sizes) ~init:0 ~f:((+)) / List.length !(data.tree_sizes)
let max_tree_size data = List.fold !(data.tree_sizes) ~init:0 ~f:(max)
let min_tree_size data = List.fold !(data.tree_sizes) ~init:(max_tree_size data) ~f:(min)


let to_string (data : t) =
  Printf.sprintf "%d,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%d,%f,%f,%f,%f,%f,%d,%d"
    !(data.log_inst_size)
    (* data.phys_inst_size *)
    (!(data.time) |> Time.Span.to_ms)
    (!(data.fast_cex_time) |> Time.Span.to_ms)
    (!(data.impl_time) |> Time.Span.to_ms)
    (!(data.check_valid_time) |> Time.Span.to_ms)
    (!(data.normalize_packet_time) |> Time.Span.to_ms)
    (!(data.eq_time) |> Time.Span.to_ms)
    (!(data.make_vc_time) |> Time.Span.to_ms)
    (!(data.good_execs_time) |> Time.Span.to_ms)
    (!(data.ingress_egress_time) |> Time.Span.to_ms)
    (!(data.check_sliceable_time) |> Time.Span.to_ms)
    (!(data.prefixing_time) |> Time.Span.to_ms)
    !(data.eq_num_z3_calls)
    (!(data.model_search_time) |> Time.Span.to_ms)
    (!(data.model_holes_time) |> Time.Span.to_ms)
    (!(data.model_cond_time) |> Time.Span.to_ms)
    (!(data.search_wp_time) |> Time.Span.to_ms)
    (!(data.model_z3_time) |> Time.Span.to_ms)
    (!(data.model_z3_calls))
    (!(data.num_backtracks))


    (* (data.model_cond_time |> Time.Span.to_ms)
     *
     * (data.interp_time |> Time.Span.to_ms) *)
    (* (data.fixup_time |> Time.Span.to_ms)
     *
     * (mean_tree_size data)
     * (min_tree_size data)
     * (max_tree_size data) *)


let to_csv (dataset : t list) =
  Printf.sprintf "%s\n%s" header_string
    (List.fold dataset ~init:"" ~f:(fun acc dataline -> Printf.sprintf "%s%s\n" acc (to_string dataline)))

let zero _ : t ref =
  ref { log_inst_size = ref 0;
        phys_inst_size = ref 0;
        time = ref Time.Span.zero;
        fast_cex_time = ref Time.Span.zero;
        impl_time = ref Time.Span.zero;
        check_valid_time = ref Time.Span.zero;
        eq_time = ref Time.Span.zero;
        normalize_packet_time = ref Time.Span.zero;
        make_vc_time = ref Time.Span.zero;
        good_execs_time = ref Time.Span.zero;
        ingress_egress_time = ref Time.Span.zero;
        prefixing_time = ref Time.Span.zero;
        check_sliceable_time = ref Time.Span.zero;
        eq_num_z3_calls = ref 0;
        model_search_time = ref Time.Span.zero;
        cand_time = ref Time.Span.zero;
        model_z3_time =  ref Time.Span.zero;
        model_cond_time = ref Time.Span.zero;
        model_holes_time = ref Time.Span.zero;
        interp_time = ref Time.Span.zero;
        model_z3_calls = ref 0;
        fixup_time = ref Time.Span.zero;
        search_wp_time = ref Time.Span.zero;
        num_backtracks = ref 0;
        tree_sizes = ref [];
    }

let update_time_val acc time =
  let open Time in
  let open Span in
  acc := !acc + time

let update_time acc ?end_time:(end_time = Time.now()) start_time =
  let open Time in
  diff end_time start_time
  |> update_time_val acc

let incr acc = acc := !acc + 1
