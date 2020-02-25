open Core

type t = {
    log_inst_size : int;
    phys_inst_size : int;
    time : Time.Span.t;
    eq_time : Time.Span.t;
    eq_num_z3_calls : int;
    model_search_time : Time.Span.t;
    model_z3_time : Time.Span.t;
    model_z3_calls: int;
    search_wp_time: Time.Span.t;
    make_vc_time: Time.Span.t;
    tree_sizes: int list;
  }


let headers =
  ["log_inst_size";
   "phys_inst_size";
   "time";
   "eq_time";
   "eq_num_z3_calls";
   "model_search_time";
   "model_z3_time";
   "model_z3_calls";
   "search_wp_time";
   "make_vc_time";
   "mean_tree_size";
   "min_tree_size";
   "max_tree_size"]


let header_string =
  List.reduce_exn headers ~f:(fun x y -> x ^ "," ^ y)

let mean_tree_size data = List.fold (data.tree_sizes) ~init:0 ~f:((+)) / List.length (data.tree_sizes)
let max_tree_size data = List.fold (data.tree_sizes) ~init:0 ~f:(max) 
let min_tree_size data = List.fold (data.tree_sizes) ~init:(max_tree_size data) ~f:(min)
    
    
let to_string (data : t) =
  Printf.sprintf "%d,%d,%f,%f,%d,%f,%f,%d,%f,%f,%d,%d,%d"
    data.log_inst_size
    data.phys_inst_size
    (data.time |> Time.Span.to_ms)
    (data.eq_time |> Time.Span.to_ms)
    data.eq_num_z3_calls
    (data.model_search_time |> Time.Span.to_ms)
    (data.model_z3_time |> Time.Span.to_ms)
    data.model_z3_calls
    (data.search_wp_time |> Time.Span.to_ms)
    (data.make_vc_time |> Time.Span.to_ms)
    (mean_tree_size data)
    (min_tree_size data)
    (max_tree_size data)
    

let to_csv (dataset : t list) =
  Printf.sprintf "%s\n%s" header_string
    (List.fold dataset ~init:"" ~f:(fun acc dataline -> Printf.sprintf "%s%s\n" acc (to_string dataline)))
  
let zero _ : t ref =
  ref { log_inst_size = 0;
        phys_inst_size = 0;
        time = Time.Span.zero;
        eq_time = Time.Span.zero;
        eq_num_z3_calls = 0;
        model_search_time = Time.Span.zero;
        model_z3_time = Time.Span.zero;
        model_z3_calls = 0;
        search_wp_time = Time.Span.zero;
        make_vc_time = Time.Span.zero;
        tree_sizes = [];
    }
    
