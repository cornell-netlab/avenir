open Core
open Async
open Cohttp_async
open Runtime
(* module Server = Cohttp_async.Server *)

let params = ref Parameters.default
let problem = ref Problem.empty

type op = Add of string * string list * string list * int
        | Delete of string * int

type avenir_request =  Op of op | Verify of op list * op list

let op_to_json (op : op) : Yojson.Basic.t =
  match op with
  | Add (table, matches, action_data, action_id) ->
    `Assoc [("opCode", `String ("ADD"));
            ("tableID", `String (table));
            ("matches", `List (List.map matches ~f:(fun x -> `String (x))));
            ("actionData", `List (List.map action_data ~f:(fun x -> `String (x))));
             ("actionID", `Int (action_id))]
  | Delete (table, row) ->
    `Assoc [("opCode", `String ("DELETE"));
            ("tableID", `String (table));
            ("row", `Int (row))]


let null_check message json  =
  match json with
    | `Null -> raise (Failure message)
    | _ -> json

let member_not_null field json =
  let open Yojson.Basic.Util in
  (member field json)
  |> null_check (sprintf "Missing field: '%s'" field)

let parse_add_request json =
  let open Yojson.Basic.Util in
  let table = json |> member_not_null "tableID" |> to_string in
  let matches = json |> member_not_null "matches" |> to_list
                |> List.map ~f:to_string in
  let actionData = json |> member_not_null "actionData" |> to_list
                |> List.map ~f:to_string in
  let actionId = json |> member_not_null "actionID" |> to_int in
    Add (table, matches, actionData, actionId)


let parse_delete_request json =
  let open Yojson.Basic.Util in
  let table = json |> member_not_null "tableID" |> to_string in
  let row = json |> member_not_null "row" |> to_int in
    Delete (table,row)


let parse_verify_request json =
    let open Yojson.Basic.Util in

    let parse_op op = (match (op |> member_not_null "opCode" |> to_string) with
                       | "ADD" -> parse_add_request op
                       | "DELETE" -> parse_delete_request op
                       | _ -> failwith "Something went wrong while parsing verify list") in
    let abstract_ops = json |> member "abstract_ops" |> to_list |> List.map ~f:parse_op in
    let physical_ops = json |> member "physical_ops" |> to_list |> List.map ~f:parse_op in
      Verify (abstract_ops, physical_ops)


let parse_json body_string =
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_string body_string in
  let opcode = json |> member_not_null "opCode" |> to_string in
  match opcode with
    | "ADD" -> Op (parse_add_request json)
    | "DELETE" -> Op (parse_delete_request json)
    | "VERIFY" -> parse_verify_request json
    | _ -> failwith "Malformed opcode"



let edit_of_op prog op =
  match op with
  | Add (table, matches, actionData, actId) ->
     begin match Ast.get_schema_of_table table prog with
     | None -> failwith @@ Printf.sprintf "unrecognized table %s" table
     | Some (keys,_,_) ->
        let ks = List.map keys ~f:(fun (k,sz,_) -> (k,sz)) in
        Edit.Add(table,
                 (matches_of_string ks @@ String.concat ~sep:";" matches,
                  action_data_of_string @@ String.concat ~sep:";" actionData,
                  actId))
     end
  | Delete (table, rowId) -> Edit.Del(table, rowId)

let op_of_edit e =
  match e with
  | Edit.Add (table, (matches, actionData, actId)) ->
     Add (table
        , List.map ~f:(Match.to_string) matches
        , List.map ~f:(Ast.string_of_value) actionData
        , actId)
  | Edit.Del(table, rowId) ->  Delete (table, rowId)


let avenir_stub op : op list =
  let e = edit_of_op (Problem.log !problem) op in
  problem := Problem.replace_log_edits !(problem) [e];
  let es = Synthesis.cegis_math
             !(params)
             (ProfData.zero ())
             !problem in
  match es with
  | None ->
     failwith "Synthesis returned None"
  | Some es ->
     problem := Problem.commit_edits_log !params !problem;
     problem := Problem.replace_phys_edits !problem es
                |> Problem.commit_edits_phys !params;
     List.map es ~f:op_of_edit

let verify_stub abstract_ops physical_ops =
  let problem_ =
    Problem.append_log_edits !problem
    @@ List.map abstract_ops ~f:(edit_of_op (Problem.log !problem)) in
  let problem_ =
    Problem.append_phys_edits problem_
    @@ List.map physical_ops ~f:(edit_of_op (Problem.phys !problem)) in
  match Synthesis.implements !params (ProfData.zero ()) problem_ with
  | `Yes -> true
  | _ -> false


let handle_op_request (op : op) =
  (* some magic occurs *)
  let ops = avenir_stub op
            |> List.map ~f:op_to_json in
  `List (ops)

let process_post_request body =
try
  let parsed_request = parse_json body in
    match parsed_request with
    | Op (op) ->
    `Assoc [
      ("PhysOps", handle_op_request op);
      ("OK", `Bool (true));
      ("Error", `String ("")) ]
      |> Yojson.Basic.to_string ~std:true
    | Verify (absOps, physOps) ->
    `Assoc [
      ("PhysOps", `List []);
      ("OK", `Bool (verify_stub absOps physOps));
      ("Error", `String ("")) ]
      |> Yojson.Basic.to_string ~std:true

with Failure (e) ->
    `Assoc [
      ("PhysOps", `List []);
      ("OK", `Bool (false));
      ("Error", `String (e)) ]
      |> Yojson.Basic.to_string ~std:true

let handle_request
  ~(body : Cohttp_async.Body.t)
   (_ : Socket.Address.Inet.t)
   (request : Request.t) : Server.response Deferred.t =
   match request.meth with
   | `POST ->
          (Body.to_string body) >>= (fun body ->
          printf "Body: %s\n" body;
          let response_string = process_post_request body in
          let headers = Cohttp.Header.init () in
          Cohttp_async.Server.respond_string ~headers response_string)
   | _ -> printf "Malformed request from cilent\n";
          Cohttp_async.Server.respond `Not_found


let log_error _ exn =
    printf "Error: %s \n" (Exn.to_string exn)



let listen port () =
  Cohttp_async.Server.create
    ~on_handler_error:(`Call (log_error))
    (Tcp.Where_to_listen.of_port port)
    (handle_request) >>= fun _serv ->
    Deferred.never ()


let runserver params_ problem_ () =
  params := params_;
  problem := problem_;
  listen 19900 ()
