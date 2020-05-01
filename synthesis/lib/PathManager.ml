open Core
open Util
open Ast


type t = {
    hole_paths : test list;
    holeless_paths : test list;
  }

let empty = {hole_paths = []; holeless_paths = []}
let get_hole_paths (m : t) = m.hole_paths
let add_holeless_path (m : t) phi = {m with holeless_paths = phi :: m.hole_paths}
let add_hole_path (m : t) phi = {m with hole_paths = phi :: m.hole_paths}
let singleton_holey phi = {hole_paths = [phi]; holeless_paths = []}
let singleton_holeless phi = {hole_paths = []; holeless_paths = [phi]}
let union (pm : t) (pm' : t) =
  { hole_paths = pm.hole_paths @ pm'.hole_paths;
    holeless_paths = pm.holeless_paths @ pm'.holeless_paths }

let map ~f:(f : test -> test) tag (pm : t) : t =
  match tag with
  | `Stay ->
     {
       hole_paths = List.map pm.hole_paths ~f;
       holeless_paths = List.map pm.holeless_paths ~f;
     }
  | `Hole ->
     {
       hole_paths = List.map pm.hole_paths ~f @ List.map pm.holeless_paths ~f;
       holeless_paths = []
     }
  | `Holeless ->
     {
       hole_paths = [];
       holeless_paths = List.map pm.hole_paths ~f @ List.map pm.holeless_paths ~f
     }
