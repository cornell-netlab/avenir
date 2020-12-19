open Core
open Ast
       
let check_prop prog edit_props_opt prop =  
  Option.bind edit_props_opt ~f:(fun (edit,props) ->
      match FSM.step prop prog edit with
      | Category.Accept prop' | Category.Unknown prop' ->
         Some (edit, props@[prop'])
      | Category.Reject _ ->
         None
    )
       
let check_props (prog : cmd) (edit : Tables.Edit.t) =
  let prop_list = !Props.props in
  let prop_list' : (Tables.Edit.t * FSM.t list) option =
    List.fold prop_list ~init:(Some (edit,[])) ~f:(check_prop prog)
  in
  Props.props := Option.value_map prop_list' ~f:snd ~default:prop_list;
  prop_list'
