open Types

let variable2value_new =
  fun () ->
  let (c : value list) = [] in
  { variables = Hashtbl.create 0
  ; corrupted = c
  }

let show_v2v0 var2val =
  Hashtbl.iter (fun a b -> print_endline (a ^ " " ^ (Types.show_value b))) var2val.variables;
  print_endline "---"

let set v2v key value =
  Hashtbl.replace v2v.variables key value

let find v2v key =
  Hashtbl.find v2v.variables key

let find_opt v2v key =
  Hashtbl.find_opt v2v.variables key
