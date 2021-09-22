open Types

let variable2value_new =
  fun () ->
  let (c : value list) = [] in
  let (r : value list) = [] in
  { variables = Hashtbl.create 0
  ; parrent = None
  ; corrupted = c
  ; return = r
  }

let wrap v2v =
  fun () ->
  let (c : value list) = [] in
  let (r : value list) = [] in
  { variables = Hashtbl.create 0
  ; parrent = Some v2v
  ; corrupted = (v2v.corrupted)
  ; return = r
  }

let show_v2v0 var2val =
  Hashtbl.iter (fun a b -> print_endline (a ^ " " ^ (Types.show_value b))) var2val.variables;
  print_endline "---"

let set v2v key value =
  Hashtbl.replace v2v.variables key value

let find v2v key =
  Hashtbl.find v2v.variables key

let rec find_opt v2v key =
  match Hashtbl.find_opt v2v.variables key with
  | Some v -> Some v
  | None ->
    match v2v.parrent with
    | Some v2v -> find_opt v2v key
    | None -> None
