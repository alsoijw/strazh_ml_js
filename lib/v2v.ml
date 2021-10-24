open Types

let variable2value_new =
  fun () ->
  let (r : value list) = [] in
  let (m : mismatch list) = [] in
  { variables = Hashtbl.create 0
  ; constrait = Hashtbl.create 0
  ; parrent = None
  ; mismatches = { list = m }
  ; return = r
  }

let wrap v2v =
  fun () ->
  let (r : value list) = [] in
  { variables = Hashtbl.create 0
  ; constrait = Hashtbl.create 0
  ; parrent = Some v2v
  ; mismatches = v2v.mismatches
  ; return = r
  }

let show_v2v0 var2val =
  Hashtbl.iter (fun a b -> print_endline (a ^ " " ^ (Loc.show_position @@ fst b) ^ " " ^ (Types.show_value @@ snd b))) var2val.variables;
  print_endline "---"

let set ?(merge = false) v2v key value =
  List.iter (fun i ->
      Hashtbl.iter (fun k v ->
          let flag = (Loc.pos_cmp k (fst value) > 0 || merge) && v @@ snd value in
          if flag then { value = snd value; set = fst value; usage = k } |> Vector.append v2v.mismatches
        ) i) @@ Hashtbl.find_all v2v.constrait key;
  Hashtbl.replace v2v.variables key value

let find v2v key =
  Hashtbl.find v2v.variables key |> snd

let rec find_opt v2v key =
  match Hashtbl.find_opt v2v.variables key with
  | Some v -> Some (snd v)
  | None ->
    match v2v.parrent with
    | Some v2v -> find_opt v2v key
    | None -> None

let constrait_set v2v name position value =
  Hashtbl.add
    (match Hashtbl.find_opt v2v.constrait name with
     | Some v -> v
     | None -> let c = Hashtbl.create 1 in
       Hashtbl.add v2v.constrait name c;
       c)
    position value;
  match Hashtbl.find_opt v2v.variables name with
  | Some v -> if value @@ snd v then Vector.append v2v.mismatches { value = snd v; set = fst v; usage = position }
  | _ -> ()
