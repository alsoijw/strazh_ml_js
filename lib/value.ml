open Types

let counter = ref 0
let next_val = 
  fun () ->
  incr counter;
  !counter

let flatten i =
  let o = ref [] in
  List.iter (fun i -> 
      o := List.append !o (match i.kind with
          | Union -> i.bases_on;
          | _ -> [ i ])
    ) i;
  !o

let value_new =
  fun v2v kind bases_on ->
  let bases_on = flatten bases_on in
  let corrupted =
    match kind with
    | Types.Db -> (
        List.exists (fun a ->
            match a.kind with
            | Types.Raw -> true
            | _ -> false
          ) bases_on)
    | _ -> false in
  let this = { id = next_val(); kind = kind; bases_on = bases_on } in
  if corrupted then 
    v2v.corrupted <- List.append v2v.corrupted [ this ];
  this

let type_blacklist v _type =
  match v.kind with
  | Types.Union -> (
      List.exists (fun a -> a.kind = _type) v.bases_on)
  | _ -> v.kind = _type

let type_db_blacklist v =
  type_blacklist v Types.Raw
