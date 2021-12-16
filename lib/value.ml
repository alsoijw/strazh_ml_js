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
  fun kind bases_on ->
  let bases_on = flatten bases_on in
  { id = next_val(); kind = kind; bases_on = bases_on }

let type_blacklist _type v =
  match v.kind with
  | Types.Union -> (
      List.exists (fun a -> a.kind = _type) v.bases_on)
  | _ -> v.kind = _type

let type_db_blacklist v =
  type_blacklist Types.Raw v
