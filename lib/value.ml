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
