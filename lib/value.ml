open Types

let counter = ref 0
let next_val = 
  fun () ->
  incr counter;
  !counter

let value_new =
  fun v2v kind bases_on ->
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
