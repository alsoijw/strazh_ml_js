let counter = ref 0
let next_val = 
  fun () ->
  incr counter;
  !counter

type value = {
  id : int
}
[@@deriving show]

let value_new =
  fun () ->
  { id = next_val() }

let show_v2v var2val =
  Hashtbl.iter (fun a b -> print_endline (a ^ " " ^ (show_value b))) var2val;
  print_endline "---"

