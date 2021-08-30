open Lib.Ast_walk
open Lib.Value

let try_test var2val debug code setup =
  let v2v = match var2val with
    | Some v -> v
    | None -> Hashtbl.create 0 in
  let aw = ast_walk (debug > 1) v2v in

  setup v2v;

  if debug > 0 then show_v2v v2v;
  parse code |> List.iter aw;
  if debug > 0 then show_v2v v2v;

  v2v

