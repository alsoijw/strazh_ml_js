open Lib.Ast_walk
open Lib.Value
open Lib

let try_test v2v debug code =
  let aw = ast_walk (debug > 1) v2v in

  if debug > 0 then Types.show_variable2value v2v |> print_endline;
  parse code |> List.iter aw;
  if debug > 0 then Types.show_variable2value v2v |> print_endline;
  ()

let setup var2val func =
  let v2v = match var2val with
    | Some v -> v
    | None -> V2v.variable2value_new() in
  func v2v;
  v2v

let test_func =
  fun () ->
  let v2v = V2v.variable2value_new() in
  let vn vtype v2v a = value_new v2v vtype a in
  let func_new f =
    value_new v2v (Callable f) [] in
  func_new @@ vn Raw |> V2v.set v2v "raw_data";
  func_new @@ vn Value |> V2v.set v2v "safe_data";
  func_new @@ vn Db |> V2v.set v2v "db_query";
  v2v
