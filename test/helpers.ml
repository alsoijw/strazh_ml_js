open Lib.Ast_walk
open Lib.Value
open Lib.Types
open Lib

let try_test v2v debug code =
  parse code |> ast_walk1 debug v2v

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
  func_new @@ vn Value |> V2v.set v2v "_";
  vn Value v2v [] |> V2v.set v2v "b";
  v2v

let test_if =
  fun () ->
  let counter = ref 0 in
  let next_val =
    fun () ->
      let c = !counter in
      incr counter;
      c in
  let v2v = V2v.variable2value_new() in
  let vn vtype v2v a = value_new v2v vtype a in
  let func_new f =
    value_new v2v (Callable f) [] in
  func_new (fun v2v a -> value_new v2v (Numbered (next_val())) a)
  |> V2v.set v2v "_";
  vn Value v2v [] |> V2v.set v2v "b";
  v2v

let numb2list v2v =
  (V2v.find v2v "a").bases_on |>
  List.map (fun i -> match i.kind with
      | Numbered i -> i
      | _ -> -1)
