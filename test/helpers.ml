open Lib.Ast_walk
open Lib.Value
open Lib.Types
open Lib
open Loc

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
  let fn vtype = fun _ b c -> vn vtype b c in
  let func_new f =
    value_new v2v (Callable f) [] in
  let set name value = V2v.set v2v name (Loc.none.start, value) in
  func_new @@ fn Raw |> set "raw_data";
  func_new @@ fn Value |> set "safe_data";
  func_new (fun aa v2v bases_on ->
      List.iteri (fun n i ->
          match i with
          | Flow_ast.Expression.Expression exp -> (
              match snd exp with
              | Flow_ast.Expression.Identifier name ->
                V2v.constrait_set v2v (snd name).name (fst name).start Value.type_db_blacklist;
              | _ -> (
                  match List.nth_opt bases_on n with
                  | Some v -> if Value.type_db_blacklist v then
                      Vector.append v2v.mismatches v
                  | _ -> ()
                )
            )
          | _ -> ()) aa;
      vn Db v2v bases_on) |> set "db_query";
  func_new @@ fn Value |> set "_";
  vn Value v2v [] |> set "b";
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
  let set name value = V2v.set v2v name (Loc.none.start, value) in
  func_new (fun _ v2v a -> value_new v2v (Numbered (next_val())) a)
  |> set "_";
  vn Value v2v [] |> set "b";
  v2v

let numb2list v2v =
  (V2v.find v2v "a").bases_on |>
  List.map (fun i -> match i.kind with
      | Numbered i -> i
      | _ -> -1)
