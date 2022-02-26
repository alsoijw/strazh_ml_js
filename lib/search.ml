open Loc
open Ast_walk

let process ?(debug = false) str =
  let code = Helpers.parse str in
  let pp b = Flow_ast.Statement.show_t' Loc.pp Loc.pp (snd b) |> print_endline in
  if debug then (
    List.iter pp code;
    List.length code |> Printf.printf "%d\n");
  let partial fn = function
    | Flow_ast.Expression.Member.PropertyIdentifier _ -> fn
    | Flow_ast.Expression.Member.PropertyExpression (_, a) -> (match a with
        | Flow_ast.Expression.Literal _ -> fn
        | _ -> [])
    | _ -> [] in
  let rec safe = fun _ -> function
    | Flow_ast.Expression.Member c -> (match snd c._object with
        | Flow_ast.Expression.Identifier (d, e) -> partial (condition d e) c.property
        | Flow_ast.Expression.This _ -> partial [fst c._object] c.property
        | _ -> [])
    | _ -> []
  and possible_unsafe = fun i -> function
    | Flow_ast.Expression.This _ -> [fst i]
    | Flow_ast.Expression.Identifier (a, b) -> condition a b
    | _ -> []
  and condition a b =
    if List.exists (fun c -> c = b.name) ["globalThis"; "window"; "self"; "frames"; "global"] then [a] else [] in

  let safe = {
    expression2loc = safe;
    pattern_tail = false } in
  let possible_unsafe = {
    expression2loc = possible_unsafe;
    pattern_tail = false } in
  let possible_unsafe_list =
    List.map (fun i -> Ast_walk.ast_walk possible_unsafe i) code
    |> List.flatten |> List.flatten
  in
  if false then List.iter (fun i -> Loc.show i |> print_endline) possible_unsafe_list;
  let safe_list =
    List.map (fun i -> Ast_walk.ast_walk safe i) code
    |> List.flatten |> List.flatten
  in
  Helpers.exclude possible_unsafe_list safe_list
