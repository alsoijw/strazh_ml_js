module Hashtbl = struct
  include Hashtbl

  let pp pp_key pp_value ppf values =
    Hashtbl.iter (fun a b -> Format.fprintf ppf "  @[<1>%a: %a@]@." pp_key a pp_value b) values;
end

(*
type r_key = Val | Key | Nkey of string
and r_type = Link | Call
and relation_list = Relation of relation
                  | RList of relation list
and relation = Rel of r_type * Loc.t * relation_list * (r_key, relation) Hashtbl.t
   *)
type r_type = Get of string | Call of r_type list | Set of string * r_type | Unmatched
and relation = Rel of r_type * Loc.t
[@@deriving show]

let rec ast2val = function
  | Flow_ast.Expression.Identifier f ->
    Get (snd f).name
  | Flow_ast.Expression.Call f ->
    Call ((snd f.callee |> ast2val) :: List.map ast2val1 (snd f.arguments).arguments)
  | Flow_ast.Expression.Assignment d -> (
      match snd d.left with
      | Flow_ast.Pattern.Identifier e ->
        Set ((e.name |> snd).name, snd d.right |> ast2val)
      | _ -> Unmatched)

  | _ -> Unmatched
and ast2val1 = function
  | Flow_ast.Expression.Expression b -> ast2val (snd b)
  | _ -> Unmatched

let rec statement2relation debug a =
  let b = snd a in
  if debug > 1 then Flow_ast.Statement.show_t' Loc.pp Loc.pp b |> print_endline;
  match b with
  | Flow_ast.Statement.Expression c -> ast2val (snd c.expression)
  | Flow_ast.Statement.If c ->
    Unmatched
  | Flow_ast.Statement.Block c ->
    Unmatched
  | Flow_ast.Statement.Return d ->
    Unmatched
  | Flow_ast.Statement.VariableDeclaration d ->
    Unmatched
  | _ ->
    Unmatched
