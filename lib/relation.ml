module Hashtbl = struct
  include Hashtbl

  let pp pp_key pp_value ppf values =
    Hashtbl.iter (fun a b -> Format.fprintf ppf "  @[<1>%a: %a@]@." pp_key a pp_value b) values;
end

type r_type = Get of string
            | Call of r_type * r_type list
            | Set of string * r_type
            | Prop of r_type * r_type
            | PropName of r_type * string
            | LStr of string
            | LReg of string
            | LBool of bool
            | LNum of float
            | LArr of r_type list
            | New of r_type * r_type list
            | Spread of r_type
            | Lambda of string list * r_type
            | Unmatched
and relation = Rel of r_type * Loc.t
[@@deriving show]

let rec ast2val = function
  | Flow_ast.Expression.Identifier (_, f) ->
    Get f.name
  | Flow_ast.Expression.Call f ->
    Call (a2v f.callee, List.map ast2val1 (snd f.arguments).arguments)
    (*
      Call (a2v f.callee :: List.map ast2val1 (snd f.arguments).arguments)
       *)
  | Flow_ast.Expression.Assignment d -> (
      match snd d.left with
      | Flow_ast.Pattern.Identifier e ->
        Set ((e.name |> snd).name, a2v d.right)
      | _ -> Unmatched)

  | Flow_ast.Expression.Member d -> (match d.property with
      | Flow_ast.Expression.Member.PropertyIdentifier (_, f) -> PropName (a2v d._object, f.name)
      | Flow_ast.Expression.Member.PropertyExpression f -> (match a2v f with
          | LStr g -> PropName (a2v d._object, g)
          | g -> Prop (a2v d._object, g))
      | _ -> Unmatched)

    | Flow_ast.Expression.Literal g ->
      (match g.value with
       | Flow_ast.Literal.String h -> LStr h
       | Flow_ast.Literal.Number h -> LNum h
       | Flow_ast.Literal.RegExp _ -> LReg g.raw
       | _ -> Unmatched)

    | Flow_ast.Expression.Array d ->
      LArr (List.map (function
          | Flow_ast.Expression.Array.Expression e -> a2v e
          | Flow_ast.Expression.Array.Spread (_, e) -> a2v e.argument
          | _ -> Unmatched)
          d.elements)

    | Flow_ast.Expression.New d ->
      New (
        a2v d.callee,
        match d.arguments with
        | Some (_, v) -> List.map ast2val1 v.arguments
        | None -> [])
    | Flow_ast.Expression.ArrowFunction b ->
      Lambda (
        (let open Flow_ast.Function.Param in
         List.map (fun i -> match (snd i).argument |> snd with
             | Flow_ast.Pattern.Identifier c -> (snd c.name).name
             | _ -> ""
           ) (snd b.params).params),
        match b.body with
        | Flow_ast.Function.BodyExpression d -> a2v d
        | _ -> Unmatched
      )

    | _ -> Unmatched
and ast2val1 = function
  | Flow_ast.Expression.Expression (_, b) -> ast2val b
  | _ -> Unmatched
and a2v i = snd i |> ast2val

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

let intersection blocks =
  List.map (fun i ->
      List.map (fun j -> match j with
          | Set (n, _) -> Some n
          | _ -> None
        ) i |> Helpers.ext
    ) blocks

