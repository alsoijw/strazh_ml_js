open Value

let parse code =
  (Parser_flow.program code |> fst |> snd).statements

let ast_walk debug v2v a =
  if debug then Flow_ast.Statement.show_t' Loc.pp Loc.pp (snd a) |> print_endline;
  match a |> snd with
  | Flow_ast.Statement.Expression c -> (
      match snd c.expression with
      | Flow_ast.Expression.Assignment d -> (
          match snd d.left with
          | Flow_ast.Pattern.Identifier e -> (
              match snd d.right with
              | Flow_ast.Expression.Identifier f -> (
                  match Hashtbl.find_opt v2v (snd f).name with
                  | Some g -> g
                  (* FIXME undefinded var *)
                  | None -> value_new())
              | _ -> value_new()
            ) |> Hashtbl.replace v2v (e.name |> snd).name;
          | _ -> ())
      | _ -> ())
  | _ -> ();
    if debug then show_v2v v2v
