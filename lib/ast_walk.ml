open Types

let parse code =
  (Parser_flow.program code |> fst |> snd).statements

let extract f i =
  if Array.length i > 0 then
    match f i.(0) with
    | Ok v -> (
        let o = Array.make (Array.length i) v in
        let break = ref false in
        let return = ref None in
        for n = 1 to Array.length i - 1 do
          if not (!break)
          then
            match f (i.(n)) with
            | Ok v -> o.(n) <- v
            | Error v -> break := true; return := Some v
        done;
        match !return with
        | None -> Ok o
        | Some v -> Error v
      )
    | Error v -> Error v
  else Ok [| |]

let rec ast2val v2v e =
  match e with

  | Flow_ast.Expression.Identifier f -> (
      match V2v.find_opt v2v (snd f).name with
      | Some g -> Ok g
      | None -> Error Undef)

  | Flow_ast.Expression.Call f -> ( 
      match snd f.callee with
      | Flow_ast.Expression.Identifier g -> (
          match (snd g).name |> V2v.find_opt v2v with
          | Some h -> (
              match h.kind with
              | Callable g -> (
                  let ast2val1_ = ast2val1 v2v in
                  match Array.of_list (snd f.arguments).arguments |> extract ast2val1_ with
                  | Ok t -> Ok (g v2v (Array.to_list t))
                  | Error v -> Error v)
              | _ -> Error IsNotCallable)
          | None -> Error Undef)
      | _ -> Error Undef)

  | Flow_ast.Expression.Assignment d -> (
      match snd d.left with
      | Flow_ast.Pattern.Identifier e -> (
          match ast2val v2v (snd d.right) with
          | Ok f -> V2v.set v2v (e.name |> snd).name f; Ok f
          | _ -> Error Undef)
      | _ -> Error Undef)

  | _ -> Error Undef
and ast2val1 v2v a =
  match a with
  | Flow_ast.Expression.Expression b -> ast2val v2v (snd b)
  | _ -> Error Undef

let ast_walk debug v2v a =
  let b = snd a in
  if debug then Flow_ast.Statement.show_t' Loc.pp Loc.pp b |> print_endline;
  match b with
  | Flow_ast.Statement.Expression c -> let _ = ast2val v2v (snd c.expression) in ()
  | _ -> ();
    if debug then Types.show_variable2value v2v |> print_endline
