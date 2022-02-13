open Loc

type slice = {
  begin_ : int;
  end_ : int;
}
[@@deriving show]

type leaf =
  | Leaf of slice * string
  | Tree of slice * tree list
and tree = leaf ref
[@@deriving show]

let rec array_pattern2loc = function
  | Flow_ast.Pattern.Array.Element (_, a) -> pattern2loc a.argument @ expression2loc_opt a.default
  | Flow_ast.Pattern.Array.RestElement (_, a) -> pattern2loc a.argument
  | Flow_ast.Pattern.Array.Hole _ -> []
and pattern2loc i = match snd i with
  | Flow_ast.Pattern.Expression e -> expression2loc_tail e
  | Flow_ast.Pattern.Identifier _ -> []
  | Flow_ast.Pattern.Array a -> List.map array_pattern2loc a.elements |> List.flatten
  | Flow_ast.Pattern.Object a -> List.map (fun i -> match i with
      | Flow_ast.Pattern.Object.Property (_, b) ->
        (match b.key with
         | Flow_ast.Pattern.Object.Property.Literal _ -> []
         | Flow_ast.Pattern.Object.Property.Identifier _ -> []
         | Flow_ast.Pattern.Object.Property.Computed (_, b) -> expression2loc b.expression) @
        pattern2loc b.pattern @ expression2loc_opt b.default
      | Flow_ast.Pattern.Object.RestElement (_, b) -> pattern2loc b.argument
    ) a.properties |> List.flatten
and expression2loc i = match snd i with
  | Flow_ast.Expression.Array a -> List.map (function
      | Flow_ast.Expression.Array.Expression b -> expression2loc b
      | Flow_ast.Expression.Array.Spread (_, b) -> let open Flow_ast.Expression.SpreadElement in
        expression2loc b.argument
      | Flow_ast.Expression.Array.Hole _ -> []
    ) a.elements |> List.flatten
  | Flow_ast.Expression.ArrowFunction a -> []
  | Flow_ast.Expression.Assignment d -> pattern2loc d.left @ expression2loc d.right
  | Flow_ast.Expression.Binary a -> expression2loc a.left @ expression2loc a.right
  | Flow_ast.Expression.Member d -> (match d.property with
      | Flow_ast.Expression.Member.PropertyIdentifier _ -> []
      | Flow_ast.Expression.Member.PropertyPrivateName _ -> []
      | Flow_ast.Expression.Member.PropertyExpression f ->
        [
          (fst d._object).start;
          (fst d._object)._end;
          (fst f).start;
          (fst f)._end;
          (fst i)._end;
        ] :: expression2loc d._object
        @ expression2loc f)
  | Flow_ast.Expression.This _ -> []
  | Flow_ast.Expression.TypeCast a -> expression2loc a.expression
  | Flow_ast.Expression.Unary b -> (match b.operator with
      | Flow_ast.Expression.Unary.Delete -> expression2loc_tail b.argument
      | _ -> expression2loc b.argument)
  | Flow_ast.Expression.Update b -> expression2loc_tail b.argument
  | Flow_ast.Expression.Yield a -> expression2loc_opt a.argument
  | __ -> []
and expression2loc_tail i = match expression2loc i with
  | [] -> []
  | _ :: tl -> tl
and expression2loc_opt = function
  | Some b -> expression2loc b
  | None -> []
and statement2loc = function
  | Flow_ast.Statement.Expression c -> expression2loc c.expression
  | Flow_ast.Statement.VariableDeclaration a -> List.map (fun (_, i) ->
      let open Flow_ast.Statement.VariableDeclaration.Declarator in
      pattern2loc i.id @ expression2loc_opt i.init
    ) a.declarations |> List.flatten
  | __ -> []

let convert line_lenght =
  List.map (fun i ->
      List.mapi (fun n j -> if n < i.line - 1 then j + 1 else 0) line_lenght
      |> List.fold_left (+) 0 |> (+) i.column)

let rec split tree s_list =
  if List.length s_list > 0 then begin
    match !tree with
    | Leaf (slice, str) -> (
        let s_list = slice.begin_ :: s_list @ [ slice.end_ ] |> List.rev |> List.to_seq |> Stack.of_seq in
        let rec sub b list =
          if not @@ Stack.is_empty s_list then (
            let e = Stack.pop s_list in
            Printf.printf "%d %d %d %d %d\t" b e (b - slice.begin_) (e - b) @@ String.length str;
            let s = String.sub str (b - slice.begin_) (e - b) in
            s |> print_endline;
            sub e (ref (Leaf ({ begin_ = b; end_ = e}, s)) :: list)
          ) else list in
        let b = Stack.pop s_list in
        tree := Tree (slice, sub b []))
    | Tree (_, list) -> (
        let begin_ = List.hd s_list in
        let v = List.find (fun i -> match !i with
            | Leaf (i, _) -> i.begin_ <= begin_
            | Tree (i, _) -> i.begin_ <= begin_
          ) list in
        split v s_list)
  end

let process str =
  let code = Ast_walk.parse str in
  let pp b = Flow_ast.Statement.show_t' Loc.pp Loc.pp (snd b) |> print_endline in
  let pl i = Loc.show_position i |> print_endline in
  List.iter pp code;
  List.length code |> Printf.printf "%d\n";
  print_endline str;
  let line_lenght = String.split_on_char '\n' str |> List.map String.length in
  let str_tree = ref (Leaf ({begin_ = 0; end_ = String.length str }, str)) in
  let _ =
    List.iter (fun i -> snd i |> statement2loc |> List.iter (fun i -> convert line_lenght i |> split str_tree)) code
  in
  let rec join i = match !i with
    | Leaf (_, s) -> s
    | Tree (_, l) ->
      let r = List.map join l |> List.rev in
      let s = String.concat "" [ List.nth r 0; "g("; List.nth r 1; ", "; List.nth r 3; ")"; List.nth r 5 ] in
      Printf.printf "%s\n---\n" s;
      s
  in
  show_tree str_tree |> print_endline;
  print_endline "===";
  let r = join str_tree in
  r |> print_endline;
  r
