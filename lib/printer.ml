open Loc

           (*
let pattern2code i =
  wrap (match snd i with
      | Flow_ast.Pattern.Identifier e -> (e.name |> snd).name
      | _ -> "") i

              *)
let rec expression2loc i =
  (match snd i with
   | Flow_ast.Expression.Assignment d -> expression2loc d.right
   | Flow_ast.Expression.Member d -> (match d.property with
       | Flow_ast.Expression.Member.PropertyIdentifier (_, f) -> []
       | Flow_ast.Expression.Member.PropertyExpression f -> (
           [
             (fst d._object).start;
             (fst d._object)._end;
             (fst f).start;
             (fst f)._end;
             (fst i)._end;
           ]
         )
       | _ -> []
     )
   | _ -> []) 

let statement2loc = function
  | Flow_ast.Statement.Expression c -> expression2loc c.expression
  | _ -> []

let convert =
  List.map (fun i -> i.column)

let split str s_list =
  let s_list = [0] @ s_list @ [ String.length str ] |> List.rev |> List.to_seq |> Stack.of_seq in
  let rec sub b list =
    if not @@ Stack.is_empty s_list then (
      let e = Stack.pop s_list in
      Printf.printf "%d %d\t" b e;
      let s = String.sub str b (e - b) in
      s |> print_endline;
      sub e (s :: list)
    ) else list in
  let b = Stack.pop s_list in
  sub b []

let test =
  let r = split "() => arr[ i[0] ];" [ 6; 9; 11; 16; ]
  in ()

let process str =
  let code = Ast_walk.parse str in
  let pp b = Flow_ast.Statement.show_t' Loc.pp Loc.pp (snd b) |> print_endline in
  let pl i = Loc.show_position i |> print_endline in
  List.iter pp code;
  print_endline str;
  let r = List.hd code |> snd |> statement2loc |> convert |> split str |> List.rev in
  String.concat "" [ List.nth r 0; "g("; List.nth r 1; ", "; List.nth r 3; ")"; List.nth r 5 ]
  |> print_endline
