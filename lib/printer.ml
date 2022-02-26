open Loc
open Ast_walk

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
            if false then Printf.printf "%d %d %d %d %d\t" b e (b - slice.begin_) (e - b) @@ String.length str;
            let s = String.sub str (b - slice.begin_) (e - b) in
            if false then s |> print_endline;
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

let process ?(debug = false) str =
  let code = Helpers.parse str in
  let pp b = Flow_ast.Statement.show_t' Loc.pp Loc.pp (snd b) |> print_endline in
  let _pl i = Loc.show_position i |> print_endline in
  if debug then (
    List.iter pp code;
    List.length code |> Printf.printf "%d\n");
  (*
    print_endline str;
     *)
  let line_lenght = String.split_on_char '\n' str |> List.map String.length in
  let str_tree = ref (Leaf ({begin_ = 0; end_ = String.length str }, str)) in

  let fn = {
    expression2loc = (fun i -> function
        | Flow_ast.Expression.Member d -> (match d.property with
            | Flow_ast.Expression.Member.PropertyIdentifier _ -> []
            | Flow_ast.Expression.Member.PropertyPrivateName _ -> []
            | Flow_ast.Expression.Member.PropertyExpression f -> [
                (fst d._object).start;
                (fst d._object)._end;
                (fst f).start;
                (fst f)._end;
                (fst i)._end; ])
        | _ -> []);

    pattern_tail = true } in
  let _ =
    List.iter (fun i -> Ast_walk.ast_walk fn i |> List.iter (fun i -> convert line_lenght i |> split str_tree)) code
  in
  let rec join i = match !i with
    | Leaf (_, s) -> s
    | Tree (_, l) ->
      let r = List.map join l |> List.rev in
      let s = String.concat "" [ List.nth r 0; "g("; List.nth r 1; ", "; List.nth r 3; ")"; List.nth r 5 ] in
      if false then Printf.printf "%s\n---\n" s;
      s
  in
  (*
    if false then (
      show_tree str_tree |> print_endline;
      print_endline "===");
     *)
  let r = join str_tree in
  (*
    r |> print_endline;
     *)
  r
