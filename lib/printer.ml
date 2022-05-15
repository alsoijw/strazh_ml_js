open Loc
open Ast_walk

type slice = {
  begin_ : int;
  end_ : int;
  is_this : bool;
}
[@@deriving show]

type leaf =
  | Leaf of slice * string
  | Tree of slice * tree list
and tree = leaf ref
[@@deriving show]

let rec split tree is_this s_list =
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
            sub e (ref (Leaf ({ begin_ = b; end_ = e; is_this = false }, s)) :: list)
          ) else list in
        let b = Stack.pop s_list in
        tree := Tree ({slice with is_this = is_this }, sub b []))
    | Tree (_, list) -> (
        let begin_ = List.hd s_list in
        let v = List.find (fun i -> match !i with
            | Leaf (i, _) -> i.begin_ <= begin_
            | Tree (i, _) -> i.begin_ <= begin_
          ) list in
        split v is_this s_list)
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
  let str_tree = ref (Leaf ({begin_ = 0; end_ = String.length str; is_this = false}, str)) in

  let fn = {
    expression2loc = (fun i -> function
        | Flow_ast.Expression.Member d -> (match d.property with
            | Flow_ast.Expression.Member.PropertyIdentifier _ -> []
            | Flow_ast.Expression.Member.PropertyPrivateName _ -> []
            | Flow_ast.Expression.Member.PropertyExpression f -> [
                ([(fst d._object).start;
                  (fst d._object)._end;
                  (fst f).start;
                  (fst f)._end;
                  (fst i)._end; ],
                 (match snd d._object with
                  | Flow_ast.Expression.This _ -> true
                  | _ -> false))
              ])
        | _ -> []);

    pattern_tail = true } in
  let _ =
    List.iter (fun i ->
        Ast_walk.ast_walk fn i
        |> List.flatten
        |> List.iter (fun (i, is_this) ->
            Helpers.convert line_lenght i
            |> split str_tree is_this)) code
  in
  let rec join i = match !i with
    | Leaf (_, s) -> s
    | Tree (config, l) ->
      let r = List.map join l |> List.rev in
      let s = String.concat "" [
          List.nth r 0;
          "g(";
          List.nth r 1;
          ", ";
          List.nth r 3;
          if config.is_this then ", false)" else ")";
          List.nth r 5 ] in
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
