open Loc

let ext i =
  List.rev i
  |> List.fold_left (fun acc i -> match i with
      | Some v -> v :: acc;
      | None -> acc) []

let uniq i =
  let u = Hashtbl.create 0 in
  let o = ref [] in
  List.iter (fun v -> match Hashtbl.find_opt u v with
      | None -> Hashtbl.add u v 1; o := List.append !o [ v ]
      | _ -> ()
    ) i;
  !o

let parse code =
  (Parser_flow.program code |> fst |> snd).statements

let exclude a b =
  List.filter (fun i -> not @@ List.exists (fun j -> i = j) b) a

let convert line_lenght =
  List.map (fun i ->
      List.mapi (fun n j -> if n < i.line - 1 then j + 1 else 0) line_lenght
      |> List.fold_left (+) 0 |> (+) i.column)
