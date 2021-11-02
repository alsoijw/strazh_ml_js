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
