open Types
open Loc

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
                  | Ok t -> Ok (g (snd f.arguments).arguments v2v (Array.to_list t))
                  | Error v -> Error v)
              | _ -> Error IsNotCallable)
          | None -> Error Undef)
      | _ -> Error Undef)

  | Flow_ast.Expression.Assignment d -> (
      match snd d.left with
      | Flow_ast.Pattern.Identifier e -> (
          match ast2val v2v (snd d.right) with
          | Ok f -> V2v.set v2v (e.name |> snd).name ((fst d.left).start, f); Ok f
          | _ -> Error Undef)
      | _ -> Error Undef)

  | _ -> Error Undef
and ast2val1 v2v a =
  match a with
  | Flow_ast.Expression.Expression b -> ast2val v2v (snd b)
  | _ -> Error Undef

let rec def_func v2v a =
  let b = snd a in
  match b with
  | Flow_ast.Statement.FunctionDeclaration d -> (
      match d.id with
      | Some name ->
        let func_new f =
          Value.value_new (Callable f) [] in
        let run_func _ v2v args =
          let original = v2v in
          let v2v = V2v.wrap v2v () in
          let args_names = ref [] in

          List.iteri (fun n i -> 
              let open Flow_ast.Function.Param in
              match (snd i).argument |> snd with
              | Flow_ast.Pattern.Identifier i ->
                args_names := List.append !args_names [ (snd i.name).name ];
                (Loc.none.start, match List.nth_opt args n with
                  | Some v -> v
                  | None -> Value.value_new Value [])
                |> V2v.set v2v (snd i.name).name;
              | _ -> ()
            ) (snd d.params).params;

          (match d.body with
           | Flow_ast.Function.BodyBlock b ->
             ast_walk1 0 v2v (snd b).body
           | _ -> ());

          Hashtbl.iter (fun n i ->
              Hashtbl.iter (fun p c ->
                  let contains =
                    match Hashtbl.find_opt original.constrait n with
                    | Some hash ->
                      Hashtbl.find_all hash p |> List.exists (fun j -> j == c)
                    | _ -> false in
                  if not contains then
                    V2v.constrait_set original n p c
                ) i
            ) v2v.constrait;

          Hashtbl.iter (fun n v ->
              if not @@ List.mem n !args_names then
                V2v.set ~merge:true original n ((fst a)._end, Value.value_new Types.Union @@ uniq @@ ext [ V2v.find_opt original n; Some (snd v)])) v2v.variables;

          Value.value_new Types.Union v2v.return in
        V2v.set v2v (snd name).name (Loc.none.start, func_new run_func);
      | None -> ()
    )
  | _ -> ()

and ast_walk debug v2v a =
  let aw = ast_walk false in
  let b = snd a in
  if debug then Flow_ast.Statement.show_t' Loc.pp Loc.pp b |> print_endline;
  match b with
  | Flow_ast.Statement.Expression c -> let _ = ast2val v2v (snd c.expression) in true
  | Flow_ast.Statement.If c ->
    let true_ = V2v.wrap v2v () in
    let else_ = V2v.wrap v2v () in
    let _ = ast2val true_ @@ snd c.test in ();

    Hashtbl.iter (fun k v -> V2v.set else_ k v) true_.variables;
    let _ = aw true_ c.consequent in ();

    (match c.alternate with
     | Some i -> let _ = aw else_ (snd i).body in ();
     | None -> ());

    v2v.return <- List.append true_.return else_.return;

    let key2list = fun k _ acc -> k :: acc in
    Hashtbl.fold key2list true_.variables []
    |> Hashtbl.fold key2list else_.variables
    |> uniq
    |> List.iter (fun k ->
        let v =
          ext [ V2v.find_opt true_ k; V2v.find_opt else_ k ]
          |> uniq
          |> Value.value_new Union in V2v.set v2v k ((fst a).start, v));
    List.length true_.return == 0 && List.length else_.return == 0

  | Flow_ast.Statement.Block c ->
    ast_walk1 0 v2v c.body;
    List.length v2v.return == 0

  | Flow_ast.Statement.Return d -> (
      match d.argument with
      | Some e -> (
          match snd e |> ast2val v2v with
          | Ok value -> v2v.return <- List.append v2v.return [ value ]; false
          | Error _ -> false
        )
      | None -> false
    )

  | _ -> true;

and ast_walk1 debug v2v ast =
  let aw = ast_walk (debug > 1) v2v in
  let df = def_func v2v in

  if debug > 0 then Types.show_variable2value v2v |> print_endline;
  List.iter df ast;
  let _ = List.fold_left (fun a i ->
    if a && (match snd i with
          | Flow_ast.Statement.Return _ -> let _ = aw i in (); false
          | _ -> true)
    then
      aw i
    else
      false) true ast in ();
  if debug > 0 then Types.show_variable2value v2v |> print_endline;
