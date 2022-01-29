module Hashtbl = struct
  include Hashtbl

  let pp pp_key pp_value ppf values =
    Hashtbl.iter (fun a b -> Format.fprintf ppf "  @[<1>%a: %a@]@." pp_key a pp_value b) values;
end


type e_type = Type of string * e_type list
            | Union of e_type list
            | TCall of e_type list * e_type
            | TParam of int
            | TDeferred of int
            | TUnknown
            | TUnmatched
and visibility =
    Inherited
  | Local
  | Block
and variable2type = {
  variables : (string, e_type) Hashtbl.t;
  (*
    visibility : (string, visibility) Hashtbl.t;
  constrait : (string, e_type -> bool) Hashtbl.t;
  parrent : variable2type option;
  mutable return : e_type list
     *)
}
[@@deriving show]

let concat = String.concat ", "

let rec print_type = function
  | Type (name, param) -> name ^ (if List.length param > 0 then
                                    "<" ^ (List.map print_type param |> concat) ^ ">"
                                  else "")
  | TCall (args, return) -> List.map print_type (args @ [return]) |> String.concat ", "
  | Union list -> List.map print_type list |> String.concat " | "
  | _ -> ""

let sign type_v =
  let arr = [
    "Array", [
      "sort", TCall ([], Type ("Array", []));
      "join", TCall ([], Type ("String", []));
      "map", TCall ([ Type ("'a", [ TParam 0 ]); Type ("number", []); Type ("Array", [])], Type ("b", []))
    ];
    "Date", [
      "setTime", TCall ([Type ("number", [])], Type ("number", []));
    ];
    "String", [
      "replace", TCall ([Type ("String", [])], Type ("String", []));
      "toLowerCase", TCall ([], Type ("String", []));
      "split", TCall ([Type ("String", [])], Type ("Array", []));
    ];
  ] in
  let h = Hashtbl.create 0 in

  List.iter (fun i ->
      let hash = Hashtbl.create 0 in
      List.iter (fun j -> Hashtbl.add hash (fst j) (snd j)) (snd i);
      Hashtbl.add h (fst i) hash; ) arr;
  Hashtbl.iter (fun name body ->
      print_endline ("class " ^ name);
      Hashtbl.iter (fun name sign -> "   fn " ^ name ^ "(" ^ (print_type sign) ^ ")" |> print_endline) body;) h;
  h

let rec type_inference i v2t =
  let tid i = ti_debug i v2t in
  match i with
  | Relation.Call (fn, args) -> (match tid fn with
      | TCall (_, return) -> return
      | _ -> TUnmatched)

  | PropName (obj, prop) -> (match ti_debug obj v2t with
      | Type (t, param) -> (match Hashtbl.find_opt sign t with
          | Some vtable -> (match Hashtbl.find_opt vtable prop with
              | Some sign -> sign
              | None -> TUnmatched)
          | None -> TUnmatched)
      | _ -> TUnmatched)

  | Set (name, value) ->
    let ret = ti_debug value v2t in
    Hashtbl.replace v2t.variables name ret;
    ret
  | New (t_name, _) -> (match t_name with
      | Get name -> Type (name, [])
      | _ -> TUnmatched)
  | Get name -> (match Hashtbl.find_opt v2t.variables name with
      | Some t -> t
      | None -> TUnmatched)
  | LStr _ -> Type ("String", [])
  | LArr arr -> Type ("Array", [ Union (List.map tid arr |> Helpers.uniq) ])
  | LNum _ -> Type ("number", [])
  | _ -> TUnmatched

and ti_debug i v2t =
  let r = type_inference i v2t in
  (*
  show_e_type r |> print_endline;
     *)
  print_type r |> print_endline;
  Relation.show_r_type i |> print_endline;
  print_endline "---";
  r
