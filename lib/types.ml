module Hashtbl = struct
  include Hashtbl

  let pp pp_key pp_value ppf values =
    Hashtbl.iter (fun a b -> Format.fprintf ppf "  @[<1>%a: %a@]@." pp_key a pp_value b) values;
end

type callable = (Loc.t, Loc.t) Flow_ast.Expression.expression_or_spread list -> variable2value -> value list -> value
and val_type =
    Value
  | Callable of callable
  | Raw | Db
  | Numbered of int
  | Union
and value = {
  id : int;
  kind : val_type;
  bases_on : value list
}
and variable2value = {
  variables : (string, Loc.position * value) Hashtbl.t;
  constrait : (string, (Loc.position, value -> bool) Hashtbl.t) Hashtbl.t;
  parrent : variable2value option;
  mismatches : value Vector.vector;
  mutable return : value list
}
[@@deriving show]

type runtime_error =
    Undef
  | IsNotCallable
[@@deriving show]
