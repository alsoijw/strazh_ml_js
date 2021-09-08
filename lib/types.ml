module Hashtbl = struct
  include Hashtbl

  let pp pp_key pp_value ppf values =
    Hashtbl.iter (fun a b -> Format.fprintf ppf "  @[<1>%a: %a@]@." pp_key a pp_value b) values;
end


type callable = variable2value -> value list -> value
and val_type =
    Value
  | Callable of callable
  | Raw | Db
and value = {
  id : int;
  kind : val_type;
  bases_on : value list
}
and variable2value = {
  variables : (string, value) Hashtbl.t;
  mutable corrupted : value list
}
[@@deriving show]

type runtime_error =
    Undef
  | IsNotCallable
[@@deriving show]
