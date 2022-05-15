open Core

let sanitize filename =
  In_channel.read_all filename
  |> Lib.Printer.process
  |> print_endline

let search filename =
  let code = In_channel.read_all filename in
  let lines = Stdlib.String.split_on_char '\n' code in
  let line_lenght = Stdlib.List.map String.length lines in
  Lib.Search.process code
  |> List.iter ~f:(fun i ->
      Printf.printf "\027[35m%s:%d:%d\027[0m\n" filename i.start.line i.start.column;
      let v = Stdlib.List.nth lines (i.start.line - 1) in
      let before = Stdlib.String.sub v 0 i.start.column in
      let now = Stdlib.String.sub v i.start.column (i._end.column - i.start.column) in
      let after = Stdlib.String.sub v i._end.column ((Stdlib.List.nth line_lenght (i.start.line - 1)) - i._end.column) in
      Printf.printf "%s\027[31m%s\027[0m%s\n" before now after;
      let leftpad = List.init i.start.column ~f:(fun _ -> " ") |> String.concat in
      Printf.printf "%s\027[32m^\027[0m\n" leftpad;)

let print_command =
  Command.basic
    ~summary:"Print escaped code"
    ~readme:(fun () -> "More detailed information")
    Command.Param.(
      map
        (anon ("filename" %: string))
        ~f:(fun filename () -> sanitize filename))

let search_command =
  Command.basic
    ~summary:"Find globalThis location"
    ~readme:(fun () -> "More detailed information")
    Command.Param.(
      map
        (anon ("filename" %: string))
        ~f:(fun filename () -> search filename))

let command =
  Command.group
    ~summary:"Search for non-obvious reflection"
    [ "print", print_command; "search", search_command ]

let () = Command.run command
