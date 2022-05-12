open Core

let () =
  (*
  "a.b[c].d"
    *)
  In_channel.read_all "/home/thousand/work/videospeed/inject.js"
  |> Lib.Printer.process ?debug:(Some false)
  |> print_endline
