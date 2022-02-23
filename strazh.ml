open Core

let () = let _r = Lib.Printer.process (In_channel.read_all "/home/thousand/work/videospeed/inject.js") in
  print_endline "";
