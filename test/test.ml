open Lib


let print_1() =
  let _ = Printer.process "c = arr  [ 0 ]; console('1'); d=arr[1]" in
  (*
let code = Ast_walk.parse " a = b;\nc=d" in
let code = Ast_walk.parse "arr  [i[0] ]" in
let code = Ast_walk.parse "l = arr  [ 0 ]" in
let pp b = Flow_ast.Statement.show_t' Loc.pp Loc.pp (snd b) |> print_endline in
let pl i = Loc.show_position i |> print_endline in
let s2c i = snd i |> Printer.statement2loc |> List.iter pl in
List.iter pp code;
List.iter s2c code;
     *)
  (*
  Printer.test;
let pp b = Flow_ast.Statement.show_t' Loc.pp Loc.pp (snd b) |> print_endline in
let s2c i = snd i |> Printer.statement2code |> print_endline in
List.iter pp code;
List.iter s2c code;
     *)
Alcotest.(check bool) "" true false

