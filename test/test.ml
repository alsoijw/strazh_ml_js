open Helpers
open Lib.Value

let assignment_1() = 
  let v2v = try_test None 0
      "a = b"
      (fun v2v -> 
         value_new() |> Hashtbl.replace v2v "b") in
  Alcotest.(check bool) "" ((Hashtbl.find v2v "a") == (Hashtbl.find v2v "b")) true

let assignment_2() = 
  let v2v = try_test None 0
      "a = c"
      (fun v2v ->
         let t = value_new() in
         Hashtbl.replace v2v "a" t;
         Hashtbl.replace v2v "b" t;
         value_new() |> Hashtbl.replace v2v "c") in
  Alcotest.(check bool) "" ((Hashtbl.find v2v "a") != (Hashtbl.find v2v "b")) true

