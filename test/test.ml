open Helpers
open Lib.Value
open Lib

let assignment_1() = 
  let v2v = setup None
      (fun v2v -> 
         value_new v2v Value [] |> V2v.set v2v "b") in
  try_test v2v 0
    "a = b";
  Alcotest.(check bool) "" ((V2v.find v2v "a") == (V2v.find v2v "b")) true

let assignment_2() = 
  let v2v = setup None
      (fun v2v ->
         let t = value_new v2v Value [] in
         V2v.set v2v "a" t;
         V2v.set v2v "b" t;
         value_new v2v Value [] |> V2v.set v2v "c") in
  try_test v2v 0
    "a = c";
  Alcotest.(check bool) "" ((V2v.find v2v "a") != (V2v.find v2v "b")) true

let raw_values_1() = 
  let v2v = test_func() in
  try_test v2v 0
    "d = a = raw_data()";

  Alcotest.(check bool) ""
    (match (V2v.find v2v "a").kind with
     | Raw -> true
     | _ -> false)
    true

let raw_values_2() = 
  let v2v = test_func() in
  try_test v2v 1
    "a = db_query(raw_data())";
  let a = V2v.find v2v "a" in
  let f = 
    (match a.kind with
     | Db ->
       List.length a.bases_on == 1 && (
         match (List.nth a.bases_on 0).kind with
         | Raw -> true
         | _ -> false)
     | _ -> false)
    && List.length v2v.corrupted == 1
    && List.nth v2v.corrupted 0 == a
  in
  Alcotest.(check bool) "" f
    true

