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
  try_test v2v 0
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

let if_0() =
  let v2v1 = test_func() in
  try_test v2v1 0
    "if(raw_data())
       a = raw_data()
     else
       a = safe_data()
     a = db_query(a)";
  let v2v2 = test_func() in
  try_test v2v2 0
    "if(raw_data())
       a = safe_data()
     else
       a = raw_data()
     a = db_query(a)";
  let f =
    List.for_all2 (=) v2v1.corrupted [ V2v.find v2v1 "a" ]
    && List.for_all2 (=) v2v2.corrupted [ V2v.find v2v2 "a" ] in
  Alcotest.(check bool) "" f true

let if_1() =
  let v2v = test_if() in
  try_test v2v 0
    "a = _()
     if(b)
       a = _()";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 1; 0 ]) true

let if_2() =
  let v2v = test_if() in
  try_test v2v 0
    "a = _()
     if(a = _())
       _()";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 1 ]) true

let if_3() =
  let v2v = test_if() in
  try_test v2v 0
    "a = _()
     if(a = _())
       a = _()";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 2; 1 ]) true

let if_4() =
  let v2v = test_if() in
  try_test v2v 0
    "a = _()
     if(a = _())
       a = _()
     else
       a = _()";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 2; 3 ]) true

let if_5() =
  let v2v = test_if() in
  try_test v2v 0
    "a = _()
     if(a = _())
       a = _()
     else if(_())
       a = _()";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 2; 4; 1 ]) true

let if_6() =
  let v2v = test_if() in
  try_test v2v 0
    "a = _()
     if(a = _())
       a = _()
     else if(a = _())
       a = _()
     else
       a = _();";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 2; 4; 5 ]) true

let if_7() =
  let v2v = test_if() in
  try_test v2v 0
    "a = _()
     if(_()) {
       if(a = _()) {
         a = _()
       } else {
         a = _()
       }
     } else {
       _()
     }";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 3; 4; 0 ]) true

let function_1() =
  let v2v = test_func() in
  try_test v2v 0
    "b = a(raw_data())

     function a(b) {
       return db_query(b);
     }";
  let b = (V2v.find v2v "b").bases_on in
  let r = List.for_all2 (=) b v2v.corrupted &&
          List.length b = 1 in
  Alcotest.(check bool) "" r true

let function_2() =
  let v2v = test_if() in
  try_test v2v 0
    "a = c()

     function c() {
       if(b)
         return _()
       else
         return _()
     }";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 0; 1 ]) true

let function_3() =
  let v2v = test_if() in
  try_test v2v 0
    "a = b()

     function b() {
       function c() {
         return _();
       }
       return c();
     }";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 0 ]) true

let function_4() =
  let v2v = test_if() in
  try_test v2v 0
    "a = b()

     function b() {
       function c() {
         return _();
       }
       c();
       return c();
     }";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 1 ]) true

let function_5() =
  let v2v = test_if() in
  try_test v2v 0
    "a = c()

     function c() {
       if(b) {
          return _();
       } else if(b) {
          return _();
       } else {
          return _();
       }
       return _();
     }";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 0; 1; 2 ]) true

let function_6() =
  let v2v = test_if() in
  try_test v2v 0
    "a = b()

     function b() {
       return _();
       return _();
     }";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 0 ]) true

let function_7() =
  let v2v = test_if() in
  try_test v2v 0
    "a = b()

     function b() {
       {
         return _();
       }
       return _();
     }";
  let r = numb2list v2v in
  Alcotest.(check bool) "" (List.for_all2 (=) r [ 0 ]) true
