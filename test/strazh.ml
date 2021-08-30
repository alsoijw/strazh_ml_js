open Test

let () =
  let open Alcotest in
  run "Type checker" [
    "1 assignment", [
      test_case "1" `Quick assignment_1;
      test_case "2" `Quick assignment_2;
    ];
  ]
