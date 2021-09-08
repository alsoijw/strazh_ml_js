open Test

let () =
  let open Alcotest in
  run "Type checker" [
    "assignment", [
      test_case "1" `Quick assignment_1;
      test_case "2" `Quick assignment_2;
    ];
    "raw values", [
      test_case "1" `Quick raw_values_1;
      test_case "2" `Quick raw_values_2;
    ];
  ]
