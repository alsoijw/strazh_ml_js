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
    "if", [
      test_case "0" `Quick if_0;
      test_case "1" `Quick if_1;
      test_case "2" `Quick if_2;
      test_case "3" `Quick if_3;
      test_case "4" `Quick if_4;
      test_case "5" `Quick if_5;
      test_case "6" `Quick if_6;
      test_case "7" `Quick if_7;
    ];
  ]
