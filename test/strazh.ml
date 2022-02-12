open Test

let () =
  let open Alcotest in
  run "TypeChecker" [
    "Print", [
      test_case "1" `Quick print_0;
      test_case "1" `Quick print_1;
      test_case "1" `Quick print_2;
      test_case "1" `Quick print_3;
      test_case "1" `Quick print_4;
      test_case "1" `Quick print_5;
      test_case "1" `Quick print_6;
      test_case "1" `Quick print_7;
      test_case "1" `Quick print_8;
      test_case "1" `Quick print_9;
      test_case "1" `Quick print_10;
      test_case "1" `Quick print_11;
    ];
  ]
