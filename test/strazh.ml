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
    ];
  ]
