open Test

let () =
  let open Alcotest in
  run "Sanitize" [
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
      test_case "1" `Quick print_12;
      test_case "1" `Quick print_13;
      test_case "1" `Quick print_14;
      test_case "1" `Quick print_15;
      test_case "1" `Quick print_16;
      test_case "1" `Quick print_17;
    ];
    "Search", [
      test_case "1" `Quick search_0;
      test_case "1" `Quick search_1;
      test_case "1" `Quick search_2;
      test_case "1" `Quick search_3;
      test_case "1" `Quick search_4;
      test_case "1" `Quick search_5;
      test_case "1" `Quick search_6;
    ]
  ]
