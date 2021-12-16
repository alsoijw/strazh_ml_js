open Test

let () =
  let open Alcotest in
  run "TypeChecker" [
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
    "function", [
      test_case "1" `Quick function_1;
      test_case "2" `Quick function_2;
      test_case "3" `Quick function_3;
      test_case "4" `Quick function_4;
      test_case "5" `Quick function_5;
      test_case "6" `Quick function_6;
      test_case "7" `Quick function_7;
    ];
    "constrait", [
      test_case "1" `Quick constrait_1;
      test_case "2" `Quick constrait_2;
    ];
    "visibility", [
      test_case "1" `Quick visibility_1;
      test_case "2" `Quick visibility_2;
      test_case "3" `Quick visibility_3;
      test_case "4" `Quick visibility_4;
      test_case "5" `Quick visibility_5;
      test_case "6" `Quick visibility_6;
      test_case "7" `Quick visibility_7;
      test_case "8" `Quick visibility_8;
    ];
    "scope", [
      test_case "1" `Quick scope_1;
      test_case "2" `Quick scope_2;
    ];
    "relation", [
      test_case "1" `Quick relation_1;
    ];
  ]
