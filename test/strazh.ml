open Test

let () =
  let open Alcotest in
  run "TypeChecker" [
    "Print", [
      test_case "1" `Quick print_1;
    ];
  ]
