open Lib

let print_0() =
  Alcotest.(check string) "" (Printer.process "c = arr  [ 0 ]; console('1'); d=arr[1]") "c = g(arr, 0); console('1'); d=g(arr, 1)"

let print_1() =
  Alcotest.(check string) "" (Printer.process "c = arr[0][1]") "c = g(g(arr, 0), 1)"

let print_2() =
  Alcotest.(check string) "" (Printer.process "c = arr[i][j]") "c = g(g(arr, i), j)"

let print_3() =
  Alcotest.(check string) "" (Printer.process "c = arr[0][j]") "c = g(g(arr, 0), j)"

let print_4() =
  Alcotest.(check string) "" (Printer.process "c[ i[1] ] = arr[i][j]") "c[ g(i, 1) ] = g(g(arr, i), j)"
