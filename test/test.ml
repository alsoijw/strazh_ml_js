open Lib
open Loc

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

let print_5() =
  Alcotest.(check string) "" (Printer.process "\n\tv = a[0]") "\n\tv = g(a, 0)"

let print_6() =
  Alcotest.(check string) "" (Printer.process "v = a.c") "v = a.c"

let print_7() =
  Alcotest.(check string) "" (Printer.process "v = a[ c.i[j] ]") "v = g(a, g(c.i, j))"

let print_8() =
  Alcotest.(check string) "" (Printer.process "delete a[ b[c] ]") "delete a[ g(b, c) ]"

let print_9() =
  Alcotest.(check string) "" (Printer.process "!a[b]") "!g(a, b)"

let print_10() =
  Alcotest.(check string) "" (Printer.process "var { a: [ b = e[f] ] } = d") "var { a: [ b = g(e, f) ] } = d"

let print_11() =
  Alcotest.(check string) "" (Printer.process "var { a: { b = e[f] } = h[i] } = j[k]") "var { a: { b = g(e, f) } = g(h, i) } = g(j, k)"

let print_12() =
  Alcotest.(check string) "" (Printer.process "a[ b[c] ]++") "a[ g(b, c) ]++"

let print_13() =
  Alcotest.(check string) "" (Printer.process "a[ b[c] ]++") "a[ g(b, c) ]++"

let print_14() =
  Alcotest.(check string) "" (Printer.process "function a(b = c[d]) {}") "function a(b = g(c, d)) {}"

let print_15() =
  Alcotest.(check string) "" (Printer.process "a.b[c].d") "g(a.b, c).d"

let print_16() =
  Alcotest.(check string) "" (Printer.process "a[b] = c") "a[b] = c"

let print_17() =
  Alcotest.(check string) "" (Printer.process ?debug:(Some true) "this[a]") "g(this, a, false)"

let search_0() =
  let list = Search.process "this.a = b" in
  let v = [] in
  Alcotest.(check string) "" (if v = list then "true" else "false") "true"

let search_1() =
  let list = Search.process "this['a'] = b" in
  let v = [] in
  Alcotest.(check string) "" (if v = list then "true" else "false") "true"

let search_2() =
  let list = Search.process "this[a] = b" in
  let v = [
    { source = None;
      start = { line = 1; column = 0 };
      _end = { line = 1; column = 4 } }
  ] in
  Alcotest.(check string) "" (if v = list then "true" else "false") "true"

let search_3() =
  let list = Search.process "a = this" in
  let v = [ {
      source = None;
      start = { line = 1; column = 4 };
      _end = { line = 1; column = 8 }
    } ] in
  Alcotest.(check string) "" (if v = list then "true" else "false") "true"

let search_4() =
  let list = Search.process "b = this.a" in
  let v = [] in
  Alcotest.(check string) "" (if v = list then "true" else "false") "true"

let search_5() =
  let list = Search.process "b = this['a']" in
  let v = [] in
  Alcotest.(check string) "" (if v = list then "true" else "false") "true"

let search_6() =
  let list = Search.process "b = this[a]" in
  let v = [
    { source = None;
      start = { line = 1; column = 4 };
      _end = { line = 1; column = 8 } }
  ] in
  Alcotest.(check string) "" (if v = list then "true" else "false") "true"
