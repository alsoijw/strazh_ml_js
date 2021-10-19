type 'a vector = {
  mutable list: 'a list
}
[@@deriving show]

let append v i =
  v.list <- List.append v.list [ i ]

