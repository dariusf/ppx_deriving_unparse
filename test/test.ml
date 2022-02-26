type expr =
  | Num of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@prec left, 4]
[@@deriving unparse { between = " " }]

let () =
  let ex1 = Times (Plus (Num 1, Num 2), Num 3) in
  print_endline (unparse_expr ex1)

type protocol =
  | Comm of string * string [@form _1 "->" _2]
  | Par of protocol * protocol [@form _1 " || " _2] [@prec left, 3]
  | Seq of protocol * protocol [@form _1 "; " _2] [@prec left, 4]
[@@deriving unparse]

let () =
  let ex1 = Seq (Comm ("a", "b"), Par (Comm ("c", "d"), Comm ("a", "b"))) in
  print_endline (unparse_protocol ex1)