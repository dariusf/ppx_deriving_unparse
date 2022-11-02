type expr =
  | Num of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@prec left, 4]
[@@deriving unparse { between = " " }]

let () =
  let e = Times (Plus (Num 1, Num 2), Num 3) in
  print_endline (unparse_expr e);
  let e = Plus (Times (Num 1, Num 2), Num 3) in
  print_endline (unparse_expr e)