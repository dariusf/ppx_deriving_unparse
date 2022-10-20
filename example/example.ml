type expr =
  | Num of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@prec left, 4]
[@@deriving unparse { between = " " }]

type term =
  | Lambda of term [@form "λ." _1] [@prec left, 2]
  | Var of int [@form _1]
  | App of term * term [@form _1 " " _2] [@prec left, 3]
[@@deriving unparse]

let () =
  let e = Times (Plus (Num 1, Num 2), Num 3) in
  print_endline (unparse_expr e);
  let e = Plus (Times (Num 1, Num 2), Num 3) in
  print_endline (unparse_expr e);
  let l = App (Lambda (Var 0), Lambda (Var 0)) in
  print_endline (unparse_term l)
