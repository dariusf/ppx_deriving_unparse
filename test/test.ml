type expr =
  | Lit of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@prec left, 4]
[@@deriving unparse]

let () = print_endline (render_expr (Times (Plus (Lit 1, Lit 2), Lit 3)))
