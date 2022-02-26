type expr =
  | Lit of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@prec left, 4]
[@@deriving unparse]

let () =
  let ex1 = Times (Plus (Lit 1, Lit 2), Lit 3) in
  print_endline (render_expr ex1)

type protocol =
  | Comm [@form "->"]
  | Par of protocol * protocol [@form _1 "||" _2] [@prec left, 3]
  | Seq of protocol * protocol [@form _1 ";" _2] [@prec left, 4]
[@@deriving unparse]

let () =
  let ex1 = Seq (Comm, Par (Comm, Comm)) in
  print_endline (render_protocol ex1)