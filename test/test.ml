type expr =
  | Num of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@form.latex _1 "\\times" _2]
      [@prec left, 4]
[@@deriving unparse { between = " "; latex }]

let () =
  let ex1 = Times (Plus (Num 1, Num 2), Num 3) in
  print_endline (unparse_expr ex1);
  print_endline latex_commands_expr;
  print_endline (latex_unparse_expr ex1)

type party = (string[@form _1]) [@@deriving unparse]
type channel = (string[@form _1]) [@@deriving unparse]
type event = (party * int[@form _1 _2]) [@@deriving unparse]

type global =
  | GComm of party * int * party * channel [@form _1 "-" _2 "->" _3 ": " _4]
  | GSeq of global * global [@form _1 "; " _2] [@prec left, 4]
  | GPar of global * global [@form _1 " * " _2] [@prec left, 3]
  | GEnd [@form "end"]
[@@deriving unparse]

let () =
  let ex1 =
    GSeq
      ( GSeq
          ( GComm ("a", 1, "b", "c1"),
            GPar (GComm ("a", 1, "b", "c1"), GComm ("a", 1, "b", "c1")) ),
        GEnd )
  in
  print_endline (unparse_global ex1)

type term =
  | Lambda of term [@form "λ." _1] [@prec left, 2]
  | Var of int [@form _1]
  | App of term * term [@form _1 " " _2] [@prec left, 3]
[@@deriving unparse ~latex]

let () =
  let l = App (Lambda (Var 0), Lambda (Var 0)) in
  print_endline (unparse_term l);
  let l = Lambda (App (Var 0, Lambda (Var 0))) in
  print_endline (unparse_term l);
  print_endline latex_commands_term;
  print_endline (latex_unparse_term l)

type re =
  | Emp [@form "ε"]
  | Bot [@form "⊥"]
  | Label of string [@form _1]
  | Concat of re * re [@form _1 "⋅" _2] [@prec left, 3]
  | Star of re [@form _1 "✭"] [@prec prefix, 4]
  | Inf of re [@form _1 "∞"] [@prec prefix, 4]
  | Omega of re [@form _1 "ω"] [@prec prefix, 4]
  | Or of re * re [@form _1 " ∨ " _2] [@prec left, 2]
[@@deriving unparse]

let () =
  print_endline
    (unparse_re
       (Or
          ( Omega (Label "a"),
            Or
              ( Inf (Concat (Bot, Label "c")),
                Concat (Star (Concat (Emp, Label "b")), Star (Label "b")) ) )))
