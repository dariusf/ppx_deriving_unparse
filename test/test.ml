type expr =
  | Num of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@prec left, 4]
[@@deriving unparse { between = " " }]

let () =
  let ex1 = Times (Plus (Num 1, Num 2), Num 3) in
  print_endline (unparse_expr ex1)

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
