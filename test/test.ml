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

(* let render_event (p, n) = render_party p ^^ render_int n *)

type global =
  | GComm of party * int * party * channel [@form _1 "-" _2 "->" _3 ": " _4]
  | GSeq of global * global [@form _1 "; " _2] [@prec left, 4]
  | GPar of global * global [@form _1 " * " _2] [@prec left, 3]
  | GEnd [@form "end"]
[@@deriving unparse]

let () =
  let ex1 =
    GSeq
      ( GComm ("a", 1, "b", "c1"),
        GPar (GComm ("a", 1, "b", "c1"), GComm ("a", 1, "b", "c1")) )
  in
  print_endline (unparse_global ex1)

(* TODO mutual recursion, list, indentation, pprint *)

(* type stmt =
     | Send of int [@form "send" _1]
     | Select of program list [@form _1 "; " _2] [@prec left, 4]

   and program = stmt list [@@deriving unparse] *)

(* type local =
     | LSend o(c, e)f event * channel
     | LRecv of event * channel
     | LSeq of local * local
     | LPar of local * local
     | LEnd

   let () =
     let ex1 =
       GSeq
         ( GComm ("a", 1, "b", "c1"),
           GPar (GComm ("a", 1, "b", "c1"), GComm ("a", 1, "b", "c1")) )
     in
     print_endline (unparse_global ex1) *)

(* latex? *)