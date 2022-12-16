type expr =
  | Num of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@prec left, 4]
[@@deriving unparse ~latex]

module Latex = struct
  let escape s = Format.asprintf "$%s$" s

  let standalone cmds f =
    print_endline "\\documentclass{article}";
    print_endline cmds;
    print_endline "\\begin{document}";
    f ();
    print_endline "\\end{document}"
end

let () =
  let e = Times (Plus (Num 1, Num 2), Num 3) in
  Latex.(
    standalone latex_commands_expr (fun () ->
        print_endline (escape (latex_print_expr e))));
  let e = Times (Plus (Num 1, Num 2), Num 3) in
  print_endline (unparse_expr e);
  let e = Plus (Times (Num 1, Num 2), Num 3) in
  print_endline (unparse_expr e)
