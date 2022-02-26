let noparens table inner outer side =
  let pi, fi = List.assoc inner table in
  let po, fo = List.assoc outer table in
  if pi > po then true
  else
    match (fi, side) with
    | `Postfix, `Left -> true
    | `Prefix, `Right -> true
    | `Infix `Left, `Left -> pi = po && fo = `Infix `Left
    | `Infix `Right, `Right -> pi = po && fo = `Infix `Right
    | _, `Nonassoc -> fi = fo
    | _ -> false