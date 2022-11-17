let get_side fixity index =
  match (fixity, index) with
  | `Prefix, _ -> `Right
  | `Postfix, _ -> `Left
  | `Infix _, 0 -> `Left
  | `Infix _, 1 -> `Right
  | `Infix _, _ -> `Nonassoc

let noparens lookup inner outer index =
  let inner = lookup inner in
  let outer = lookup outer in
  match (inner, outer) with
  | Some (pi, fi), Some (po, fo) ->
    if pi > po then true
    else
      let side = get_side fo index in
      (match (fi, side) with
      | `Postfix, `Left -> true
      | `Prefix, `Right -> true
      | `Infix `Left, `Left -> pi = po && fo = `Infix `Left
      | `Infix `Right, `Right -> pi = po && fo = `Infix `Right
      | _, `Nonassoc -> fi = fo
      | _ -> false)
  | _ -> true
