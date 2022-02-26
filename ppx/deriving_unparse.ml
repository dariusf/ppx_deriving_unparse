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
  (* let inner = List.assoc inner table in *)
  (* let outer = List.assoc outer table in *)
  match (inner, outer) with
  | Some (pi, fi), Some (po, fo) ->
    (* let pi, fi = List.assoc inner table in *)
    (* let po, fo = List.assoc outer table in *)
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
