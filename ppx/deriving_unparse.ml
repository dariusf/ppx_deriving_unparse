let noparens table inner outer side =
  let inner = List.assoc inner table in
  let outer = List.assoc outer table in
  match (inner, outer) with
  | Some (pi, fi), Some (po, fo) ->
    (* let pi, fi = List.assoc inner table in *)
    (* let po, fo = List.assoc outer table in *)
    if pi > po then true
    else begin
      match (fi, side) with
      | `Postfix, `Left -> true
      | `Prefix, `Right -> true
      | `Infix `Left, `Left -> pi = po && fo = `Infix `Left
      | `Infix `Right, `Right -> pi = po && fo = `Infix `Right
      | _, `Nonassoc -> fi = fo
      | _ -> false
    end
  | _ -> true
