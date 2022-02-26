open Ppxlib
module Ast = Ast_builder.Default

let ppx_name = "unparse"
let table_name name = "table_" ^ name

exception Failed of location * string

let error ~loc fmt = Format.ksprintf (fun s -> raise (Failed (loc, s))) fmt
let error_expr ~loc msg = [%expr [%ocaml.error [%e Ast.estring ~loc msg]]]
let error_str ~loc msg = [%str [%ocaml.error [%e Ast.estring ~loc msg]]]

let to_list_expr ~loc items =
  List.fold_right (fun c t -> [%expr [%e c] :: [%e t]]) items [%expr []]

(** Given a type, generate a function for printing it (to a string, by default) *)
let printer_for ~prefix typ =
  let type_name =
    match typ.ptyp_desc with
    | Ptyp_constr ({ txt = Lident name; _ }, _) -> name
    | _ -> failwith "nyi type name other than constr"
  in
  let loc = typ.ptyp_loc in
  match type_name with
  | "int" -> [%expr string_of_int]
  | "string" -> [%expr Fun.id]
  | "bool" -> [%expr string_of_bool]
  | "float" -> [%expr string_of_float]
  | _ -> Ast.pexp_ident ~loc { loc; txt = Lident (prefix ^ type_name) }

(** Parenthesize a rendered expression *)
let parenthesize ~loc e = [%expr Format.sprintf "(%s)" [%e e]]

(** Given the ith argument, generates and applies its printer *)
let typed_printed_expr ~prefix ~loc i typ =
  let fn = printer_for ~prefix typ in
  let arg =
    Ast.pexp_ident ~loc { loc; txt = Lident (Format.sprintf "v%d" i) }
  in
  (arg, [%expr [%e fn] [%e arg]])

(** Given a constructor, finds the form template in its attributes *)
let constructor_form constr =
  match
    List.find_opt
      (fun a -> String.equal a.attr_name.txt "form")
      constr.pcd_attributes
  with
  | None -> error ~loc:constr.pcd_loc "no default form yet"
  | Some f ->
    (match f.attr_payload with
    | PStr [{ pstr_desc = Pstr_eval (e, _attrs); _ }] ->
      begin
        match e.pexp_desc with
        | Pexp_apply (f, args) -> f :: List.map snd args
        | Pexp_ident _ | Pexp_constant _ -> [e]
        | _ -> failwith "not apply"
      end
    | _ -> failwith "form is not structure")

(** Given a constructor, reads the precedence annotation as an expression *)
let constructor_prec_spec ~loc constr =
  match
    List.find_opt (fun a -> a.attr_name.txt = "prec") constr.pcd_attributes
  with
  | None -> None
  | Some p ->
    (match p.attr_payload with
    | PStr [{ pstr_desc = Pstr_eval (e, _attrs); _ }] ->
      begin
        match e.pexp_desc with
        | Pexp_tuple
            [
              { pexp_desc = Pexp_ident { txt = Lident a; _ }; _ };
              { pexp_desc = Pexp_constant (Pconst_integer (i, _)); _ };
            ] ->
          let i = Ast.eint ~loc (int_of_string i) in
          let a =
            match a with
            | "left" -> [%expr `Infix `Left]
            | "right" -> [%expr `Infix `Right]
            | "nonassoc" -> [%expr `Infix `Nonassoc]
            | "prefix" -> [%expr `Prefix]
            | "postfix" -> [%expr `Postfix]
            | _ -> failwith "unrecognised prec spec"
          in
          Some (i, a)
        | _ -> failwith "not recognised prec spec"
      end
    | _ -> failwith "form is not structure")

(** Given a datatype and a list of constructor declarations, generates a printer function *)
let generate_printer ~loc padding fn name branches =
  let (module Ast) = Ast_builder.make loc in
  let prefix = fn ^ "_" in
  let fn_name = prefix ^ name in
  let var_name = "e" in
  (* TODO mutually recursive *)
  let rec_flag = Recursive in
  Ast.pstr_value rec_flag
    [
      Ast.value_binding
        ~pat:(Ast.ppat_var { loc; txt = fn_name })
        ~expr:
          (Ast.pexp_fun Nolabel None
             (Ast.ppat_var { loc; txt = var_name })
             (Ast.pexp_match
                (Ast.pexp_ident { loc; txt = Lident var_name })
                (List.map
                   (fun constr ->
                     let form = constructor_form constr in
                     let has_prec =
                       Option.is_some (constructor_prec_spec ~loc constr)
                     in
                     let vars =
                       match constr.pcd_args with
                       | Pcstr_tuple xs ->
                         List.mapi
                           (fun i _typ ->
                             Ast.ppat_var { loc; txt = Format.sprintf "v%d" i })
                           xs
                       | _ -> failwith "nyi non tuple lhs"
                     in
                     let right =
                       match constr.pcd_args with
                       | Pcstr_tuple xs ->
                         (* print each variable *)
                         let vars, rendered =
                           List.mapi
                             (fun i typ ->
                               typed_printed_expr ~prefix ~loc i typ)
                             xs
                           |> List.split
                         in
                         (* substitute variables into template *)
                         let items =
                           List.map
                             (fun f ->
                               match f.pexp_desc with
                               | Pexp_ident { txt = Lident i; _ }
                                 when i.[0] = '_' ->
                                 let var_i =
                                   int_of_string
                                     (String.sub i 1 (String.length i - 1))
                                   - 1
                                 in
                                 let var = List.nth vars var_i in
                                 let printed = List.nth rendered var_i in
                                 if not has_prec then printed
                                 else
                                   (* decide whether to parenthesize *)
                                   [%expr
                                     if
                                       Deriving_unparse.noparens
                                         [%e
                                           Ast.pexp_ident
                                             {
                                               loc;
                                               txt = Lident (table_name name);
                                             }]
                                         [%e var] e [%e Ast.eint var_i]
                                     then [%e printed]
                                     else [%e parenthesize ~loc printed]]
                               | _ -> f)
                             form
                         in
                         (* produce the body of each case split *)
                         let concat =
                           match items with
                           | [] -> failwith "empty segments"
                           | [i] -> i
                           | _ ->
                             let segments = to_list_expr ~loc items in
                             [%expr
                               String.concat [%e Ast.estring padding]
                                 [%e segments]]
                         in
                         concat
                       | _ -> failwith "nyi non tuple rhs"
                     in
                     Ast.case ~guard:None
                       ~lhs:
                         (Ast.ppat_construct
                            { loc; txt = Lident constr.pcd_name.txt }
                            (match vars with
                            | [] -> None
                            | _ -> Some (Ast.ppat_tuple vars)))
                       ~rhs:right)
                   branches)));
    ]

let generate_precedence_match ~loc name cstrs =
  let (module Ast) = Ast_builder.make loc in
  Ast.pstr_value Nonrecursive
    [
      Ast.value_binding
        ~pat:(Ast.ppat_var { loc; txt = table_name name })
        ~expr:
          (Ast.pexp_function
             (List.concat_map
                (fun c ->
                  let spec = constructor_prec_spec ~loc c in
                  match spec with
                  | None -> []
                  | Some (i, e) ->
                    [
                      Ast.case
                        ~lhs:
                          (Ast.ppat_construct
                             { loc; txt = Lident c.pcd_name.txt }
                             (Some Ast.ppat_any))
                        ~guard:None
                        ~rhs:[%expr Some ([%e i], [%e e])];
                    ])
                cstrs
             @ [Ast.case ~lhs:Ast.ppat_any ~guard:None ~rhs:[%expr None]]));
    ]

let generate_type_decl padding fn t =
  (* TODO mutually recursive types *)
  let td = List.hd t in
  let { loc; txt = name } = td.ptype_name in
  match td.ptype_kind with
  | Ptype_variant cstrs ->
    [
      generate_precedence_match ~loc name cstrs;
      generate_printer ~loc padding fn name cstrs;
    ]
  | _ -> failwith "nyi non variant ptype kind"

let str_gen ~loc:_ ~path:_ (_rec, t) padding fn =
  let padding = Option.value ~default:"" padding in
  let fn = Option.value ~default:"unparse" fn in
  try
    let extra = generate_type_decl padding fn t in
    extra
  with Failed (loc, s) -> error_str ~loc s

let sig_gen ~loc ~path:_ (_rec, _t) =
  let (module Ast) = Ast_builder.make loc in
  (* we are silently dropping mutually recursive definitions to keep things
     brief *)
  (* let t = List.hd t in
     let name = module_name_of_type t in
     let type_ =
       let sig_ =
         [%sig:
           val path : string
           val name : string]
       in
       Ast.pmty_signature sig_
     in
     Ast.module_declaration ~name ~type_ |> Ast.psig_module |> fun a -> [a] *)
  []

let () =
  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(
        empty +> arg "padding" (estring __) +> arg "fn" (estring __))
      str_gen
  in
  let sig_type_decl = Deriving.Generator.make_noarg sig_gen in
  Deriving.add ppx_name ~str_type_decl ~sig_type_decl |> Deriving.ignore
