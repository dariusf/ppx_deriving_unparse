open Ppxlib
module Ast = Ast_builder.Default

let ppx_name = "unparse"
let table_name name = "table_" ^ name

type config = {
  between : string;
  fn : string;
  latex : bool;
}

let template_var n = Format.asprintf "_%d" n

let generate_case_split_fn loc cases =
  let (module Ast) = Ast_builder.make loc in
  let var_name = "e" in
  Ast.pexp_fun Nolabel None
    (Ast.ppat_var { loc; txt = var_name })
    (Ast.pexp_match (Ast.pexp_ident { loc; txt = Lident var_name }) cases)

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
let get_form ~loc ?(latex = false) attrs =
  let really name =
    List.find_opt (fun a -> String.equal a.attr_name.txt name) attrs
    (* | None ->  *)
    (* | Some f -> *)
    |> Option.map (fun f ->
           match f.attr_payload with
           | PStr [{ pstr_desc = Pstr_eval (e, _attrs); _ }] ->
             begin
               match e.pexp_desc with
               | Pexp_apply (f, args) -> f :: List.map snd args
               | Pexp_ident _ | Pexp_constant _ -> [e]
               | _ -> failwith "not apply"
             end
           | _ -> failwith "form is not structure")
  in
  match
    match latex with
    | false -> really "form"
    | true ->
      (match really "form.latex" with
      | None -> really "form"
      | Some f -> Some f)
  with
  | Some f -> f
  | None -> error ~loc "no default form yet"

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

type template_item =
  | Var of int
  | Constant of string

type template = template_item list

let parse_template form =
  form
  |> List.map (fun f ->
         match f.pexp_desc with
         | Pexp_ident { txt = Lident i; _ } when i.[0] = '_' ->
           let var_i =
             int_of_string (String.sub i 1 (String.length i - 1)) - 1
           in
           Var var_i
         | Pexp_constant (Pconst_string (s, _, _)) -> Constant s
         | _ ->
           failwith
             (Format.asprintf "cannot parse template %a" Pprintast.expression f))

(** Given a template encoded as a payload (expr), output an expression for the right side of a case split *)
let interpret_template_expr loc type_name no_parens vars rendered form between =
  let (module Ast) = Ast_builder.make loc in
  let items =
    form |> parse_template
    |> List.map (fun f ->
           match f with
           | Var i ->
             let var = List.nth vars i in
             let printed = List.nth rendered i in
             if no_parens then printed
             else
               (* decide whether to parenthesize *)
               [%expr
                 if
                   Ppx_deriving_unparse_runtime.noparens
                     [%e
                       Ast.pexp_ident
                         { loc; txt = Lident (table_name type_name) }]
                     [%e var] e [%e Ast.eint i]
                 then [%e printed]
                 else [%e parenthesize ~loc printed]]
           | Constant s -> Ast.estring s)
  in

  (* produce the body of each case split *)
  let concat =
    match items with
    | [] -> failwith "empty segments"
    | [i] -> i
    | _ ->
      let segments = to_list_expr ~loc items in
      [%expr String.concat [%e Ast.estring between] [%e segments]]
  in
  concat

(** Whether a particular AST construct should have its subexpressions parenthesized *)
let skip_adding_parens has_prec = not has_prec

(** Given a datatype and a list of constructor declarations, generates a printer function *)
let generate_printer_variant ~loc config type_name constr_decls =
  let (module Ast) = Ast_builder.make loc in
  let prefix = config.fn ^ "_" in
  let fn_name = prefix ^ type_name in
  (* TODO mutually recursive *)
  let rec_flag = Recursive in
  let unparse_fn =
    let cases =
      constr_decls
      |> List.map (fun constr ->
             (* each constructor maps to a branch in the case split *)
             let form = get_form ~loc:constr.pcd_loc constr.pcd_attributes in
             let has_prec =
               Option.is_some (constructor_prec_spec ~loc constr)
             in
             let match_rhs =
               match constr.pcd_args with
               | Pcstr_tuple xs ->
                 (* print each variable *)
                 let vars, rendered =
                   List.mapi
                     (fun i typ -> typed_printed_expr ~prefix ~loc i typ)
                     xs
                   |> List.split
                 in
                 interpret_template_expr loc type_name
                   (skip_adding_parens has_prec)
                   vars rendered form config.between
               | _ -> failwith "nyi non tuple rhs"
             in
             let pattern_vars =
               match constr.pcd_args with
               | Pcstr_tuple xs ->
                 List.mapi
                   (fun i _typ ->
                     Ast.ppat_var { loc; txt = Format.sprintf "v%d" i })
                   xs
               | _ -> failwith "nyi non tuple lhs"
             in
             Ast.case ~guard:None
               ~lhs:
                 (Ast.ppat_construct
                    { loc; txt = Lident constr.pcd_name.txt }
                    (match pattern_vars with
                    | [] -> None
                    | _ -> Some (Ast.ppat_tuple pattern_vars)))
               ~rhs:match_rhs)
    in
    Ast.pstr_value rec_flag
      [
        Ast.value_binding
          ~pat:(Ast.ppat_var { loc; txt = fn_name })
          ~expr:(generate_case_split_fn loc cases);
      ]
  in
  unparse_fn

let generate_precedence_match_table ~loc name cstrs =
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

let latex_cmd_name type_name constr_name =
  Format.asprintf "%s%s" (String.sub type_name 0 2)
    (String.lowercase_ascii constr_name)

let generate_latex_form loc name n =
  let s = Ast.estring ~loc in
  let id name = Ast.pexp_ident ~loc { loc; txt = Lident (template_var name) } in
  List.init n (fun i ->
      (match i with
      | 0 -> [s ("\\" ^ name ^ "{"); id (i + 1)]
      | _ -> [s "}{"; id (i + 1)])
      @ if i = n - 1 then [s "}"] else [])
  |> List.concat

let generate_latex_variant ~loc type_name constr_decls =
  let (module Ast) = Ast_builder.make loc in
  let cmd_prefix = "latex_commands_" in
  let prefix = "latex_unparse_" in
  let latex_cmds =
    let templates =
      constr_decls
      |> List.map (fun constr ->
             let form =
               get_form ~latex:true ~loc:constr.pcd_loc constr.pcd_attributes
               |> parse_template
             in
             let templ =
               List.map
                 (fun f ->
                   match f with
                   | Var i -> Format.asprintf "#%d" (i + 1)
                   | Constant s -> s)
                 form
               |> String.concat " "
             in

             let raw =
               match constr.pcd_args with
               | Pcstr_tuple xs ->
                 Format.asprintf "\\newcommand*{\\%s}[%d]{%s}"
                   (latex_cmd_name type_name constr.pcd_name.txt)
                   (List.length xs) templ
               | _ -> failwith "nyi non tuple rhs"
             in
             raw)
      |> String.concat "\n" |> Ast.estring
    in
    let rec_flag = Nonrecursive in
    Ast.pstr_value rec_flag
      [
        Ast.value_binding
          ~pat:(Ast.ppat_var { loc; txt = cmd_prefix ^ type_name })
          ~expr:templates;
      ]
  in
  let latex_print_fn =
    let cases =
      constr_decls
      |> List.map (fun constr ->
             (* each constructor maps to a branch in the case split *)
             let has_prec =
               Option.is_some (constructor_prec_spec ~loc constr)
             in
             let match_rhs =
               match constr.pcd_args with
               | Pcstr_tuple xs ->
                 let form =
                   generate_latex_form loc
                     (latex_cmd_name type_name constr.pcd_name.txt)
                     (List.length xs)
                 in
                 (* print each variable *)
                 let vars, rendered =
                   xs
                   |> List.mapi (fun i typ ->
                          typed_printed_expr ~prefix ~loc i typ)
                   |> List.split
                 in
                 (* substitute variables into template *)
                 interpret_template_expr loc type_name
                   (skip_adding_parens has_prec)
                   vars rendered form ""
               | _ -> failwith "nyi non tuple rhs"
             in
             let pattern_vars =
               match constr.pcd_args with
               | Pcstr_tuple xs ->
                 List.mapi
                   (fun i _typ ->
                     Ast.ppat_var { loc; txt = Format.sprintf "v%d" i })
                   xs
               | _ -> failwith "nyi non tuple lhs"
             in
             Ast.case ~guard:None
               ~lhs:
                 (Ast.ppat_construct
                    { loc; txt = Lident constr.pcd_name.txt }
                    (match pattern_vars with
                    | [] -> None
                    | _ -> Some (Ast.ppat_tuple pattern_vars)))
               ~rhs:match_rhs)
    in
    (* TODO mutually recursive *)
    let rec_flag = Recursive in
    Ast.pstr_value rec_flag
      [
        Ast.value_binding
          ~pat:(Ast.ppat_var { loc; txt = prefix ^ type_name })
          ~expr:(generate_case_split_fn loc cases);
      ]
  in
  [latex_print_fn; latex_cmds]

(** For various other kinds of types, like tuples, type aliases *)
let generate_printer_type ~loc config name typ =
  let (module Ast) = Ast_builder.make loc in
  let prefix = config.fn ^ "_" in
  let fn_name = prefix ^ name in
  let rec_flag = Nonrecursive in
  let cases =
    let types, left =
      match typ.ptyp_desc with
      | Ptyp_tuple elts ->
        let vars =
          List.mapi
            (fun i _ -> Ast.ppat_var { loc; txt = Format.sprintf "v%d" i })
            elts
        in
        (elts, Ast.ppat_tuple vars)
      | Ptyp_constr (_cons, _args) -> ([typ], Ast.ppat_var { loc; txt = "v0" })
      | _ -> failwith "unknown other kind of type"
    in
    let form =
      get_form ~loc:typ.ptyp_loc typ.ptyp_attributes |> parse_template
    in
    let right =
      (* print each variable *)
      let rendered =
        List.mapi (fun i typ -> typed_printed_expr ~prefix ~loc i typ) types
        |> List.map snd
      in
      (* substitute variables into template *)
      let items =
        form
        |> List.map (fun f ->
               match f with
               | Var i ->
                 (* let var = List.nth vars var_i in *)
                 let printed = List.nth rendered i in
                 printed
                 (* var *)
               | Constant s -> Ast.estring s)
      in
      (* produce the body of each case split *)
      let concat =
        match items with
        | [] -> failwith "empty segments"
        | [i] -> i
        | _ ->
          let segments = to_list_expr ~loc items in
          [%expr String.concat [%e Ast.estring config.between] [%e segments]]
      in
      concat
    in
    [Ast.case ~guard:None ~lhs:left ~rhs:right]
  in
  Ast.pstr_value rec_flag
    [
      Ast.value_binding
        ~pat:(Ast.ppat_var { loc; txt = fn_name })
        ~expr:(generate_case_split_fn loc cases);
    ]

let generate_type_decl config t =
  (* TODO mutually recursive types *)
  let td = List.hd t in
  let { loc; txt = name } = td.ptype_name in
  match td.ptype_kind with
  | Ptype_variant cstrs ->
    [
      generate_precedence_match_table ~loc name cstrs;
      generate_printer_variant ~loc config name cstrs;
    ]
    @ if config.latex then generate_latex_variant ~loc name cstrs else []
  | Ptype_abstract ->
    begin
      match td.ptype_manifest with
      | None -> error ~loc "cannot generate for abstract type"
      | Some typ -> [generate_printer_type ~loc config name typ]
    end
  | Ptype_record _ | Ptype_open -> failwith "nyi non variant ptype kind"

let str_gen ~loc:_ ~path:_ (_rec, t) between fn latex =
  let config =
    {
      between = Option.value ~default:"" between;
      fn = Option.value ~default:"unparse" fn;
      latex;
    }
  in
  try
    let extra = generate_type_decl config t in
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
  (* let x =
       let open Ast_pattern in
       pexp_construct (lident (string "bool")) none
     in *)
  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(
        empty
        +> arg "between" (estring __)
        +> arg "fn" (estring __)
        +> flag "latex")
      str_gen
  in
  let sig_type_decl = Deriving.Generator.make_noarg sig_gen in
  Deriving.add ppx_name ~str_type_decl ~sig_type_decl |> Deriving.ignore
