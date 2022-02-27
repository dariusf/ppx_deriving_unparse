
# deriving_unparse

## What's this?

Annotate your AST like this

```ocaml
type expr =
  | Num of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@prec left, 4]
[@@deriving unparse { between = " " }]
```

and a printer which knows how to parenthesize expressions properly is generated.

```ocaml
let () =
  let e = Times (Plus (Num 1, Num 2), Num 3) in
  assert ((unparse_expr e) = "(1 + 2) * 3")
```

## Basics

`[@form <e1> <e2> ...]` goes on constructors.
The payload is a sequence of expressions. String literals are printed as is. Variables starting with `_` refer to positional or record arguments.

`[@prec <fixity>, <precedence>]` also goes on constructors.
`fixity` is one of `left`, `right`, `nonassoc`, `prefix`, or `postfix`.
`precedence` is a natural number.
If a constructor is not annotated, its arguments will not be parenthesized (as it is presumably not an operator).

A number of options may be passed to the plugin.

- the string `between` is inserted between every expression in `[@form ...]`
- `fn` controls what the generated function is named
