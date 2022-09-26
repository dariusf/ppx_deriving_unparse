
# ppx_deriving_unparse

Are you tired of writing pretty-printers?!

Annotate your AST type like this:

```ocaml
type expr =
  | Num of int [@form _1]
  | Plus of expr * expr [@form _1 "+" _2] [@prec left, 3]
  | Times of expr * expr [@form _1 "*" _2] [@prec left, 4]
[@@deriving unparse { between = " " }]
```

and a printer which parenthesizes expressions properly is generated!

```ocaml
let () =
  let e = Times (Plus (Num 1, Num 2), Num 3) in
  assert ((unparse_expr e) = "(1 + 2) * 3")
```

## Basics

`[@form <e1> <e2> ...]` goes on constructors, to specify their pretty-printed form.
The payload is a sequence of expressions. String literals are printed as is. Variables starting with `_` refer to positional or record arguments.

`[@prec <fixity>, <precedence>]` also goes on constructors.
`fixity` is one of `left`, `right`, `nonassoc`, `prefix`, or `postfix`.
`precedence` is a natural number.
If a constructor is not annotated, its arguments will not be parenthesized (as it is presumably not an operator).

A number of options may be passed to the deriver.

- the string `between` is inserted between every expression in `[@form ...]`
- `fn` controls what the generated function is named

## Background

This is an implementation of the classic paper *Unparsing expressions with prefix and postfix operators* (1998).

It's unproven beyond the small examples in the tests. I might pick it up again the next time I have to write a pretty-printer...