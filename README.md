[![Build Status](https://travis-ci.org/cryptosense/ocamllint.svg)](https://travis-ci.org/cryptosense/ocamllint)

Ocamllint - detect common errors in OCaml code
==============================================

What is ocamllint?
------------------

Ocamllint is a tool to find problematic constructions in OCaml code.

For example, consider the following piece of code:

```ocaml
let print_summary solutions =
  if List.length solutions = 0 then
    print_endline "No solutions"
  else
    print_endline "Some solutions were found"
```

This piece of code is correct, but it is doing too much work: if the `solutions`
list is long, it will take a long time to compute its length, only to check
whether this length is zero.

When compiled with ocamllint, a warning message will be displayed:

```
File "example.ml", line 2, characters 5-30:
(ocamllint) Use = []
```

Indeed, a more efficient way to do it is

```ocaml
let print_summary solutions =
  if solutions = [] then
    print_endline "No solutions"
  else
    print_endline "Some solutions were found"
```

Or using pattern matching:

```ocaml
let print_summary solutions =
  match solutions with
  | [] -> print_endline "No solutions"
  | _::_ -> print_endline "Some solutions were found"
```

Neither of these versions will trigger the above warning.

What is ocamllint NOT?
----------------------

Due to the way it is implemented, `ocamllint` does not deal with concrete
syntax. This means that it can not detect individual tokens, only abstract
syntax.

How to use ocamllint
--------------------

First you need to install ocamllint as an ocamlfind package. `make` followed by
`make install` will do the right thing. An OPAM package is also in the works.

Then you only need to pass `-package ocamllint.ppx` to your build system.
If you are using `ocamlbuild`, adding `true: package(ocamllint.ppx)` to
your `_tags` file should work.

Contributing
------------

New rules are welcome! Write a test in `tests.ml`, a rule in `rules.ml` and you
should be set.

There is no configuration system at the moment to enable/disable rules, nor a
way to dynamically load custom rules tied to your own project, but these
features are definitely on the roadmap. Please open a bug first before
implementing this kind of feature.
