# Important

We're archiving `ocamllint` as it's not used at Cryptosense anymore and we believe
there are better and more flexible lint tools available than a standalone binary.

If you think otherwise and are interested in maintaining it, please don't hesitate to
contact us at github@cryptosense.com.


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

How to configure ocamllint
--------------------------

Similarly to how `ocamlbuild` works, it is necessary to write a plugin to modify
the configuration. It is a dynamically-linked file from which one has to call
`Ocamllint.Plugin.set_config`. The `example_plugin.ml` file is, well, an
example. This plugin disables a warning. It can be built using `make example`.

When the ppx rewriter is loaded without an argument, it will use a default
configuration:

```
% ocamlfind ppx_tools/rewriter -ppx "ocamlfind ocamllint/ppx_lint.native" -str
'let y = []@[] in let x = 3 in x'
File "", line 1, characters 17-31:
(ocamllint) Useless let binding
File "", line 1, characters 8-13:
(ocamllint) List operation on litteral: @
let _ = let y = [] @ [] in let x = 3 in x
```

If you pass an option to the rewriter, it will load the configuration:

```
% ocamlfind ppx_tools/rewriter -ppx "ocamlfind ocamllint/ppx_lint.native
_build/example_plugin.cmxs" -str 'let y = []@[] in let x = 3 in x'
File "", line 1, characters 8-13:
(ocamllint) List operation on litteral: @
let _ = let y = [] @ [] in let x = 3 in x
```

Contributing
------------

New rules are welcome! Write a test in `tests.ml`, a rule in `rules.ml` and you
should be set.
