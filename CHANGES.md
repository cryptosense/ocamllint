## unreleased

- Support OCaml 4.04.0+beta1 (#17, #18, thanks Damien Doligez!)

## v0.3.0

*2016-06-27*

- Only run when a compiler is running. Makes it possible to use directly as
  `-package ocamllint.ppx` (#12)
- Suggest using `function` instead of `fun` + `match` (#10)
- Support OCaml 4.03.0
- Detect typos in documentation (requires OCaml 4.03)

## v0.2.0

*2016-03-10*

(This release contains breaking changes, indicated by a star)

- Use a `Warning.t` type instead of plain strings
* Rename findlib package `ppx_lint` to `ppx` (#6)
- Add a configuration system based on plugins

## v0.1.0

*2016-01-07*

- Initial release
