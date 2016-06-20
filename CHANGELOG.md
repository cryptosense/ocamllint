## unreleased

- Only run when a compiler is running. Makes it possible to use directly as
  `-package ocamllint.ppx` (#12)
- Support OCaml 4.03.0

## v0.2.0

*2016-03-10*

(This release contains breaking changes, indicated by a star)

- Use a `Warning.t` type instead of plain strings
* Rename findlib package `ppx_lint` to `ppx` (#6)
- Add a configuration system based on plugins

## v0.1.0

*2016-01-07*

- Initial release
