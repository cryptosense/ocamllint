open Ast_mapper
open Parsetree

let handle expr =
  match expr with
  | [%expr String.concat [%e? s] [ [%e? l] ] ] -> print_endline "warning2"
  | _ -> ()

let lint_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      handle expr;
      default_mapper.expr mapper expr
  }

let () = register "lint" lint_mapper
