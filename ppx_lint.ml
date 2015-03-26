open Ast_mapper
open Parsetree

let report_warning ~loc msg =
  let msg = Printf.sprintf "(ocamllint) %s\n" msg in
  let err = Location.error ~loc msg in
  Location.report_error Format.std_formatter err

let handle expr =
  let loc = expr.pexp_loc in
  match expr with
  | [%expr String.concat [%e? s] [ [%e? l] ] ] -> report_warning ~loc "String.concat on singleton"
  | [%expr List.map [%e? _] [ [%e? _] ] ] -> report_warning ~loc "List.map on singleton"
  | [%expr List.fold_left [%e? _] [%e? _] [ [%e? _] ] ] -> report_warning ~loc "List.fold_left on singleton"
  | [%expr List.fold_right [%e? _] [ [%e? _] ] [%e? _] ] -> report_warning ~loc "List.fold_right on singleton"
  | [%expr if [%e? _] then true else false ] -> report_warning ~loc "Useless if"
  | [%expr if [%e? _] then () else [%e? _]] -> report_warning ~loc "Backwards if"
  | [%expr if [%e? _] then [%e? _] else ()] -> report_warning ~loc "Useless else"
  | [%expr List.hd ] -> report_warning ~loc "Use of partial function List.hd"
  | [%expr List.tl ] -> report_warning ~loc "Use of partial function List.tl"
  | [%expr String.sub [%e? s] 0 [%e? n] ] -> report_warning ~loc "Use Str.first_chars"
  | [%expr List.length [%e? _] > 0 ] -> report_warning ~loc "Use <> []"
  | [%expr List.length [%e? _] = 0 ] -> report_warning ~loc "Use = []"
  | [%expr [%e? _] = true]
  | [%expr [%e? _] = false]
  | [%expr [%e? _] == true]
  | [%expr [%e? _] == false]
  | [%expr [%e? _] <> true]
  | [%expr [%e? _] != false] -> report_warning ~loc "Useless comparison to boolean"
  | _ -> ()

let lint_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      handle expr;
      default_mapper.expr mapper expr
  }

let () = register "lint" lint_mapper
