open Ast_mapper
open Asttypes
open Parsetree

let report_warning ~loc warn =
  let msg = Printf.sprintf "(ocamllint) %s\n" (Warning.to_string warn) in
  let err = Location.error ~loc msg in
  Location.report_error Format.std_formatter err

let warn_on ~loc = function
  | Some warning -> report_warning ~loc warning
  | None -> ()

let handle expr =
  let loc = expr.pexp_loc in
  warn_on ~loc (Rules.rate_expression expr)

let handle_module_binding mb =
  let loc = mb.pmb_loc in
  let name = mb.pmb_name.txt in
  warn_on ~loc (Rules.rate_module_name name)

let handle_module_type_declaration mtd =
  let loc = mtd.pmtd_loc in
  let name = mtd.pmtd_name.txt in
  warn_on ~loc (Rules.rate_module_type_name name)

let lint_mapper argv =
  { default_mapper with
    expr = (fun mapper expr ->
      handle expr;
      default_mapper.expr mapper expr);
    module_type_declaration = (fun mapper mtd ->
      handle_module_type_declaration mtd;
      default_mapper.module_type_declaration mapper mtd);
    module_binding = (fun mapper module_binding ->
      handle_module_binding module_binding;
      default_mapper.module_binding mapper module_binding);
  }

let () = register "lint" lint_mapper
