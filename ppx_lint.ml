open Ast_mapper
open Asttypes
open Parsetree

let with_def_use = false

let report_warning ~loc msg =
  let msg = Printf.sprintf "(ocamllint) %s\n" msg in
  let err = Location.error ~loc msg in
  Location.report_error Format.std_formatter err

let report_def ~loc name =
  Printf.printf "DEF: %s " name;
  Location.print_loc Format.std_formatter loc;
  Printf.printf "\n"

let report_use name =
  Printf.printf "USE: %s\n" name

let track_def ~loc name =
  if with_def_use then
    report_def ~loc name

let track_use expr =
  if with_def_use then
    match expr.pexp_desc with
    | Pexp_ident lid -> report_use (Longident.last lid.txt)
    | _ -> ()

let warn_on ~loc = function
  | Some warning -> report_warning ~loc warning
  | None -> ()

let handle expr =
  let loc = expr.pexp_loc in
  track_use expr;
  warn_on ~loc (Rules.rate_expression expr)

let handle_module_binding mb =
  let loc = mb.pmb_loc in
  let name = mb.pmb_name.txt in
  warn_on ~loc (Rules.rate_module_name name)

let handle_module_type_declaration mtd =
  let loc = mtd.pmtd_loc in
  let name = mtd.pmtd_name.txt in
  warn_on ~loc (Rules.rate_module_type_name name)

let handle_signature_item si =
  match si.psig_desc with
  | Psig_value vd -> begin
      let loc = vd.pval_loc in
      track_def ~loc vd.pval_name.txt
    end
  | _ -> ()

let handle_structure_item si =
  match si.pstr_desc with
  | Pstr_value (_, bindings) ->
      List.iter (function
        | { pvb_pat = { ppat_desc = Ppat_var name ; ppat_loc = loc } } -> track_def ~loc name.txt
        | _ -> ()
      ) bindings
  | _ -> ()

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
    signature_item = (fun mapper signature_item ->
      handle_signature_item signature_item;
      default_mapper.signature_item mapper signature_item);
    structure_item = (fun mapper structure_item ->
      handle_structure_item structure_item;
      default_mapper.structure_item mapper structure_item);
  }

let () = register "lint" lint_mapper
