open Ast_mapper
open Asttypes
open Parsetree

let raise_errorf ?sub ?if_highlight ?loc message =
  message |> Printf.kprintf (fun str ->
    let err = Location.error ?sub ?if_highlight ?loc str in
    raise (Location.Error err))

let dynlink ?(loc=Location.none) filename =
  let filename = Dynlink.adapt_filename filename in
  try
    Dynlink.loadfile filename
  with Dynlink.Error error ->
    raise_errorf ~loc "Cannot load %s: %s" filename (Dynlink.error_message error)

let load_config filename =
  dynlink filename;
  Ocamllint.Plugin.get_config ()

let report_warning ~loc warn =
  let msg = Printf.sprintf "(ocamllint) %s\n" (Ocamllint.Warning.to_string warn) in
  let err = Location.error ~loc msg in
  Location.report_error Format.std_formatter err

let warn_on config ~loc wo =
  let context = Ocamllint.Context.make ~loc () in
  match wo with
  | Some warning when Ocamllint.Config.warning_active config context warning ->
      report_warning ~loc warning
  | _ -> ()

let handle config expr =
  let loc = expr.pexp_loc in
  warn_on config ~loc (Rules.rate_expression expr)

let handle_module_binding config mb =
  let loc = mb.pmb_loc in
  let name = mb.pmb_name.txt in
  warn_on config ~loc (Rules.rate_module_name name)

let handle_module_type_declaration config mtd =
  let loc = mtd.pmtd_loc in
  let name = mtd.pmtd_name.txt in
  warn_on config ~loc (Rules.rate_module_type_name name)

let should_run () =
  match tool_name () with
  | "ocamlc" -> true
  | "ocamlopt" -> true
  | _ -> false

let lint_mapper argv =
  let config = match argv with
    | [] -> Ocamllint.Config.default
    | [config_module] -> load_config config_module
    | _ -> raise_errorf "Only one plugin can be loaded"
  in
  if should_run () then
    { default_mapper with
      expr = (fun mapper expr ->
        handle config expr;
        default_mapper.expr mapper expr);
      module_type_declaration = (fun mapper mtd ->
        handle_module_type_declaration config mtd;
        default_mapper.module_type_declaration mapper mtd);
      module_binding = (fun mapper module_binding ->
        handle_module_binding config module_binding;
        default_mapper.module_binding mapper module_binding);
    }
  else
    default_mapper

let () = register "lint" lint_mapper
