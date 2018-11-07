open Migrate_parsetree.Ast_404.Ast_mapper
open Migrate_parsetree.Ast_404.Asttypes
open Migrate_parsetree.Ast_404.Parsetree

module Config : sig
  val reset_args : unit -> unit

  val args : (Arg.key * Arg.spec * Arg.doc) list

  val plugin : unit -> string option
end = struct
  let plugin_ref = ref None

  let set_plugin s =
    plugin_ref := Some s

  let plugin () =
    !plugin_ref

  let reset_args () =
    plugin_ref := None

  let args =
    [ ("--plugin", Arg.String set_plugin, "Load plugin")
    ]
end

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
  warn_on config ~loc (Ocamllint_rules.rate_expression expr)

let handle_module_binding config mb =
  let loc = mb.pmb_loc in
  let name = mb.pmb_name.txt in
  warn_on config ~loc (Ocamllint_rules.rate_module_name name)

let handle_module_type_declaration config mtd =
  let loc = mtd.pmtd_loc in
  let name = mtd.pmtd_name.txt in
  warn_on config ~loc (Ocamllint_rules.rate_module_type_name name)

let handle_signature_item config sigitem =
  let loc = sigitem.psig_loc in
  warn_on config ~loc (Ocamllint_rules.rate_signature_item sigitem)

let should_run {Migrate_parsetree.Driver.tool_name} =
  match tool_name with
  | "ocamlc" -> true
  | "ocamlopt" -> true
  | _ -> false

let lint_mapper driver_config _ =
  let config = match Config.plugin () with
    | None -> Ocamllint.Config.default
    | Some config_module -> load_config config_module
  in
  if should_run driver_config then
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
      signature_item = (fun mapper signature_item ->
        handle_signature_item config signature_item;
        default_mapper.signature_item mapper signature_item);
    }
  else
    default_mapper

let main () =
  Migrate_parsetree.Driver.register
    ~name:"lint"
    ~reset_args:Config.reset_args
    ~args:Config.args
    Migrate_parsetree.Versions.ocaml_404
    lint_mapper;
  Migrate_parsetree.Driver.run_main ()

let () = main ()
