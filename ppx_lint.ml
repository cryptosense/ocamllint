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

(** Syntactic equality *)
let expr_eq e1 e2 =
  match e1.pexp_desc, e2.pexp_desc with
  | Pexp_constant c1, Pexp_constant c2 -> c1 = c2
  | Pexp_ident i1, Pexp_ident i2 -> i1.txt = i2.txt
  | _ -> false

let is_uppercase s =
  s = String.uppercase s

let is_lowercase s =
  s = String.lowercase s

let is_snake_case name =
  let all_but_first = Str.string_after name 1 in
  let words = Str.split (Str.regexp "_") all_but_first in
  List.for_all (fun word ->
    is_lowercase word || is_uppercase word
  ) words

let check_module_name ~loc name =
  if not (is_snake_case name) then
    report_warning ~loc ("Module name not in snake case: " ^ name)

let track_def ~loc name =
  if with_def_use then
    report_def ~loc name

let track_use expr =
  if with_def_use then
    match expr.pexp_desc with
    | Pexp_ident lid -> report_use (Longident.last lid.txt)
    | _ -> ()

let handle expr =
  let loc = expr.pexp_loc in
  track_use expr;
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
  | [%expr String.sub [%e? s] [%e? n] (String.length [%e? s'] - [%e? n']) ]
    when expr_eq s s' && expr_eq n n'
    -> report_warning ~loc "Use Str.string_after"
  | [%expr String.sub [%e? s] (String.length [%e? s'] - [%e? n]) [%e? n'] ]
    when expr_eq s s' && expr_eq n n'
    -> report_warning ~loc "Use Str.last_chars"
  | [%expr List.length [%e? _] > 0 ] -> report_warning ~loc "Use <> []"
  | [%expr List.length [%e? _] = 0 ] -> report_warning ~loc "Use = []"
  | [%expr [%e? _] = true]
  | [%expr [%e? _] = false]
  | [%expr [%e? _] == true]
  | [%expr [%e? _] == false]
  | [%expr [%e? _] <> true]
  | [%expr [%e? _] != false] -> report_warning ~loc "Useless comparison to boolean"
  | [%expr [%e? _] |> [%e? { pexp_desc = Pexp_fun _} ]]
  | [%expr [%e? { pexp_desc = Pexp_fun _} ] @@ [%e? _]] -> report_warning ~loc "Use let/in"
  | { pexp_desc = Pexp_letmodule ({ txt }, _, _) } -> check_module_name ~loc txt
  | [%expr [%e? e1] := ![%e? e2] + 1] when expr_eq e1 e2 -> report_warning ~loc "Use incr"
  | [%expr [%e? e1] := ![%e? e2] - 1] when expr_eq e1 e2 -> report_warning ~loc "Use decr"
  | _ -> ()

let handle_module_binding mb =
  let loc = mb.pmb_loc in
  let name = mb.pmb_name.txt in
  check_module_name ~loc name

let handle_module_type_declaration mtd =
  let loc = mtd.pmtd_loc in
  let name = mtd.pmtd_name.txt in
  if not (is_uppercase name) then
    report_warning ~loc ("Module type name not uppercase : " ^ name)

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
