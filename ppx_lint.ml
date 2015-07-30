open Ast_mapper
open Asttypes
open Parsetree

let report_warning ~loc msg =
  let msg = Printf.sprintf "(ocamllint) %s\n" msg in
  let err = Location.error ~loc msg in
  Location.report_error Format.std_formatter err

(** Syntactic equality *)
let expr_eq e1 e2 =
  match e1.pexp_desc, e2.pexp_desc with
  | Pexp_constant c1, Pexp_constant c2 -> c1 = c2
  | Pexp_ident i1, Pexp_ident i2 -> i1.txt = i2.txt
  | _ -> false

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
  | [%expr String.sub [%e? s] [%e? n] (String.length [%e? s'] - [%e? n']) ]
    when expr_eq s s' && expr_eq n n'
    -> report_warning ~loc "Use Str.string_after"
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
  | _ -> ()

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

let handle_module_binding mb =
  let loc = mb.pmb_loc in
  let name = mb.pmb_name.txt in
  if not (is_snake_case name) then
    report_warning ~loc ("Module name not in snake case: " ^ name)

let handle_module_type_declaration mtd =
  let loc = mtd.pmtd_loc in
  let name = mtd.pmtd_name.txt in
  if not (is_uppercase name) then
    report_warning ~loc ("Module type name not uppercase : " ^ name)

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
