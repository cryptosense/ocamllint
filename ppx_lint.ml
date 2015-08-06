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

let rec li_eq li1 li2 =
  let open Longident in
  match (li1, li2) with
  | Lident s1, Lident s2 -> s1 = s2
  | Ldot (i1, s1), Ldot (i2, s2) -> li_eq i1 i2 && s1 = s2
  | Lapply (lia1, lib1), Lapply (lia2, lib2) ->
      li_eq lia1 lia2 && li_eq lib1 lib2
  | _ -> false

(** Syntactic equality *)
let rec expr_eq e1 e2 =
  match e1.pexp_desc, e2.pexp_desc with
  | Pexp_constant c1, Pexp_constant c2 -> c1 = c2
  | Pexp_ident i1, Pexp_ident i2 -> i1.txt = i2.txt
  | Pexp_construct ({ txt = li1 }, eo1), Pexp_construct ({ txt = li2 }, eo2) ->
      li_eq li1 li2 && expr_option_eq eo1 eo2
  | Pexp_variant (l1, eo1), Pexp_variant (l2, eo2) ->
      l1 = l2 && expr_option_eq eo1 eo2
  | Pexp_field (e1, { txt = li1 }), Pexp_field (e2, { txt = li2 }) ->
      expr_eq e1 e2 && li_eq li1 li2
  | Pexp_array el1, Pexp_array el2
  | Pexp_tuple el1, Pexp_tuple el2
    -> expr_list_eq el1 el2
  | _ -> false

and expr_option_eq eo1 eo2 = match (eo1, eo2) with
  | Some e1, Some e2 -> expr_eq e1 e2
  | None, None -> true
  | _ -> false

and expr_list_eq el1 el2 =
  try
    List.for_all2 expr_eq el1 el2
  with Invalid_argument _ -> false

(** Detect when a pattern correspond to an expression, as in Some x -> x. *)
let pat_is_exp p e =
  match p.ppat_desc, e.pexp_desc with
  | Ppat_var { txt = pat_var }, Pexp_ident { txt = Longident.Lident exp_var } ->
      pat_var = exp_var
  | _ -> false

(** Evaluation of this expression is effect-free. *)
let rec is_pure e =
  match e.pexp_desc with
  | Pexp_ident _
  | Pexp_constant _
    -> true
  | Pexp_construct (_, eo)
  | Pexp_variant (_, eo)
    -> is_pure_option eo
  | Pexp_field (e, _)
    -> is_pure e
  | Pexp_array es
  | Pexp_tuple es ->
      List.for_all is_pure es
  | _ -> false

and is_pure_option = function
  | None -> true
  | Some e -> is_pure e

let all_branches_same expr = match expr.pexp_desc with
  | Pexp_match (_, cases) ->
      begin
        let exprs = List.map (fun c -> c.pc_rhs) cases in
        match exprs with
        | [] -> false
        | [_] -> false
        | h::t -> List.for_all (fun e -> expr_eq h e) t
      end
  | _ -> false

let match_on_const expr = match expr.pexp_desc with
  | Pexp_match (e, _) ->
      begin
        match e.pexp_desc with
        | Pexp_constant _ -> true
        | Pexp_construct _ -> true
        | Pexp_variant _ -> true
        | _ -> false
      end
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
  | ( [%expr match [%e? _] with | None -> [%e? def] | Some [%p? p] -> [%e? e]]
    | [%expr match [%e? _] with | Some [%p? p] -> [%e? e] | None -> [%e? def]]
    )
    when is_pure def && pat_is_exp p e ->
      report_warning ~loc "Use Option.default"
  | [%expr if [%e? _] then [%e? e1] else [%e? e2]] when expr_eq e1 e2 ->
      report_warning ~loc "Useless if"
  | _ when all_branches_same expr ->
      report_warning ~loc "Useless match"
  | _ when match_on_const expr ->
      report_warning ~loc "Match on constant or constructor"
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
