open Ast_tools
open Asttypes
open Parsetree

(** Allocated litterals *)
let is_allocated_lit exp =
  match exp.pexp_desc with
  | Pexp_constant c ->
    begin
      let open Ast_convenience.Constant in
      match of_constant c with
      | Pconst_string _ -> true
      | _ -> false
    end
  | Pexp_tuple _
  | Pexp_construct (_, Some _)
  | Pexp_variant (_, Some _)
  | Pexp_array _
  | Pexp_record _
      -> true
  | _ -> false

let is_phys_eq = function
  | [%expr (==)]
  | [%expr (!=)]
    -> true
  | _ -> false

let rec is_list_lit = function
  | [%expr []] -> true
  | [%expr [%e? _]::[%e? t]] -> is_list_lit t
  | _ -> false

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
  | Pexp_match ([%expr ()], _) -> false
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

module Lid_set = Set.Make (struct
  type t = Longident.t
  let compare = Pervasives.compare
end)

let partial_functions =
  Lid_set.of_list @@ List.map Longident.parse
    [ "List.hd"
    ; "List.tl"
    ]

let lid_to_string lid =
  String.concat "." @@ Longident.flatten lid

let is_partial_function lid =
  Lid_set.mem lid partial_functions

let rate_module_name name =
  let open Ocamllint.Warning in
  if is_snake_case name then
    None
  else
    Some (Module_name_not_snake_case name)

let rec is_explicit_catch_of_sys_break = function
  | [%pat? Sys.Break] -> true
  | { ppat_desc = Ppat_alias (p, _) } -> is_explicit_catch_of_sys_break p
  | [%pat? [%p? p1] | [%p? p2]] -> is_explicit_catch_of_sys_break p1 ||
                                   is_explicit_catch_of_sys_break p2
  | _ -> false

let is_wildcard case =
  match case.pc_lhs.ppat_desc with
  | Ppat_any -> true
  | Ppat_var _ -> true
  | _ -> false

let sys_break_implicitly_caught cases =
  let explicitly_caught =
    List.exists (fun case -> is_explicit_catch_of_sys_break case.pc_lhs) cases
  in
  not explicitly_caught && List.exists is_wildcard cases

let rec rate_expression =
  let open Ocamllint.Warning in
  function
  | [%expr [%e? e1] @@ [%e? e2]]
  | [%expr [%e? e2] |> [%e? e1]] -> rate_expression [%expr [%e e1] [%e e2]]
  | [%expr String.concat [%e? s] [ [%e? l] ] ] -> Some (List_function_on_singleton "String.concat")
  | [%expr List.map [%e? _] [ [%e? _] ] ] -> Some (List_function_on_singleton "List.map")
  | [%expr List.fold_left [%e? _] [%e? _] [ [%e? _] ] ] -> Some (List_function_on_singleton "List.fold_left")
  | [%expr List.fold_right [%e? _] [ [%e? _] ] [%e? _] ] -> Some (List_function_on_singleton "List.fold_right")
  | [%expr if [%e? _] then true else false ] -> Some Useless_if
  | [%expr if [%e? _] then () else [%e? _]] -> Some Backwards_if
  | [%expr if [%e? _] then [%e? _] else ()] -> Some Useless_else
  | { pexp_desc = Pexp_ident { txt = lid } }
    when is_partial_function lid
    -> Some (Partial_function (lid_to_string lid))
  | [%expr String.sub [%e? s] 0 [%e? n] ] -> Some (Inlined_function "Str.first_chars")
  | [%expr String.sub [%e? s] [%e? n] (String.length [%e? s'] - [%e? n']) ]
    when expr_eq s s' && expr_eq n n'
    -> Some (Inlined_function "Str.string_after")
  | [%expr String.sub [%e? s] (String.length [%e? s'] - [%e? n]) [%e? n'] ]
    when expr_eq s s' && expr_eq n n'
    -> Some (Inlined_function "Str.last_chars")
  | [%expr List.length [%e? _] > 0 ] -> Some (Empty_list_test "<>")
  | [%expr List.length [%e? _] = 0 ] -> Some (Empty_list_test "=")
  | [%expr [%e? _] = true]
  | [%expr [%e? _] = false]
  | [%expr [%e? _] == true]
  | [%expr [%e? _] == false]
  | [%expr [%e? _] <> true]
  | [%expr [%e? _] != false] -> Some Comparison_to_boolean
  | [%expr [%e? { pexp_desc = Pexp_fun _} ] [%e? _]] -> Some Abstract_apply
  | { pexp_desc = Pexp_letmodule ({ txt }, _, _) } -> rate_module_name txt
  | [%expr [%e? e1] := ![%e? e2] + 1] when expr_eq e1 e2 -> Some (Inlined_function "incr")
  | [%expr [%e? e1] := ![%e? e2] - 1] when expr_eq e1 e2 -> Some (Inlined_function "decr")
  | [%expr if [%e? _] then [%e? e1] else [%e? e2]] when expr_eq e1 e2 ->
      Some Constant_if
  | expr when all_branches_same expr ->
      Some Constant_match
  | expr when match_on_const expr ->
      Some Match_on_constructor
  | [%expr [%e? f] [%e? e1] [%e? e2]]
      when is_phys_eq f && (is_allocated_lit e1 || is_allocated_lit e2) ->
      Some Physical_comparison_on_allocated_litteral
  | [%expr let _ = List.map [%e? _] [%e? _] in [%e? _]] ->
      Some (Discarded_result ("List.map", "List.iter"))
  | [%expr [ [%e? _] ] @ [%e? _]] -> Some (List_operation_on_litteral "::")
  | [%expr [%e? e1] @ [%e? e2]] when is_list_lit e1 && is_list_lit e2 ->
      Some (List_operation_on_litteral "@")
  | [%expr let [%p? p] = [%e? _] in [%e? e]] when pat_is_exp p e ->
      Some Identity_let
  | [%expr Printf.sprintf [%e? _]] ->
      Some Identity_sprintf_string
  | [%expr Printf.sprintf "%s" [%e? _]] ->
      Some Identity_sprintf_ps
  | { pexp_desc = Pexp_try (e, cases) }
    when sys_break_implicitly_caught cases ->
      Some Sys_break_implicitly_caught
  | { pexp_desc = Pexp_fun (_, _, px, { pexp_desc = Pexp_match(ex, _)}) }
    when pat_is_exp px ex ->
      Some Fun_match
  | _ -> None

let rate_module_type_name name =
  let open Ocamllint.Warning in
  if is_uppercase name then
    None
  else
    Some
      (Module_type_name_not_uppercase name)
