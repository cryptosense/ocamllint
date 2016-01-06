open Ast_tools
open Asttypes
open Parsetree

(** Allocated litterals *)
let is_allocated_lit exp =
  match exp.pexp_desc with
  | Pexp_constant (Const_string _)
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

let rate_module_name name =
  if is_snake_case name then
    None
  else
    Some ("Module name not in snake case: " ^ name)

let rec rate_expression = function
  | [%expr [%e? e1] @@ [%e? e2]]
  | [%expr [%e? e2] |> [%e? e1]] -> rate_expression [%expr [%e e1] [%e e2]]
  | [%expr String.concat [%e? s] [ [%e? l] ] ] -> Some "String.concat on singleton"
  | [%expr List.map [%e? _] [ [%e? _] ] ] -> Some "List.map on singleton"
  | [%expr List.fold_left [%e? _] [%e? _] [ [%e? _] ] ] -> Some "List.fold_left on singleton"
  | [%expr List.fold_right [%e? _] [ [%e? _] ] [%e? _] ] -> Some "List.fold_right on singleton"
  | [%expr if [%e? _] then true else false ] -> Some "Useless if"
  | [%expr if [%e? _] then () else [%e? _]] -> Some "Backwards if"
  | [%expr if [%e? _] then [%e? _] else ()] -> Some "Useless else"
  | [%expr List.hd ] -> Some "Use of partial function List.hd"
  | [%expr List.tl ] -> Some "Use of partial function List.tl"
  | [%expr String.sub [%e? s] 0 [%e? n] ] -> Some "Use Str.first_chars"
  | [%expr String.sub [%e? s] [%e? n] (String.length [%e? s'] - [%e? n']) ]
    when expr_eq s s' && expr_eq n n'
    -> Some "Use Str.string_after"
  | [%expr String.sub [%e? s] (String.length [%e? s'] - [%e? n]) [%e? n'] ]
    when expr_eq s s' && expr_eq n n'
    -> Some "Use Str.last_chars"
  | [%expr List.length [%e? _] > 0 ] -> Some "Use <> []"
  | [%expr List.length [%e? _] = 0 ] -> Some "Use = []"
  | [%expr [%e? _] = true]
  | [%expr [%e? _] = false]
  | [%expr [%e? _] == true]
  | [%expr [%e? _] == false]
  | [%expr [%e? _] <> true]
  | [%expr [%e? _] != false] -> Some "Useless comparison to boolean"
  | [%expr [%e? { pexp_desc = Pexp_fun _} ] [%e? _]] -> Some "Use let/in"
  | { pexp_desc = Pexp_letmodule ({ txt }, _, _) } -> rate_module_name txt
  | [%expr [%e? e1] := ![%e? e2] + 1] when expr_eq e1 e2 -> Some "Use incr"
  | [%expr [%e? e1] := ![%e? e2] - 1] when expr_eq e1 e2 -> Some "Use decr"
  | [%expr if [%e? _] then [%e? e1] else [%e? e2]] when expr_eq e1 e2 ->
      Some "Useless if"
  | expr when all_branches_same expr ->
      Some "Useless match"
  | expr when match_on_const expr ->
      Some "Match on constant or constructor"
  | [%expr [%e? f] [%e? e1] [%e? e2]]
      when is_phys_eq f && (is_allocated_lit e1 || is_allocated_lit e2) ->
      Some "Use structural comparison"
  | [%expr let _ = List.map [%e? _] [%e? _] in [%e? _]] ->
      Some "Use List.iter"
  | [%expr [ [%e? _] ] @ [%e? _]] -> Some "Use ::"
  | [%expr [%e? e1] @ [%e? e2]] when is_list_lit e1 && is_list_lit e2 ->
      Some "Merge list litterals"
  | [%expr let [%p? p] = [%e? _] in [%e? e]] when pat_is_exp p e ->
      Some "Useless let"
  | [%expr Printf.sprintf [%e? _]] ->
      Some "Useless sprintf"
  | [%expr Printf.sprintf "%s" [%e? _]] ->
      Some "Useless sprintf %s"
  | _ -> None

let rate_module_type_name name =
  if is_uppercase name then
    None
  else
    Some
    ("Module type name not uppercase : " ^ name)
