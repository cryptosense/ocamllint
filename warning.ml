type t =
  | List_function_on_singleton of string
  | Useless_if
  | Backwards_if
  | Useless_else
  | Partial_function of string
  | Inlined_function of string
  | Empty_list_test of string
  | Comparison_to_boolean
  | Abstract_apply
  | Module_name_not_snake_case of string
  | Constant_if
  | Constant_match
  | Match_on_constructor
  | Physical_comparison_on_allocated_litteral
  | Discarded_result of string * string
  | List_operation_on_litteral of string
  | Identity_let
  | Identity_sprintf_string
  | Identity_sprintf_ps
  | Module_type_name_not_uppercase of string
  | Sys_break_implicitly_caught
  | Fun_match

let to_string = function
  | List_function_on_singleton f -> Printf.sprintf "%s on singleton" f
  | Useless_if -> "Useless if"
  | Backwards_if -> "Backwards if"
  | Useless_else -> "Useless else"
  | Partial_function f -> Printf.sprintf "Use of partial function %s" f
  | Inlined_function f -> Printf.sprintf "Use %s" f
  | Empty_list_test t -> Printf.sprintf "Use %s []" t
  | Comparison_to_boolean -> "Comparison to boolean"
  | Abstract_apply -> "Application of an anonymous function"
  | Module_name_not_snake_case m -> Printf.sprintf "Module name not in snake case: %s" m
  | Constant_if -> "Both branches of this if are identical"
  | Constant_match -> "All branches of this match are identical"
  | Match_on_constructor -> "Match on constant or constructor"
  | Physical_comparison_on_allocated_litteral -> "Use structural comparison"
  | Discarded_result (used, recommended) -> Printf.sprintf "Result of %s discarded, use %s instead" used recommended
  | List_operation_on_litteral f -> Printf.sprintf "List operation on litteral: %s" f
  | Identity_let -> "Useless let binding"
  | Identity_sprintf_string -> "Useless sprintf"
  | Identity_sprintf_ps -> "Useless sprintf"
  | Module_type_name_not_uppercase m -> Printf.sprintf "Module type name not in uppercase: %s" m
  | Sys_break_implicitly_caught -> "Sys.Break is implicitly caught"
  | Fun_match -> "Use function instead of let f x = match x with"
