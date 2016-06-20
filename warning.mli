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

val to_string : t -> string
