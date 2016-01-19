val rate_expression : Parsetree.expression -> Warning.t option

val rate_module_name : string -> Warning.t option

val rate_module_type_name : string -> Warning.t option

val is_snake_case : string -> bool
