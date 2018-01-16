val rate_expression : Parsetree.expression -> Ocamllint.Warning.t option

val rate_module_name : string -> Ocamllint.Warning.t option

val rate_module_type_name : string -> Ocamllint.Warning.t option

val rate_signature_item : Parsetree.signature_item -> Ocamllint.Warning.t option

val is_snake_case : string -> bool
