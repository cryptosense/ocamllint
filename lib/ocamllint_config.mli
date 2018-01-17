type t

val default : t

val on_warning : (Ocamllint_context.t -> Ocamllint_warning.t -> bool) -> t -> t

val warning_active : t -> Ocamllint_context.t -> Ocamllint_warning.t -> bool
