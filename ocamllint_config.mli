type t

val default : t

val on_warning : (Warning.t -> bool) -> t -> t

val warning_active : t -> Warning.t -> bool
