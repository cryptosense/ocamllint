type t

val default : t

val on_warning : (Context.t -> Warning.t -> bool) -> t -> t

val warning_active : t -> Context.t -> Warning.t -> bool
