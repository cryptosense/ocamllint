type t

val make : ?loc:Location.t -> unit -> t

val loc : t -> Location.t option
