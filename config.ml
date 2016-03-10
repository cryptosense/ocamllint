type t =
  { warning_active : Context.t -> Warning.t -> bool
  }

let on_warning warning_active c = { c with warning_active }

let warning_active config = config.warning_active

let default =
  { warning_active = fun _ _ -> true
  }
