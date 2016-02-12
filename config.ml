type t =
  { warning_active : Warning.t -> bool
  }

let on_warning warning_active c = { c with warning_active }

let warning_active c = c.warning_active

let default =
  { warning_active = fun _ -> true
  }
