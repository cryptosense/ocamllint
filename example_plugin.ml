let handle_warning ctxt = function
  | Ocamllint.Warning.Identity_let -> false
  | w -> Ocamllint.Config.(warning_active default ctxt) w

let config =
  let open Ocamllint.Config in
  default
  |> on_warning handle_warning

let _ = Ocamllint.Plugin.set_config config
