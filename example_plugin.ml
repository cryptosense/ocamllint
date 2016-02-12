let handle_warning = function
  | Warning.Identity_let -> false
  | w -> Ocamllint_config.(warning_active default) w

let config =
  let open Ocamllint_config in
  default
  |> on_warning handle_warning

let _ = Ocamllint_plugin.set_config config
