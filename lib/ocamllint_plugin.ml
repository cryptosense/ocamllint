let config = ref None

let get_config () = match !config with
    | None -> raise (Location.Error (Location.error ("No plugin was registered")))
    | Some c -> c

let set_config c =
  config := Some c
