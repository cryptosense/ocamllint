open Ocamlbuild_plugin

let (major, minor) =
  Scanf.sscanf
    Sys.ocaml_version
    "%d.%d.%s"
    (fun major minor other -> major,minor)

let compat_include_dir =
  Printf.sprintf "%d.%02d" major minor

let after_rules () =
  Options.include_dirs := compat_include_dir::!Options.include_dirs

let () = dispatch (function
    | After_rules -> after_rules ()
    | _ -> ())
