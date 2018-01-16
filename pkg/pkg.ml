#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ocamllint" @@ fun c ->
  Ok
    [ Pkg.mllib "ocamllint.mllib"
    ; Pkg.libexec "ppx_lint"
    ; Pkg.test "tests"
    ]
