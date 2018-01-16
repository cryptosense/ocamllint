#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ocamllint" @@ fun c ->
  Ok
    [ Pkg.mllib "lib/ocamllint.mllib"
    ; Pkg.libexec "ppx/ppx_lint"
    ; Pkg.test "test/tests"
    ]
