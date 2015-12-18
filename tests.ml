open OUnit

let test_snake_case =
  let open Rules in
  let t (s, r) =
    let name =
      Printf.sprintf "is_snake_case %S?" s
    in
    name >:: fun ctxt ->
      assert_equal r (is_snake_case s)
  in
  List.map t
    [ "ident", true
    ; "some_ident", true
    ; "someIdent", false
    ]

let suite =
  "Tests" >:::
  [ "Snake case" >::: test_snake_case
  ]

let _ =
  run_test_tt_main suite
