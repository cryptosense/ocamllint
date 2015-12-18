open OUnit2

let test_snake_case =
  let t (s, r) =
    let name =
      Printf.sprintf "is_snake_case %S?" s
    in
    name >:: fun ctxt ->
      assert_equal r (Rules.is_snake_case s)
  in
  List.map t
    [ "ident", true
    ; "some_ident", true
    ; "someIdent", false
    ]

let expr_to_string expr =
  let b = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer b in
  Printast.expression 0 fmt expr;
  Format.pp_print_flush fmt ();
  Buffer.contents b

let test_style =
  let t (expr, r) =
    let name =
      Printf.sprintf "Rate %s" (expr_to_string expr)
    in
    name >:: fun ctxt ->
      assert_equal
        ~ctxt
        ~cmp:[%eq:string option]
        ~printer:[%show:string option]
        r
        (Rules.rate_expression expr)
  in
  List.map t
    [ [%expr List.map f [2] ], Some "List.map on singleton"
    ]

let suite =
  "Tests" >:::
  [ "Snake case" >::: test_snake_case
  ; "Style" >::: test_style
  ]

let _ =
  run_test_tt_main suite
