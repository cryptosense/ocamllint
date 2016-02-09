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

(** Test rate_expression. Note that the recursion is implemented by the
    ast_mapper, so subexpressions are not checked in this test.
*)
let test_style =
  let warning_opt_to_string = function
    | None -> None
    | Some w -> Some (Warning.to_string w)
  in
  let t (expr, r) =
    let name =
      Printf.sprintf "Rate %s" (Pprintast.string_of_expression expr)
    in
    name >:: fun ctxt ->
      assert_equal
        ~ctxt
        ~cmp:[%eq:string option]
        ~printer:[%show:string option]
        r
        (warning_opt_to_string (Rules.rate_expression expr))
  in
  List.map t
    [ [%expr List.map f [2] ], Some "List.map on singleton"
    ; [%expr List.fold_left f z [2] ], Some "List.fold_left on singleton"
    ; [%expr List.fold_right f [2] z ], Some "List.fold_right on singleton"
    ; [%expr String.sub s 0 i ], Some "Use Str.first_chars"
    ; [%expr String.sub s i (String.length s - i) ], Some "Use Str.string_after"
    ; [%expr String.sub s (String.length s - i) i ], Some "Use Str.last_chars"
    ; [%expr List.length l > 0 ], Some "Use <> []"
    ; [%expr List.length l = 0 ], Some "Use = []"
    ; [%expr result = true ], Some "Comparison to boolean"
    ; [%expr (fun x -> x + 1) 2], Some "Application of an anonymous function"
    ; [%expr x := !x + 1], Some "Use incr"
    ; [%expr x := !x - 1], Some "Use decr"
    ; [%expr let x = 3 in x ], Some "Useless let binding"
    ; [%expr [3;14] @ [15;92] ], Some "List operation on litteral: @"
    ; [%expr [1] @ [61;80] ], Some "List operation on litteral: ::"
    ; [%expr match None with Some _ -> 1 | None -> 2 ], Some "Match on constant or constructor"
    ; [%expr match x with Some _ -> true | None -> true ], Some "All branches of this match are identical"
    ; [%expr let _ = List.map f xs in e ], Some "Result of List.map discarded, use List.iter instead"
    ; [%expr Printf.sprintf "hello" ], Some "Useless sprintf"
    ; [%expr Printf.sprintf "%s" "world" ], Some "Useless sprintf"
    ; [%expr if x then true else false], Some "Useless if"
    ; [%expr if x then () else b], Some "Backwards if"
    ; [%expr if x then a else ()], Some "Useless else"
    ; [%expr if x then f x y else f x y], Some "Both branches of this if are identical"
    ; [%expr List.hd], Some "Use of partial function List.hd"
    ; [%expr x == Some 1], Some "Use structural comparison"
    ; [%expr let module SomeThing = M in ()], Some "Module name not in snake case: SomeThing"
    ; [%expr try f x with e -> None], Some "Sys.Break is implicitly caught"
    ; [%expr try f x with Sys.Break -> Some 1 | e -> None], None
    ; [%expr try f x with Sys.Break as e -> raise e | e -> None], None
    ]

let suite =
  "Tests" >:::
  [ "Snake case" >::: test_snake_case
  ; "Style" >::: test_style
  ]

let _ =
  run_test_tt_main suite
