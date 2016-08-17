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

let warning_opt_to_string = function
  | None -> "(nothing)"
  | Some w -> Ocamllint.Warning.to_string w

(** Test rate_expression. Note that the recursion is implemented by the
    ast_mapper, so subexpressions are not checked in this test.
*)
let test_expression =
  let open Ocamllint.Warning in
  let t (expr, r) =
    let name =
      Printf.sprintf "Rate %s" (Pprintast.string_of_expression expr)
    in
    name >:: fun ctxt ->
      assert_equal
        ~ctxt
        ~printer:warning_opt_to_string
        r
        (Rules.rate_expression expr)
  in
  List.map t
    [ [%expr List.map f [2] ], Some (List_function_on_singleton "List.map")
    ; [%expr List.fold_left f z [2] ], Some (List_function_on_singleton "List.fold_left")
    ; [%expr List.fold_right f [2] z ], Some (List_function_on_singleton "List.fold_right")
    ; [%expr String.sub s 0 i ], Some (Inlined_function "Str.first_chars")
    ; [%expr String.sub s i (String.length s - i) ], Some (Inlined_function "Str.string_after")
    ; [%expr String.sub s (String.length s - i) i ], Some (Inlined_function "Str.last_chars")
    ; [%expr List.length l > 0 ], Some (Empty_list_test "<>")
    ; [%expr List.length l = 0 ], Some (Empty_list_test "=")
    ; [%expr result = true ], Some Comparison_to_boolean
    ; [%expr (fun x -> x + 1) 2], Some Abstract_apply
    ; [%expr x := !x + 1], Some (Inlined_function "incr")
    ; [%expr x := !x - 1], Some (Inlined_function "decr")
    ; [%expr let x = 3 in x ], Some Identity_let
    ; [%expr [3;14] @ [15;92] ], Some (List_operation_on_litteral "@")
    ; [%expr [1] @ [61;80] ], Some (List_operation_on_litteral "::")
    ; [%expr match None with Some _ -> 1 | None -> 2 ], Some Match_on_constructor
    ; [%expr match x with Some _ -> true | None -> true ], Some Constant_match
    ; [%expr let _ = List.map f xs in e ], Some (Discarded_result ("List.map", "List.iter"))
    ; [%expr Printf.sprintf "hello" ], Some Identity_sprintf_string
    ; [%expr Printf.sprintf "%s" "world" ], Some Identity_sprintf_ps
    ; [%expr if x then true else false], Some Useless_if
    ; [%expr if x then () else b], Some Backwards_if
    ; [%expr if x then a else ()], Some Useless_else
    ; [%expr if x then f x y else f x y], Some Constant_if
    ; [%expr List.hd], Some (Partial_function "List.hd")
    ; [%expr x == Some 1], Some Physical_comparison_on_allocated_litteral
    ; [%expr let module SomeThing = M in ()], Some (Module_name_not_snake_case "SomeThing")
    ; [%expr try f x with e -> None], Some Sys_break_implicitly_caught
    ; [%expr try f x with Sys.Break -> Some 1 | e -> None], None
    ; [%expr try f x with Sys.Break as e -> raise e | e -> None], None
    ; [%expr try f x with Sys.Break | Exit -> Some 1 | e -> None], None
    ; [%expr match () with _ when a -> ea | _ when b -> eb | _ -> something], None
    ; [%expr fun x -> match x with 1 -> true | _ -> false], Some Fun_match
    ]

let string_of_signature_item x =
  let open Format in
  ignore (flush_str_formatter ()) ;
  let f = str_formatter in
  Pprintast.signature f [x];
  flush_str_formatter ()

(**
   Test rate_signature_item.
   Same recursion caveat as in test_expression applies.
*)
let test_signature_item =
  let t (sigitem, r) =
    let name =
      Printf.sprintf "Rate %s" (string_of_signature_item sigitem)
    in
    name >:: fun ctxt ->
      assert_equal
        ~ctxt
        ~printer:warning_opt_to_string
        r
        (Rules.rate_signature_item sigitem)
  in
  let open Ast_helper in
  let open Location in
  let open Parsetree in
  List.map t
    [ Sig.value @@ Val.mk
        ~attrs:
            [ mknoloc "ocaml.doc"
            , PStr [%str "a string to descibe the function"]
            ]
        (mknoloc "f")
        [%type: int]
      , Some (Ocamllint.Warning.Typo_in_doc "descibe")
    ]


let suite =
  "Tests" >:::
  [ "Snake case" >::: test_snake_case
  ; "Expression" >::: test_expression
  ; "Signature item" >::: test_signature_item
  ]

let _ =
  run_test_tt_main suite
