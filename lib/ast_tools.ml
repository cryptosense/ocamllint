open Asttypes
open Parsetree

let rec li_eq li1 li2 =
  let open Longident in
  match (li1, li2) with
  | Lident s1, Lident s2 -> s1 = s2
  | Ldot (i1, s1), Ldot (i2, s2) -> li_eq i1 i2 && s1 = s2
  | Lapply (lia1, lib1), Lapply (lia2, lib2) ->
      li_eq lia1 lia2 && li_eq lib1 lib2
  | _ -> false

(** Syntactic equality *)
let rec expr_eq e1 e2 =
  match e1.pexp_desc, e2.pexp_desc with
  | Pexp_constant c1, Pexp_constant c2 -> c1 = c2
  | Pexp_ident i1, Pexp_ident i2 -> i1.txt = i2.txt
  | Pexp_construct ({ txt = li1 }, eo1), Pexp_construct ({ txt = li2 }, eo2) ->
      li_eq li1 li2 && expr_option_eq eo1 eo2
  | Pexp_variant (l1, eo1), Pexp_variant (l2, eo2) ->
      l1 = l2 && expr_option_eq eo1 eo2
  | Pexp_field (e1, { txt = li1 }), Pexp_field (e2, { txt = li2 }) ->
      expr_eq e1 e2 && li_eq li1 li2
  | Pexp_array el1, Pexp_array el2
  | Pexp_tuple el1, Pexp_tuple el2
    -> expr_list_eq el1 el2
  | Pexp_apply (e1, el1), Pexp_apply (e2, el2) ->
      expr_eq e1 e2 && expr_label_list_eq el1 el2
  | _ -> false

and expr_option_eq eo1 eo2 = match (eo1, eo2) with
  | Some e1, Some e2 -> expr_eq e1 e2
  | None, None -> true
  | _ -> false

and expr_list_eq el1 el2 =
  try
    List.for_all2 expr_eq el1 el2
  with Invalid_argument _ -> false

and label_eq l1 l2 =
  let open Ast_convenience.Label in
  match explode l1, explode l2 with
  | Nolabel, Nolabel -> true
  | Labelled s1, Labelled s2
  | Optional s1, Optional s2 -> (s1 : string) = s2
  | _ -> false

and expr_label_list_eq el1 el2 =
  let expr_label_eq (l1, e1) (l2, e2) =
    label_eq l1 l2 && expr_eq e1 e2
  in
  try
    List.for_all2 expr_label_eq el1 el2
  with Invalid_argument _ -> false

let sigitem_attributes = Compat.sigitem_attributes
