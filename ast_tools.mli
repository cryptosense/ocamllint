val expr_eq : Parsetree.expression -> Parsetree.expression -> bool

(**
   Return all attributes for a signature item.
   In some cases (e.g. recursive type declarations) there can be several ones.
 *)
val sigitem_attributes : Parsetree.signature_item -> Parsetree.attributes list
