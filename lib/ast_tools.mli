val expr_eq : Migrate_parsetree.Ast_404.Parsetree.expression -> Migrate_parsetree.Ast_404.Parsetree.expression -> bool

(**
   Return all attributes for a signature item.
   In some cases (e.g. recursive type declarations) there can be several ones.
 *)
val sigitem_attributes : Migrate_parsetree.Ast_404.Parsetree.signature_item -> Migrate_parsetree.Ast_404.Parsetree.attributes list
