open Parsetree

let sigitem_attributes { psig_desc } =
  match psig_desc with
  | Psig_value vd -> [vd.pval_attributes]
  | Psig_type tdecls -> List.map (fun tdecl -> tdecl.ptype_attributes) tdecls
  | Psig_typext text -> [text.ptyext_attributes]
  | Psig_exception exc -> [exc.pext_attributes]
  | Psig_module md -> [md.pmd_attributes]
  | Psig_recmodule mds -> List.map (fun md -> md.pmd_attributes) mds
  | Psig_modtype pmtd -> [pmtd.pmtd_attributes]
  | Psig_open popen -> [popen.popen_attributes]
  | Psig_include pincl -> [pincl.pincl_attributes]
  | Psig_class pcis
  | Psig_class_type pcis -> List.map (fun pct -> pct.pci_attributes) pcis
  | Psig_attribute attribute -> [[attribute]]
  | Psig_extension (_, attributes) -> [attributes]
