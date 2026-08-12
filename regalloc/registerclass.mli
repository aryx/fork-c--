(*s: registerclass.mli *)
  (*s: register class interface *)
  module type REGISTERCLASS = sig
    type t = Register.Set.t
    val is_empty: t -> bool
    val aliases: t -> t
    val alias_eq: t -> t -> bool
    val alias_contained:  t -> t -> bool
    val may_alias: t -> t -> bool
    val inter: t -> t -> t
    val eq: t -> t -> bool
    val to_string: t -> string
    val cardinal: t -> int
    val space_to_class: Rtl.space -> t
    val map_class: Rtl.space -> t -> unit
    val mkClass: Rtl.space -> ('a, 'b, 'c) Target.t -> t
    val init: Register.t list -> unit
  end

  (*e: register class interface *)
  module RegisterClass : REGISTERCLASS 
  (*s: class tree interface *)
    module type CLASSTREE = sig
      type t = Vertex of RegisterClass.t list ref * t list ref
      val classes: t -> RegisterClass.t list
      val down: t -> RegisterClass.t list
      val children: t -> t list
      val mkTree: RegisterClass.t list -> t
      val mkTreeList:  Rtl.space list -> Register.t list ->('a, 'b, 'c) Target.t -> t list
      val space_to_tree: Rtl.space -> t
    end
        
  (*e: class tree interface *)
  module ClassTree:CLASSTREE
(*e: registerclass.mli *)
