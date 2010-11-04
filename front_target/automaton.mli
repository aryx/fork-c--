(*s: automaton.mli *)
(*s: abstract types *)
type t
(*x: abstract types *)
type stage
(*x: abstract types *)
type counter = string
type counterenv
(*e: abstract types *)
(*s: exposed types *)
type result =
  { overflow    : Block.t
  ; regs_used   : Register.Set.t
  ; mems_used   : Rtl.loc list
  ; align_state : int   (* final alignment state of overflow block *)
  }
(*x: exposed types *)
type width = int
type kind  = string
type loc = Rtlutil.aloc =
    { fetch  : width -> Rtl.exp
    ; store  : Rtl.exp -> width -> Rtl.rtl
    }
(*x: exposed types *)
type methods =
    { allocate : width: int -> alignment: int -> kind: string -> loc
    ; freeze   : Register.Set.t -> Rtl.loc list -> result
    } 
(*x: exposed types *)
type choice_predicate = int -> string -> counterenv -> bool
(*e: exposed types *)
(*s: registration types *)
type cc_spec  = { call : stage; results : stage; cutto : stage }
type cc_specs = (string * cc_spec) list
(*e: registration types *)
(*s: exported values *)
val allocate : t -> width:int -> kind:string -> align:int -> loc
val freeze   : t -> result
(*x: exported values *)
val fetch  : loc ->            width -> Rtl.exp
val store  : loc -> Rtl.exp -> width -> Rtl.rtl
(*x: exported values *)
val of_loc : Rtl.loc -> loc
(*x: exported values *)
val at         : Rtl.space -> start:Rtl.exp -> stage -> t
val of_methods : methods -> t
(*x: exported values *)
val ( *> ) : stage -> stage -> stage
(*x: exported values *)
val wrap   : (methods -> methods) -> stage
(*x: exported values *)
val overflow : growth:Memalloc.growth -> max_alignment:int -> stage
(*x: exported values *)
val widths : int list -> stage
(*x: exported values *)
val widen  : (int -> int) -> stage
(*x: exported values *)
val align_to : (int -> int) -> stage
(*x: exported values *)
val useregs : Register.t list -> bool -> stage
(*x: exported values *)
val bitcounter  : counter -> stage
val regs_by_bits: counter -> Register.t list -> bool -> stage

val argcounter  : counter -> stage
val regs_by_args: counter -> Register.t list -> bool -> stage

val pad         : counter -> stage
(*x: exported values *)
val postprocess : stage -> (result -> result) -> stage
(*x: exported values *)
val choice : (choice_predicate * stage) list -> stage
(*x: exported values *)
val counter_is : counter -> (int -> bool) -> choice_predicate
val is_kind    : kind                     -> choice_predicate
val is_width   : int                      -> choice_predicate
val is_any     :                             choice_predicate
(*x: exported values *)
val as_stage : t -> stage
(*x: exported values *)
val first_choice : (choice_predicate * stage) list -> stage
(*x: exported values *)
val unit : stage
(*x: exported values *)
val debug : counter -> (int -> string -> int -> int -> unit) -> stage
(*x: exported values *)
val init_cc : cc_specs
(*e: exported values *)
(*e: automaton.mli *)
