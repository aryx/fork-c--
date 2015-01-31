(*s: front_target/target.mli *)
(*s: target.mli *)
(*s: map and machine types *)
type ('em, 'pr) map' = ('em, 'pr) Ep.pre_map =
    { embed   : 'em
    ; project : 'pr
    }
type brtl = Rtl.exp -> Rtl.rtl
type ('a,'b) map  = ('a -> 'b -> brtl Dag.branch, Rtl.rtl -> 'b) map'
type ('a,'b) mapc = ('a -> 'b -> brtl Dag.cbranch, Rtl.rtl -> 'b) map'

type 'a machine = 'a Mflow.machine =
  { bnegate:   Rtl.rtl -> Rtl.rtl
  ; goto:      ('a, Rtl.exp) map
  ; jump:      ('a, Rtl.exp) map
  ; call:      ('a, Rtl.exp) map
  ; branch:    ('a, Rtl.exp) mapc (* condition *)
  ; retgt_br:  Rtl.rtl -> brtl Dag.cbranch
  ; move:      'a -> src:Register.t -> dst:Register.t -> brtl Dag.block
  ; spill:     'a -> Register.t -> Rtlutil.aloc -> brtl Dag.block
  ; reload:    'a -> Rtlutil.aloc -> Register.t -> brtl Dag.block
  ; cutto:     ('a, Mflow.cut_args) map    (* newpc * newsp map*)
  ; return:    Rtl.rtl
  ; forbidden: Rtl.rtl   (* causes a run-time error *)
  }
(*e: map and machine types *)
val boxmach : unit machine
(*s: type cc *)
type 'automaton cc' =      
    { sp:           Rtl.loc         (* stack pointer                      *)
    ; return:       Rtl.rtl         (* machine instr passed to Cfg.return *)
       (* NEEDS TO TAKE ALTS AND INDEX AS PARAMETER *)
    ; proc:         'automaton  (* pass parameter to a procedure      *)
    ; cont:         'automaton  (* pass parameter to a continuation   *)
    ; ret:          'automaton  (* return values                      *)
    ; mutable allocatable:  Register.t list (* regs for reg-allocation            *)
    (* THIS TYPE SHOULD INCLUDE INFORMATION ABOUT ALTERNATE RETURN CONTINUATIONS,
       IN PARTICULAR, HOW BIG IS EACH SLOT, AND DOES IT HOLD AN INSTRUCTION OR
       AN ADDRESS? *)
    ; stack_slots:  'automaton (* where private data go *)
    }
(*e: type cc *)
(*s: auxiliary types used to define type [[t]] *)
type capabilities = {
    operators   : Rtl.opr   list;  (* operators that can be used *)
    literals    : Rtl.width list;  (* literals that can be used *)
    litops      : Rtl.opr   list;  (* operators usable on literals only *)
    memory      : Rtl.width list;  (* memory refs that can be used *)
    block_copy  : bool;            (* OK to copy large variables, large refs? *)
    itemps      : Rtl.width list;  (* what integer temporaries can be computed with *)
    ftemps      : Rtl.width list;  (* what floating temporaries can be computed with *)
    iwiden      : bool;            (* use int ops and literals at narrow widths? *)
    fwiden      : bool;            (* use float ops literals at narrow widths? *)
  }
(*e: auxiliary types used to define type [[t]] *)
(*s: type t(target.nw) *)
type ('proc, 'automaton, 'cc) t = { 
  (*s: components of [[t]] *)
  name: string;
  (*x: components of [[t]] *)
  mutable cc_specs: Automaton.cc_specs;
  cc_spec_to_auto: string -> Automaton.cc_spec -> 'cc;
  (*x: components of [[t]] *)
  vfp : Rtl.exp;   (* the (immutable) virtual frame pointer *)
     (* always equal to Vfp.mk T.pointersize *)
  (*x: components of [[t]] *)
  byteorder:      Rtl.aggregation   ; (* big/little endian, id *)
  wordsize:       int               ; (* bits *)
  pointersize:    int               ; (* bits *)
  alignment:      int               ; (* alignment of word access *)
  memsize:        int               ; (* smallest addressable unit, typically 8 *)
  memspace:       Rtl.space         ; (* redundant with byte order, word size *)
  float:          Float.t           ; (* floating pt name and semantics    *)
  charset:        string            ; (* "latin1"  character encoding      *)
  globals:        'automaton   ; (* Automaton to allocate global vars *)
  (*x: components of [[t]] *)
  max_unaligned_load : Rtl.count;  (* how many cells to load unaligned *)
  (*x: components of [[t]] *)
  spaces:          Space.t list;
  (*x: components of [[t]] *)
  reg_ix_map :     int * int Register.Map.t;
  (*x: components of [[t]] *)
  distinct_addr_sp:  bool;
  (*x: components of [[t]] *)
  data_section:   string;          (* ASM section for global regs *)
  (*x: components of [[t]] *)
  rounding_mode : Rtl.loc;
  (*x: components of [[t]] *)
  named_locs:    Rtl.loc Strutil.Map.t;
  (*x: components of [[t]] *)
  is_instruction: Rtl.rtl -> bool;
  (*x: components of [[t]] *)
  machine : 'proc machine;
  (*x: components of [[t]] *)
  tx_ast : Auxfuns.void Nelab.compunit -> Auxfuns.void Nelab.compunit;
  (*x: components of [[t]] *)
  capabilities: capabilities;
  (*e: components of [[t]] *)
 }
(*e: type t(target.nw) *)

(*s: exported functions *)
val is_tmp : ('pr, 'au, 'cc) t -> Rtl.space -> bool  (* partially apply me *)
val fits:    ('pr, 'au, 'cc) t -> Rtl.space -> Register.t -> bool
(*x: exported functions *)
val mk_reg_ix_map : Space.t list -> (int * int Register.Map.t)
(*e: exported functions *)
val space : ('pr, 'au, 'cc) t -> Rtl.space -> Space.t
(*x: target.mli *)
val incapable : capabilities  (* the completely useless back end *)
(*x: target.mli *)
val minimal_capabilities: int -> capabilities
(*e: target.mli *)
(*e: front_target/target.mli *)
