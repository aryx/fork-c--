(*s: front_fenv/fenv.mli *)
(*s: fenv.mli *)
(*s: exposed types shared by clean and dirty environments *)
type regkind       = RReg  of string   (* hardware reg *)
                   | RKind of string   (* calling convention kind *)
                   | RNone             (* none of above *)
(*x: exposed types shared by clean and dirty environments *)
type variable =    { index:        int
                   ; rkind:        regkind
                   ; loc:          Rtl.loc
                   ; variance:     Ast.variance
                   }
(*x: exposed types shared by clean and dirty environments *)
type symclass      = Proc          of Symbol.t
                   | Code          of Symbol.t    
                   | Data          of Symbol.t
                   | Stack         of Rtl.exp  (* address of slot *)

type denotation    = Constant      of Bits.bits
                   | Label         of symclass
                   | Import        of string * Symbol.t
                                             (* external name, assembly symbol *)
                   | Variable      of variable
                   | Continuation  of continuation
and continuation = { base       : Block.t;   (* always empty; used only for address *)
                     convention : string;
                     formals    : (string * variable * aligned) list;
                                                  (* kinded, aligned formals *)
                     mutable escapes     : bool;  (* used as rvalue *)
                     mutable cut_to      : bool;  (* mentioned in annotation *)
                     mutable unwound_to  : bool;  (* mentioned in annotation *)
                     mutable returned_to : convention list;
                        (* list every convention cc such that there exists
                           a call site with convention cc and that call site
                           `also returns to' the continuation *)
                   } (* might need a convention here *)
and convention   = string
and aligned      = int
(*e: exposed types shared by clean and dirty environments *)
(* pad: now put also those private types in mli *)
(*s: private types shared by clean and dirty environments *)
type stackdata =      { soffset    :    int    (* current offset *)
                      ; smaxalign  :    int    (* max stackdata align constr*)
                      ; sname      :    string (* label for offset *)
                      }
(*x: private types shared by clean and dirty environments *)
type extern         = { imported:     Strutil.Set.t
                      ; exported:     Strutil.Set.t
                      ; nam2sym:      Symbol.t Strutil.Map.t
                      }
(*e: private types shared by clean and dirty environments *)
(*s: exported signature [[Env]] *)
module type Env = sig
    type 'a info
    type 'a partial
    val  bad: unit -> 'a info

    (*s: bindings appearing in signature [[Env]] *)
    (* pad: was previously an abstract type *)
    type 'proc env' = { scopes          :    scope list (* top = hd scopes *)
                       ; srcmap          :    Srcmap.map
                       (* pad: the assembler is embeded in the fat env !! *)
                       ; asm             :    'proc Asm.assembler

                       ; error           :    bool
                       ; metrics         :    Metrics.t
                       ; extern          :    extern
                       ; globals         :    string  list(* global registers *)
                       ; stackdata       :    stackdata
                       }
    (* type env = Proc.t env' *)
    and scope           = { mutable venv:   ventry Strutil.Map.t
                       ; tenv:   tentry Strutil.Map.t
                       ; rindex: int   (* getIndex, nextIndex *)
                       }

    (*s: types exposed in signature [[Env]] *)
    and  ventry        = Srcmap.rgn * (denotation * Types.ty) info
    (*x: types exposed in signature [[Env]] *)
    and  tentry         = Srcmap.rgn * Types.ty info
    (*e: types exposed in signature [[Env]] *)
    val map : ('b -> 'a) -> 'a env' -> 'b env'

    val empty   : Srcmap.map -> Metrics.t -> 'proc Asm.assembler -> 'proc env'
                           (* empty scope stack *)
    val srcmap  : 'proc env' -> Srcmap.map
    (* pad: allow to access the assembler embeded in the fat env *)
    val asm     : 'proc env' -> 'proc Asm.assembler
    val metrics : 'proc env' -> Metrics.t
    (*x: bindings appearing in signature [[Env]] *)
    val emptyscope: scope   
    val top:        'p env' -> scope            (* top empty = assert false *)
    val pop:        'p env' -> 'p env'              (* pop empty = assert false *)
    val push:       'p env' -> scope -> 'p env'
    (*x: bindings appearing in signature [[Env]] *)
    val foldv:      (string -> ventry -> 'a -> 'a) -> scope -> 'a -> 'a
    (*x: bindings appearing in signature [[Env]] *)
    val bindv           : string -> ventry  -> 'p env' -> 'p env'
    val rebindv         : string -> ventry  -> 'p env' -> 'p env'
    val rebindv'        : string -> ventry  -> 'p env' -> unit  (* mutates *)
    val bindt           : string -> tentry  -> 'p env' -> 'p env'
    val findv           : string -> 'p env' -> ventry   (* Error.ErrorExn *)
    val findt           : string -> 'p env' -> tentry   (* Error.ErrorExn *)
    (*x: bindings appearing in signature [[Env]] *)
    val is_localv       : string -> 'p env' -> bool     (* *)
    (*x: bindings appearing in signature [[Env]] *)
    val flagError       : 'p env' -> 'p env'
    val errorFlag       : 'p env' -> bool
    (*x: bindings appearing in signature [[Env]] *)
    val import: Srcmap.rgn -> string -> string -> 'p env' -> 'p env' (* import g as f *)
    val export: Srcmap.rgn -> string -> string -> 'p env' -> 'p env' (* export f as g *)
    (*x: bindings appearing in signature [[Env]] *)
    val symbol:   'p env' -> string -> Symbol.t
    (*e: bindings appearing in signature [[Env]] *)
end
(*e: exported signature [[Env]] *)

module Dirty : Env
    with type 'a info    = 'a Error.error
    with type 'a partial = 'a option

module Clean : Env
    with type 'a info    = 'a 
    with type 'a partial = 'a 

val clean : 'proc Dirty.env' -> 'proc Clean.env'
(*x: fenv.mli *)
val denotation's_category : denotation -> string
(*e: fenv.mli *)
(*e: front_fenv/fenv.mli *)
