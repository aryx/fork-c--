(*s: spans.mli *)
type t
type label   = string
type link    = Reloc.t
type aligned = int
val to_spans : inalloc:Rtl.loc -> outalloc:Rtl.loc -> ra:Rtl.loc ->
               users:((Bits.bits * link) list) ->
               csregs:((Register.t * Rtl.loc option) list) ->
               conts:((label * Rtl.loc * (string * int * aligned) list) list) ->
                          (* (ra, sp, parms : (hint, var number, alignment) list) *)
               sds:(Rtl.loc list)                          ->
               vars:(Rtl.loc option array)                 ->
               t
(*s: regrettably exposed [[rep]] type *)
type rep =
  { mutable inalloc  : Rtl.loc
  ; mutable outalloc : Rtl.loc
  ; mutable ra       : Rtl.loc
  ; mutable users    : (Bits.bits * link) list
  ; mutable csregs   : (Register.t * Rtl.loc option) list
  ; mutable conts    : (label * Rtl.loc * (string * int * aligned) list) list
  ; mutable sds      : Rtl.loc list
  ; mutable vars     : Rtl.loc option array
  }
(*e: regrettably exposed [[rep]] type *)
val expose : t -> rep
(*x: spans.mli *)
val fold_live_locs : (Register.x -> 'a -> 'a) -> t -> 'a -> 'a
(*e: spans.mli *)
