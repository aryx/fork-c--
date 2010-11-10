(*s: callspec.mli *)
val overflow      : dealloc:Call.party -> alloc:Call.party -> Call.overflow
val c_overflow    : Call.overflow
val tail_overflow : Call.overflow
(*x: callspec.mli *)
type nvr_saver = Talloc.Multiple.t -> Register.t -> Rtl.loc
(* suggested by NR, but not used 
 * val save_nvrs: Space.t list -> nvr_saver
 *)
(*x: callspec.mli *)
module ReturnAddress: sig
    type style  =
        | KeepInPlace           (* leave RA where it is     *)
        | SaveToTemp  of char   (* save RA in a temporary of this space  *)
    (* <<suggested functions>> *)
end     
(*x: callspec.mli *)
(*s: type t *)
type t =
    { name              : string            (* name this CC *)
    ; stack_growth      : Memalloc.growth   (* up or down *)
    ; overflow          : Call.overflow     (* overflow handling *)
    ; memspace          : Register.space
    ; sp                : Register.t        (* stack pointer register *)
    ; sp_align          : int               (* alignment of sp *)
    ; all_regs          : Register.Set.t    (* regs visible to allocator *)
    ; nv_regs           : Register.Set.t    (* preserved registers *)
    ; save_nvr          : nvr_saver         (* how to save registers *)
    ; ra                : Rtl.loc           (* where is RA, how to treat it *)
                          * ReturnAddress.style
    }
(*e: type t *)

(*x: callspec.mli *)
val to_call: cutto:(unit, Mflow.cut_args) Target.map -> 
             return:(int -> int -> ra:Rtl.exp -> Rtl.rtl) -> 
             Automaton.cc_spec ->
             t -> Call.t   
(*e: callspec.mli *)
