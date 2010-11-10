(*s: callspec.ml *)
module R  = Rtl
module RU = Rtlutil
module W  = Rtlutil.Width
module T  = Talloc.Multiple
module C  = Call
module A  = Automaton

let overflow ~dealloc ~alloc = 
    { C.parameter_deallocator = dealloc
    ; C.result_allocator      = alloc
    }


let tail_overflow = 
    { C.parameter_deallocator = C.Callee
    ; C.result_allocator      = C.Caller
    }

let c_overflow =
    { C.parameter_deallocator = C.Caller
    ; C.result_allocator      = C.Caller
    } 

type nvr_saver = Talloc.Multiple.t -> Register.t -> Rtl.loc


module ReturnAddress = struct
    type style  =
        | KeepInPlace                   (* leave RA where it is     *)
        | SaveToTemp of char            (* save RA in a temporary   *)
end    

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
(*x: callspec.ml *)
let old_end wordsize growth block = match growth with
| Memalloc.Down -> RU.addk wordsize (Block.base block) (Block.size block)
| Memalloc.Up   -> Block.base block

let young_end wordsize growth block = match growth with
| Memalloc.Down -> Block.base block
| Memalloc.Up   -> RU.addk wordsize (Block.base block) (Block.size block) 


let wordsize t = match t.sp with ((_,_,ms),_,c) -> Cell.to_width ms c
let memspace t = t.memspace
let byteorder t = let (_, b, _) = t.memspace in b
let vfp wordsize             = Vfp.mk wordsize
let std_sp_location wordsize = 
    RU.add wordsize (vfp wordsize) (R.late "minus frame size" wordsize)

let mk_automaton wordsize memspace block_name automaton = fun () ->
    Block.srelative (vfp wordsize) block_name (A.at memspace)
      automaton
(*x: callspec.ml *)
let ra t    = fst t.ra
let style t = snd t.ra
(*x: callspec.ml *)
let prolog t auto = 
  let autosp = (fun a -> young_end (wordsize t) t.stack_growth a.A.overflow) in
  C.incoming ~growth:t.stack_growth ~sp:(R.reg t.sp)
    ~mkauto:(mk_automaton (wordsize t) (memspace t) "in call parms" auto.A.call)
                        
    (* ~autosp:(fun _ -> vfp (wordsize t)) *)
    ~autosp:autosp
    ~postsp:(fun _ _ -> std_sp_location (wordsize t)) 
    ~insp:(fun a _ _ -> autosp a)
(*x: callspec.ml *)
let epilog t auto =
    C.outgoing ~growth:t.stack_growth ~sp:(R.reg t.sp)
        ~mkauto:(mk_automaton (wordsize t) (memspace t) 
                   "out ovfl results" auto.A.results)
        ~autosp:(fun r -> std_sp_location (wordsize t))
        ~postsp:(match t.overflow with
                | {C.parameter_deallocator=C.Caller; C.result_allocator=C.Caller} ->
                    (fun _ _ -> vfp (wordsize t)) 
                | {C.parameter_deallocator=C.Callee; C.result_allocator=C.Caller} 
                | {C.parameter_deallocator=C.Caller; C.result_allocator=C.Callee} 
                | {C.parameter_deallocator=C.Callee; C.result_allocator=C.Callee} -> 
                    (fun a _ -> 
                        young_end (wordsize t) t.stack_growth a.A.overflow)
                )
(*x: callspec.ml *)
let call_actuals t auto =
    C.outgoing ~growth:t.stack_growth ~sp:(R.reg t.sp)
        ~mkauto:(mk_automaton (wordsize t) (memspace t) "out call parms" auto.A.call)
        ~autosp:(fun r  -> std_sp_location (wordsize t))
        ~postsp:(fun a sp -> young_end (wordsize t) t.stack_growth a.A.overflow)
(*x: callspec.ml *)
let call_results t auto =
    let autosp = (fun a   -> young_end (wordsize t) t.stack_growth a.A.overflow) in
    C.incoming ~growth:t.stack_growth ~sp:(R.reg t.sp)
      ~mkauto:(mk_automaton (wordsize t) (memspace t) "in ovfl results" auto.A.results)
      ~autosp
      ~postsp:(fun _ _ -> std_sp_location (wordsize t)) 
      ~insp:(fun a _ _ -> autosp a)
(*x: callspec.ml *)
let also_cuts_to t auto =
    let autosp = (fun r   -> std_sp_location (wordsize t)) in
    C.incoming ~growth:t.stack_growth ~sp:(R.reg t.sp)
      ~mkauto:(mk_automaton (wordsize t) (memspace t) "in cont parms" auto.A.cutto)
      ~autosp
      ~postsp:(fun _ _ -> std_sp_location (wordsize t)) 
      ~insp:(fun a _ _ -> autosp a)

let cut_actuals t auto base =
    C.outgoing ~growth:t.stack_growth ~sp:(R.reg t.sp) 
        ~mkauto:(fun ()  -> A.at (memspace t) ~start:base auto.A.cutto)
        ~autosp:(fun r   -> R.fetch (R.reg t.sp) (wordsize t))
        ~postsp:(fun _ _ -> R.fetch (R.reg t.sp) (wordsize t))

(*x: callspec.ml *)
let ra_on_entry t block = R.fetch (ra t) (wordsize t)
(*x: callspec.ml *)
let ra_on_exit t saved_ra block temp = match style t with
    | ReturnAddress.KeepInPlace  -> saved_ra 
    | ReturnAddress.SaveToTemp s -> Talloc.Multiple.loc temp s (wordsize t) 

let where_to_save_ra t = fun ra_on_entry temp -> match style t with
    | ReturnAddress.KeepInPlace  -> ra t
    | ReturnAddress.SaveToTemp s -> Talloc.Multiple.loc temp s (wordsize t) 

let to_call ~cutto ~return auto t =
    (*s: it's impossible for the stack pointer to be a useful register too *)
    if Register.Set.mem t.sp t.all_regs then
      Impossible.impossible
        (Printf.sprintf "In convention \"%s\", stack pointer is also an ordinary register"
           t.name)
    else
      ()
    (*e: it's impossible for the stack pointer to be a useful register too *)
    ;
    let volregs = Register.Set.diff t.all_regs t.nv_regs in
    { C.name           = t.name
    ; C.overflow_alloc = t.overflow
    ; C.call_parms     = {C.in'=prolog       t auto; C.out=call_actuals t auto}
    ; C.cut_parms      = {C.in'=also_cuts_to t auto; C.out=cut_actuals  t auto}
    ; C.results        = {C.in'=call_results t auto; C.out=epilog       t auto}
    
    ; C.stack_growth     = t.stack_growth
    ; C.stable_sp_loc    = std_sp_location (wordsize t)
    ; C.jump_tgt_reg     = Rtl.reg (Register.Set.choose volregs)
    ; C.replace_vfp      = Vfp.replace_with ~sp:(R.reg t.sp)
    ; C.sp_align         = t.sp_align
    ; C.ra_on_entry      = ra_on_entry t
    ; C.where_to_save_ra = where_to_save_ra t
    ; C.ra_on_exit       = ra_on_exit t
    ; C.sp_on_unwind     = (fun _   -> Rtl.null)
    ; C.sp_on_jump       = (fun _ _ -> Rtl.null)
    ; C.pre_nvregs       = t.nv_regs
    ; C.volregs          = volregs
    ; C.saved_nvr        = t.save_nvr
    ; C.return           = return
    }
(*e: callspec.ml *)
