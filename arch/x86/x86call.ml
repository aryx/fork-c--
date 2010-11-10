(*s: x86call.ml *)
module A  = Automaton
module C  = Call
module R  = Rtl
module RO = Rewrite.Ops
module RP = Rtl.Private
module RS = Register.Set
module RU = Rtlutil
module T  = Target
let impossf fmt = Printf.kprintf Impossible.impossible fmt
(*x: x86call.ml *)
let rspace = X86regs.rspace
let eax = X86regs.eax 
let ecx = X86regs.ecx 
let edx = X86regs.edx 
let ebx = X86regs.ebx 
let esp = X86regs.esp 
let ebp = X86regs.ebp 
let esi = X86regs.esi 
let edi = X86regs.edi 
let vfp = Vfp.mk 32
(*x: x86call.ml *)
let volregs = RS.of_list [eax; ecx; edx]
let nvregs  = RS.of_list [ebx; esi; edi; ebp]
let allregs = RS.union volregs nvregs
(*x: x86call.ml *)
let saved_nvr temps =
  let t = Talloc.Multiple.loc temps 't' in fun r -> t (Register.width r)
(*x: x86call.ml *)
let sp       = R.reg esp
let spval    = R.fetch sp 32
let sp_align = 4
let growth   = Memalloc.Down
let bo       = R.LittleEndian
let memsize  = 8
(*x: x86call.ml *)
let ( *> ) = A.( *> )
(*x: x86call.ml *)
let badwidth what w = impossf "Unsupported (rounded) width %d in x86 %s" w what
let imp _ = Impossible.impossible "grave miscalculation in automaton"
(*x: x86call.ml *)
let stack_top_proxy_reg = (('\000', Rtl.Identity, Cell.of_size 80), 0, Rtl.C 1)
(*x: x86call.ml *)
let mspace = ('m', bo, Cell.of_size 8)
let amem = R.mem (R.aligned 4) mspace (R.C 4)
let ra = amem vfp
let const x _ = x
(*x: x86call.ml *)
let addk = RU.addk 32
let add  = RU.add  32
let std_sp_location = add vfp (R.late "minus frame size" 32)
(*x: x86call.ml *)
let rtn return_to k n ~ra =
  if k > n then impossf "Return <k/n> has k:%d > n:%d\n" k n
  else return_to ra

let conv ~preserved ~jump ~struct' ~altret name specs ~return_to cut =
  (*s: functions to transform automata *)
  let autoAt = A.at mspace in
  let call_actuals  =
    C.outgoing ~growth ~sp
     ~mkauto:(fun () -> Block.srelative vfp "out call parms" autoAt specs.A.call)
     ~autosp:(fun r -> Block.base r.A.overflow)
     ~postsp:(fun _ sp -> sp) in  (* was ~postsp:(fun _ -> std_sp_location) *)
  let prolog  =
    let autosp = (fun _ -> vfp) in
    C.incoming ~growth ~sp
     ~mkauto:(fun () -> autoAt (addk vfp 4) specs.A.call)
     ~autosp
     ~postsp:(fun _ _ -> std_sp_location)
     ~insp:(fun a _ _ -> autosp a) in
     (* alternate: ~mkauto:Block.relative vfp "in call parms" specs.A.call *)
     (*   ~autosp:(fun r -> addk (Block.base r.A.overflow) (-4)) *)
  (*x: functions to transform automata *)
  let c_call_results  =
    let autosp = (fun r -> Block.base r.A.overflow) in
    C.incoming ~growth ~sp
     ~mkauto:(fun () -> Block.srelative vfp "in ovfl results" autoAt specs.A.results)
     ~autosp
     ~postsp:(fun _ _ -> std_sp_location)     (* irrelevant? *)
     ~insp:(fun a _ _ -> autosp a) in
  let c_returns_struct_call_results  =
    let autosp = (fun r -> addk (Block.base r.A.overflow) 4) in
    C.incoming ~growth ~sp
     ~mkauto:(fun () -> Block.srelative vfp "in ovfl results" autoAt specs.A.results)
     ~autosp
     ~postsp:(fun _ _ -> std_sp_location)     (* irrelevant? *)
     ~insp:(fun a _ _ -> autosp a) in
  let cmm_call_results  =
    C.incoming ~growth ~sp
      ~mkauto:(fun () -> Block.srelative vfp "in ovfl results" autoAt specs.A.results)
      ~autosp:(fun r -> Block.base r.A.overflow)
      ~postsp:(fun _ _ -> std_sp_location)     (* irrelevant? *)
      ~insp:(fun a _ out_parm_block -> Block.base a.A.overflow) in
  let epilog  =
    C.outgoing ~growth ~sp
      ~mkauto:(fun () -> Block.srelative vfp "out ovfl results" autoAt specs.A.results)
      ~autosp:(fun r -> Block.base r.A.overflow)
      ~postsp:(fun _ r -> addk r (-4)) in
  let epilog_returns_struct  =
    C.outgoing ~growth ~sp
      ~mkauto:(fun () -> Block.srelative vfp "out ovfl results" autoAt specs.A.results)
      ~autosp:(fun r -> addk (Block.base r.A.overflow) 4)
      ~postsp:(fun _ r -> addk r (-4)) in
  (*x: functions to transform automata *)
  let also_cuts_to =
    let autosp = (fun _ -> std_sp_location) in
    C.incoming ~growth ~sp
      ~mkauto:(fun () -> Block.srelative vfp "in cont parms" autoAt specs.A.cutto)
      ~autosp
      ~postsp:(fun _ _ -> std_sp_location)
      ~insp:(fun a _ _ -> autosp a) in

  let cut_actuals cont  =
    C.outgoing ~growth ~sp
       ~mkauto:(fun () -> autoAt (Contn.ovblock_exp cont 8 32 sp_align) specs.A.cutto)
       ~autosp:(fun r -> spval)  (* should this be std_sp_location? *)
       ~postsp:(fun _ _ -> spval) in
  (*e: functions to transform automata *)
  let return k n ~ra =
    if altret then rtn return_to k n ~ra
    else if k = 0 & n = 0 then return_to ra
    else impossf "alternate return not allowed in %s calling convention" name in
  let nvregs  = preserved in
  let volregs = RS.diff allregs preserved in
  { C.name       = name
  ; C.overflow_alloc  = 
      { C.parameter_deallocator = if jump then C.Callee else C.Caller
      ; C.result_allocator      = C.Caller
      }
  ; C.call_parms = { C.in' = prolog;       C.out = call_actuals}
  ; C.cut_parms  = { C.in' = also_cuts_to; C.out = cut_actuals}
  ; C.results    = { C.in' = if jump then cmm_call_results
                             else if struct' then c_returns_struct_call_results
                             else c_call_results
                   ; C.out = if struct' then epilog_returns_struct else epilog}

  ; C.stack_growth     = Memalloc.Down
  ; C.stable_sp_loc    = std_sp_location
  ; C.jump_tgt_reg     = Rtl.reg edx
  ; C.replace_vfp      = Vfp.replace_with ~sp
  ; C.sp_align         = 4               (* alignment of stack pointer at call/cut *)
  ; C.ra_on_entry      = const (R.fetch ra 32)   (* where return address is on entry *)
  ; C.where_to_save_ra = (fun _ t -> Talloc.Multiple.loc t 't' 32)
      (* can't afford to leave ra where it is, even for the C convention,
         because we might make a tail call from C to C-- *)
  ; C.ra_on_exit       = 
        (let adjust = if struct' then 0 else -4 in
        (fun _ b _ -> amem (addk (Block.base b) adjust)))
  ; C.sp_on_unwind     = (fun exp -> RU.store sp exp)
  ; C.sp_on_jump       =
      if jump then (fun b _ -> RU.store sp (addk (Block.base b) (-4)))
      else (fun _ _ ->impossf "tail calls not supported by %s calling convention" name)
  ; C.pre_nvregs       = nvregs          (* registers preserved across calls *)
  ; C.volregs          = volregs         (* registers not preserved across calls *)
  ; C.saved_nvr        = saved_nvr
  ; C.return           = return
  } 
(*x: x86call.ml *)
let thread cut specs =
  let outgoing _ _ = impossf "called out using thread convention" in
  let nocut    _ _ = impossf "cut to or continuation using thread convention" in
  let noreturn _ _ = impossf "return using thread convention" in
  let incoming types formals = match types, formals with
    | [_; _], [_; _] ->
        let a = A.at mspace vfp specs.A.call in
        let crank effects' (w, k, al) formal =
          let l = A.allocate a w k al in
          A.store formal (A.fetch l w) w :: effects' in
        let shuffle = R.par (List.rev (List.fold_left2 crank [] types formals)) in
        let a = A.freeze a in
        let _autosp = vfp in
        let postsp = std_sp_location in
        let _insp   = vfp in
        let setsp  = RU.store sp postsp in
        { C.overflow = a.A.overflow
        ; C.insp     = (fun b -> RU.store sp vfp)
        ; C.regs     = a.A.regs_used
        ; C.shuffle  = shuffle
        ; C.post_sp  = R.guard (RO.ne 32 spval postsp) setsp
        ; C.pre_sp   = R.guard (RO.gt 32 spval postsp) setsp
        }
    | _ -> impossf "thread convention with %d parameters" (List.length formals) in
  let _nvregs  = RS.empty in
  let _volregs = allregs in
  { C.name  = "C-- thread"
  ; C.overflow_alloc  = 
      { C.parameter_deallocator = C.Caller
      ; C.result_allocator      = C.Caller
      }
  ; C.call_parms = { C.in' = incoming; C.out = outgoing}
  ; C.cut_parms  = { C.in' = nocut;    C.out = (fun _ -> nocut)}
  ; C.results    = { C.in' = noreturn; C.out = noreturn }
  ; C.stack_growth     = Memalloc.Down
  ; C.stable_sp_loc    = std_sp_location
  ; C.jump_tgt_reg     = Rtl.reg edx
  ; C.replace_vfp      = Vfp.replace_with ~sp
  ; C.sp_align         = 4               (* alignment of stack pointer at call/cut *)
  ; C.ra_on_entry      = const (R.fetch ra 32)   (* where return address is on entry *)
  ; C.where_to_save_ra = (fun _ t -> Talloc.Multiple.loc t 't' 32)
  ; C.ra_on_exit       = (fun _ _ t -> Talloc.Multiple.loc t 't' 32)
  ; C.sp_on_unwind     = (fun exp -> RU.store sp exp)
  ; C.sp_on_jump       = (fun _ _ ->impossf "tail calls not supported by C-- thread calling convention")
  ; C.pre_nvregs       = RS.empty        (* registers preserved across calls *)
  ; C.volregs          = allregs         (* registers not preserved across calls *)
  ; C.saved_nvr        = saved_nvr
  ; C.return           = (fun _ _ ~ra -> impossf "return in C-- thread convention")
  } 
(*x: x86call.ml *)
let cconv ~return_to cut ccname stage =
  let cconv = conv ~struct':false in
  let f =
    match ccname with
    | "C--"         -> cconv ~jump:true  ~altret:true  ~preserved:nvregs
    | "C-- thread"  -> (fun ccname stage ~return_to cut -> thread cut stage)
    | "lightweight" -> cconv ~jump:true  ~altret:true  ~preserved:nvregs
    | "notail"      -> cconv ~jump:false ~altret:true  ~preserved:nvregs
    | "paranoid C"  -> cconv ~jump:false ~altret:false ~preserved:RS.empty
    | "C"           -> cconv ~jump:false ~altret:false ~preserved:nvregs
    | "C returns struct" -> conv ~struct':true ~jump:false ~altret:false ~preserved:nvregs
    | "gc"          -> cconv ~jump:false ~altret:false 
                            ~preserved:(RS.of_list [ecx;edx;ebx;esi;edi;ebp])
    | _ -> Unsupported.calling_convention ccname
  in f ccname stage ~return_to cut
(*e: x86call.ml *)
