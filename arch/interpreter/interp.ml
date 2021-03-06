(*s: interp.ml *)
let arch        = "interp"
let byteorder   = Rtl.LittleEndian       (* byte order *)
let wordsize    = 32
let pointersize = wordsize
let memsize     = 8
let mspace      = ('m', byteorder, Cell.of_size memsize)
(*x: interp.ml *)
module AN     = Automaton
module SS32   = Space.Standard32
module T      = Target
module PA     = Preast2ir
module DG     = Dag
module D      = Rtl.Dn  (* Down *)
module U      = Rtl.Up  (* Up   *)
module R      = Rtl
module RP     = Rtl.Private
module RU     = Rtlutil
module C      = Call
module RS     = Register.Set

let downrtl = R.Dn.rtl
let uploc   = R.Up.loc
let upexp   = R.Up.exp

module Spaces = struct
    let m   = SS32.m   byteorder    [8; 16; 32]
    let c   = Space.checked
                { Space.space          = ('c', Rtl.Identity, Cell.of_size wordsize)
                ; Space.doc            = "control registers"
                ; Space.indexwidth     = 2 
                ; Space.indexlimit     = Some 4 (* pc, sp, ap, ra *)
                ; Space.widths         = [32]
                ; Space.classification = Space.Reg
                } 
    let a   = Space.checked
                { Space.space          = ('A', byteorder, Cell.of_size 1)
                ; Space.doc            = "argument passing area"
                ; Space.indexwidth     = wordsize
                ; Space.indexlimit     = None
                ; Space.widths         = [8;16;32]
                ; Space.classification = Space.Mem
                }
    let d   = Space.checked
                { Space.space          = ('d', Rtl.Identity, Cell.of_size 2)
                ; Space.doc            = "FP rounding mode"
                ; Space.indexwidth     = 1
                ; Space.indexlimit     = Some 2
                ; Space.widths         = [2]
                ; Space.classification = Space.Reg
                }
end
(*x: interp.ml *)
let c = Spaces.c.Space.space
let pc:Rtl.loc            = Rtl.reg (c,0,Rtl.C 1)  (* pc pointer     *)
let sp:Rtl.loc            = Rtl.reg (c,1,Rtl.C 1)  (* stack pointer  *)
let ap:Rtl.loc            = Rtl.reg (c,2,Rtl.C 1)  (* arg pointer    *)
let ra:Rtl.loc            = Rtl.reg (c,3,Rtl.C 1)  (* return address *)

let jump_reg = -1
let arg_reg  x            = (Spaces.a.Space.space, x, Rtl.C 1)
let argument x            = R.reg (arg_reg x)          (* arguments *) 

let fp_mode               = Rtl.reg(Spaces.d.Space.space,0,Rtl.C 1) (* rounding mode *) 
let spval                 = Rtl.fetch sp wordsize
let vfp                   = Vfp.mk wordsize          
let std_sp_location       = vfp
let std_sp_location       = RU.add pointersize 
                             vfp (R.late "minus frame size" pointersize)
(*x: interp.ml *)
module Flow = struct
    (*s: module Flow *)
    let ewidth                = RU.Width.exp 
    let lwidth                = RU.Width.loc 
    (*x: module Flow *)
    let fetch loc     = R.fetch loc wordsize        
    let store loc e   = R.store loc e wordsize        
    let add           = RU.add  wordsize
    let addk          = RU.addk wordsize
    let sub           = R.opr "sub" [wordsize]
    let add1 loc      = addk (fetch loc) 1
    let sub1 loc      = R.app sub [ fetch loc
                                ; R.bits (Bits.S.of_int 1 wordsize) wordsize]
    let inc loc       = R.store loc (add1 loc) wordsize
    let dec loc       = R.store loc (sub1 loc) wordsize
    let error         = Impossible.impossible
    (*x: module Flow *)
    let bnegate _ = Impossible.unimp "bnegate not implemented"
    (*x: module Flow *)
    let goto = { Ep.embed   = (fun _ e -> (DG.Nop, store pc e))
               ; T.project = (fun r -> match D.rtl r with
                              | RP.Rtl [(_, RP.Store(_, e, _))] -> U.exp e
                              | _ -> error "projected non-goto"
                              )
               } 
    
    let cutto =
        { T.embed   = (fun _ {Mflow.new_pc=newpc; Mflow.new_sp= newsp} ->
                            let assign loc e =
                                Rtl.store loc e (Rtlutil.Width.exp e) in
                            (DG.Nop, Rtl.par [assign sp newsp; assign pc newpc]))
         ; T.project = (fun r -> match downrtl r with
                          | RP.Rtl [ (_, RP.Store(_, nsp, _))
                                   ; (_, RP.Store(_ , npc, _))] ->
                            {Mflow.new_sp=upexp nsp; Mflow.new_pc= upexp npc}
                          | _ -> Impossible.impossible "projected non-cutto")
         }
    let call = 
        { T.embed   = (fun _ e -> (DG.Nop, R.par [store ra (fetch pc); store pc e]))
        ; T.project = (fun r -> match D.rtl r with
                      | RP.Rtl [_;(_,RP.Store(_,e,_))] -> U.exp e
                      | RP.Rtl [(_,RP.Store(_,e,_))] -> U.exp e
                      | _ -> error (Printf.sprintf "projected non-call: %s"
                                                   (RU.ToString.rtl r))
                      )
        }              

    let return = store pc (fetch ra)

    let branch =
        { T.embed   = (fun _ c -> DG.cond (fun e -> R.guard c (store pc e)))
        ; T.project = (fun r      -> match D.rtl r with
                      | RP.Rtl [(c, (RP.Store(_,_,_)))] -> U.exp c
                      | _ -> error "projected non-branch"
                      )
        }              

    (*let jump   = goto                   (* must be different from goto!  *)*)
    let jump = { Ep.embed   = (fun _ e -> (DG.Nop, store pc e))
               ; Ep.project = goto.Ep.project
               } 
    let cutto  = cutto
    (* let return = store pc (fetch ra)    (* no embed/project needed *)*)
    (*x: module Flow *)
    let store_space spc i e = R.store (spc i) e (ewidth e)
    let fetch_space spc i w = R.fetch (spc i) w
    (*e: module Flow *)
end
(*x: interp.ml *)
let seq (get : int -> Rtl.width -> Register.t list ref -> AN.loc) next =
    let index = ref 0  in
    let regs  = ref [] in
    let allocate ~width ~alignment ~kind =
      let n   = !index in
      let loc = get n width regs in
      index := !index + 1;
      loc in
    let freeze rs mems =
        next.AN.freeze (RS.union (RS.of_list (!regs)) rs) mems in
    { AN.allocate = allocate; AN.freeze   = freeze }

let getreg reg index _ regs =
  regs  := (reg index) :: !regs;
  AN.of_loc (Rtl.reg (reg index))

let seq reg = AN.wrap (seq reg)

let params = seq (getreg arg_reg)
let globals base =
  let get i w _ = AN.of_loc (Rtl.global "unknown name" i w) in
  AN.at ~start:base mspace (seq get)

let ccspecs     = { AN.call = params; AN.results = params; AN.cutto = params }
(*x: interp.ml *)
let cc' = (* new style *)
    let growth = Memalloc.Down            in (* irrelevant *) 
    (*s: transformations *)
    let autoAt = AN.at mspace in
    let call_actuals  =
      C.outgoing ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "out call parms" autoAt params) 
        ~autosp:(fun r  -> Block.base r.AN.overflow)
        ~postsp:(fun _ sp -> sp) in  

    let prolog  =
      let autosp = (fun _ -> vfp) in
      C.incoming ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "in call parms" autoAt params)
        ~autosp
        ~postsp:(fun _ _ -> std_sp_location)
        ~insp:(fun a _ _ -> autosp a) in

    let call_results  =
      let autosp = (fun r -> Block.base r.AN.overflow) in
      C.incoming ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "in ovfl results" autoAt params)
        ~autosp
        ~postsp:(fun _ _ -> std_sp_location)
        ~insp:(fun a _ _ -> autosp a) in

    let epilog  =
      C.outgoing ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "out ovfl results" autoAt params)
        ~autosp:(fun r -> Block.base r.AN.overflow)
        ~postsp:(fun _ _ -> vfp) in

    let also_cuts_to =
      let autosp = (fun r -> std_sp_location) in
      C.incoming ~growth ~sp
        ~mkauto:(fun () -> Block.srelative vfp "in cont parms" autoAt params)
        ~autosp
        ~postsp:(fun _ _ -> std_sp_location)
        ~insp:(fun a _ _ -> autosp a) in

    let cut_actuals base  =
       C.outgoing ~growth ~sp ~mkauto:(fun () -> autoAt base params)
         ~autosp:(fun r -> spval)
         ~postsp:(fun _ _ -> spval) in
    (*e: transformations *)
    { C.name             = "interp"
    ; C.overflow_alloc  = { C.parameter_deallocator = C.Callee
                          ; C.result_allocator      = C.Caller
                          }
    ; C.call_parms       = { C.in' = prolog;       C.out = call_actuals}
    ; C.cut_parms        = { C.in' = also_cuts_to; C.out = cut_actuals}
    ; C.results          = { C.in' = call_results; C.out = epilog}
 
    ; C.stack_growth     = growth
    ; C.stable_sp_loc    = std_sp_location
    ; C.jump_tgt_reg     = argument jump_reg
    ; C.replace_vfp      = Vfp.replace_with ~sp
    ; C.sp_align         = 1             
    ; C.pre_nvregs       = Register.Set.empty
    ; C.volregs          = Register.Set.empty
    ; C.saved_nvr        = (fun _ _ -> Impossible.impossible "saved regs")
    ; C.return           = (fun k n ~ra -> Flow.return)
    ; C.ra_on_entry      = (fun _     -> R.fetch ra wordsize)
    ; C.where_to_save_ra = (fun _ t   -> ra)
    ; C.ra_on_exit       = (fun t b _ -> ra)
    ; C.sp_on_jump       = (fun _ _ -> Rtl.null)
    ; C.sp_on_unwind     = (fun e ->   RU.store sp e)
    }
(*x: interp.ml *)
let target' = (* new style *)
  let spaces = [ Spaces.m ; Spaces.c ; Spaces.a ] in
  let mincap = T.minimal_capabilities 32 in
  PA.T { T.name     = arch
       ; T.tx_ast = (fun secs -> secs)
       ; T.capabilities      =
            (*  Run:
             *    gcc -E operator.c|grep 'register_operator("' \
             *    | perl -ne '/("[^"]*")/; print "; $1, ["; while (/"bits(\d+)/g) { print "$1;" }; print "]\n"' \
             *    | sort
             *  and correct the output, looking at rtlop.nw.  Some ideas for checking the output:
             *  You could try:
             *    s/32;32;/32;/g
             *    s/16;16;/16;/g
             *    s/64;64;/64;/g
             *  You might try looking at:
             *     grep -v "T.var 1" rtlop.nw
             *     grep "T.var 2" rtlop.nw
             *"
             *)
                     {mincap with
                        T.operators = 
                       List.map Rtl.Up.opr [
     "add", [16;]
    ; "add", [32;]
    ; "addc", [32;]
    ; "add_overflows", [32;]
    ; "and", [16;]
    ; "and", [32;]
    ; "bit", []
    ; "bool", []
    ; "borrow", [32;]
    ; "carry", [32;]
    ; "com", [16;]
    ; "com", [32;]
    ; "conjoin", []
    ; "disjoin", []
    ; "div", [32;]
    ; "div_overflows", [32;]
    ; "divu", [16;]
    ; "divu", [32;]
    ; "eq", [16;]
    ; "eq", [2;2;]
    ; "eq", [32;]
    ; "f2f", [32;64;]
    ; "f2f", [64;32;]
    ; "f2i", [32;32;]
    ; "f2i", [64;32;]
    ; "fabs", [32;]
    ; "fabs", [64;]
    ; "fadd", [32;]
    ; "fadd", [64;]
    ; "false", []
    ; "fdiv", [32;]
    ; "fdiv", [64;]
    ; "feq", [32;]
    ; "feq", [64;]
    ; "fge", [32;]
    ; "fge", [64;]
    ; "fgt", [32;]
    ; "fgt", [64;]
    ; "fle", [32;]
    ; "fle", [64;]
    ; "float_eq", []
    ; "float_gt", []
    ; "float_lt", []
    ; "flt", [32;]
    ; "flt", [64;]
    ; "fmul", [32;]
    ; "fmul", [64;]
    ; "fmulx", [32;64;]
    ; "fne", [32;]
    ; "fne", [64;]
    ; "fneg", [32;]
    ; "fneg", [64;]
    ; "fordered", [32;]
    ; "fordered", [64;]
    ; "fsqrt", [32;]
    ; "fsqrt", [64;]
    ; "fsub", [32;]
    ; "fsub", [64;]
    ; "funordered", [32;]
    ; "funordered", [64;]
    ; "ge", [16;]
    ; "ge", [32;]
    ; "geu", [16;]
    ; "geu", [32;]
    ; "gt", [16;]
    ; "gt", [32;]
    ; "gtu", [16;]
    ; "gtu", [32;]
    ; "i2f", [32;32;]
    ; "i2f", [32;64;]
    ; "le", [16;]
    ; "le", [32;]
    ; "leu", [16;]
    ; "leu", [32;]
    ; "lobits", [16;8;]
    ; "lobits", [32;1;]
    ; "lobits", [32;16;]
    ; "lobits", [32;32;]
    ; "lobits", [32;8;]
    ; "lt", [16;]
    ; "lt", [32;]
    ; "ltu", [16;]
    ; "ltu", [32;]
    ; "minf", [32;]
    ; "minf", [64;]
    ; "mod", [32;]
    ; "modu", [16;]
    ; "modu", [32;]
    ; "mul", [16;]
    ; "mul", [32;]
    ; "mul", [8;8;8;]
    ; "mul_overflows", [32;]
    ; "mulu_overflows", [32;]
    ; "mulux", [16;32;]
    ; "mulux", [8;8;16;]
    ; "mulx", [16;32;]
    ; "mulx", [8;8;16;]
    ; "mzero", [32;]
    ; "mzero", [64;]
    ; "NaN", [23;32;]
    ; "ne", [16;]
    ; "ne", [2;2;]
    ; "ne", [32;]
    ; "neg", [16;]
    ; "neg", [32;]
    ; "not", []
    ; "or", [16;]
    ; "or", [32;]
    ; "pinf", [32;]
    ; "pinf", [64;]
    ; "popcnt", [16;]
    ; "popcnt", [32;]
    ; "pzero", [32;]
    ; "pzero", [64;]
    ; "quot", [16;]
    ; "quot", [32;]
    ; "quot_overflows", [32;]
    ; "rem", [16;]
    ; "rem", [32;]
    ; "rotl", [16;]
    ; "rotl", [32;]
    ; "rotr", [16;]
    ; "rotr", [32;]
    ; "round_down", []
    ; "round_nearest", []
    ; "round_up", []
    ; "round_zero", []
    ; "shl", [16;]
    ; "shl", [32;]
    ; "shra", [16;]
    ; "shra", [32;]
    ; "shrl", [16;]
    ; "shrl", [32;]
    ; "sub", [16;]
    ; "sub", [32;]
    ; "subb", [32;]
    ; "sub_overflows", [32;]
    ; "sx", [1;32;]
    ; "sx", [16;32;]
    ; "sx", [32;32;]
    ; "sx", [8;16;]
    ; "sx", [8;32;]
    ; "true", []
    ; "unordered", []
    ; "xor", [16;]
    ; "xor", [32;]
    ; "zx", [1;32;]
    ; "zx", [16;32;]
    ; "zx", [32;32;]
    ; "zx", [8;16;]
    ; "zx", [8;32;]
    ]}
       ; T.cc_specs = [ "C", ccspecs ; "C--", ccspecs ]
       ; T.cc_spec_to_auto   = (fun _ _ -> cc')
       ; T.is_instruction    = (fun _ -> true)   (* wildly optimistic *)
       ; T.byteorder         = byteorder
       ; T.wordsize          = wordsize
       ; T.pointersize       = wordsize
       ; T.vfp               = Vfp.mk wordsize
       ; T.alignment         = 1
       ; T.memspace          = mspace
     ; T.max_unaligned_load  = R.C 1
       ; T.memsize           = memsize
       ; T.float             = Float.ieee754
       ; T.charset           = "latin1" 
       ; T.globals           = globals
       ; T.rounding_mode     = fp_mode
       ; T.named_locs        = Strutil.assoc2map 
                                 ["IEEE 754 rounding mode", fp_mode;
                                 ]
       ; T.spaces            = spaces
       ; T.reg_ix_map        = T.mk_reg_ix_map spaces
       ; T.distinct_addr_sp  = false
       ; T.data_section      = "data"
       ; T.machine = { Mflow.bnegate     = Flow.bnegate
                     ; Mflow.goto        = Flow.goto
                     ; Mflow.jump        = Flow.jump
                     ; Mflow.call        = Flow.call 
                     ; Mflow.cutto       = Flow.cutto 
                     ; Mflow.return      = Flow.return
                     ; Mflow.retgt_br    = (fun _ -> assert false)
                     ; Mflow.move        = (fun _ -> assert false)
                     ; Mflow.spill       = (fun _ -> assert false)
                     ; Mflow.reload      = (fun _ -> assert false)
                     ; Mflow.branch      = Flow.branch
                     ; Mflow.forbidden   =
                         Flow.store pc (Rewrite.Ops.unsigned wordsize 0)
                     }
       }
(*e: interp.ml *)
