(*s: sparccall.ml *)
(*s: sparccall.ml  *)
module R  = Rtl
module RU = Rtlutil
module RS = Register.Set
module Rg = Sparcregs
module A  = Automaton
module C  = Call
let sprintf = Printf.sprintf
let impossf fmt = Printf.kprintf Impossible.impossible fmt
(*x: sparccall.ml  *)
let rspace = Rg.Spaces.r.Space.space
let fspace = Rg.Spaces.f.Space.space
let dspace = Rg.Spaces.d.Space.space

let r i = (rspace, i, R.C 1)
let f i = (fspace, i, R.C 1)
let x i = (fspace, 8+2*i, R.C 2)
let oreg i = r (8 + i)   (* %o(i) *)

let ( *> ) = A.( *> )

(* claude: SPARC register windows mean the SAME automaton cannot
 * correctly serve both directions for integer/pointer results: the
 * callee produces its own return value via %i0/%i1 (see sparccc.ml's
 * c_results, used below by `epilog` - correct for that direction), but
 * the CALLER reads a just-made call's result via %o0/%o1 - same
 * physical register file, different window-relative name depending on
 * which side of the `save`/`restore` boundary you're on. Float results
 * have no such split (there is only one %f bank, not windowed), so only
 * the integer/pointer branch differs. Mirrors sparccc.ml's c_results
 * shape but swapped to o-registers; kept local here since it only
 * matters for this one incoming direction (`call_results` below, not
 * `epilog`). Root-caused empirically: fork-tiger's runtime.c--'s
 * "getenv" call read its own %i0 (an unrelated incoming argument
 * register) instead of the freshly-returned %o0, so a NULL-returning
 * getenv looked non-NULL and the following atoi(garbage) segfaulted. *)
let call_results_via_o =
  A.choice
    [ A.is_kind "float", A.useregs (List.map f (Auxfuns.from 0 ~upto:7)) false
    ; A.is_any, A.widen (Auxfuns.round_up_to ~multiple_of:32)
                *> A.widths [32; 64]
                *> A.useregs [oreg 0; oreg 1] false
    ]

let vol_regs    = RS.of_list (List.map r ([1;2;3;4;31] @ Auxfuns.from 16 ~upto:23))
let nv_regs     = RS.of_list (List.map r [])
let vol_fp      = 
   RS.of_list ( List.map f (Auxfuns.from 0 ~upto:7)
              @ List.map x (Auxfuns.from 0 ~upto:7)
              )
let all_regs    = List.fold_left (fun acc e -> RS.union acc e) RS.empty
                    [ vol_regs; nv_regs; vol_fp ]

let saved_nvr temps = 
    let t = Talloc.Multiple.loc temps 't' in
    let u = Talloc.Multiple.loc temps 'u' in
    let q = Talloc.Multiple.loc temps 'q' in
    fun (((sp, _, _), i, _) as reg) ->
      let bad () = impossf "cannot save $%c%d" sp i in
      let w = Register.width reg in
      (match sp with 'r' -> t | 'f' -> u | 'x' -> q | _ -> bad()) w
(*x: sparccall.ml  *)
let ra       = R.reg (r 31)
let spr      = r 14
let sp       = R.reg spr
let spval    = R.fetch sp 32
let sp_align = 16
let growth   = Memalloc.Down
let bo       = R.BigEndian
(*x: sparccall.ml  *)
let wordsize  = 32
let memsize   = 8
let byteorder = bo
let mspace    = Rg.Spaces.m.Space.space
let vfp       = Vfp.mk wordsize
let std_sp_location = RU.add wordsize vfp (R.late "minus frame size" wordsize)
let mk_automaton block_name automaton () =
  Block.srelative vfp block_name (A.at mspace) automaton

let old_end   block = RU.addk wordsize (Block.base block) (Block.size block)
let young_end block = Block.base block
(*x: sparccall.ml  *)
let tail_overflow = 
    { C.parameter_deallocator = C.Callee
    ; C.result_allocator      = C.Caller
    }

let c_overflow =
    { C.parameter_deallocator = C.Caller
    ; C.result_allocator      = C.Caller
    } 
(*x: sparccall.ml  *)
let prolog auto = 
  let autosp a = young_end a.A.overflow in
  C.incoming ~growth:growth ~sp:sp
    ~mkauto:(mk_automaton "in call parms" auto.A.call)
    ~autosp:autosp
    ~postsp:(fun _ _ -> std_sp_location) 
    ~insp:(fun a _ _ -> autosp a)
(*x: sparccall.ml  *)
let epilog auto =
    C.outgoing ~growth:growth ~sp:sp
        ~mkauto:(mk_automaton "out ovfl results" auto.A.results)
        ~autosp:(fun r -> std_sp_location)
        ~postsp:(fun _ _ -> vfp)
(*x: sparccall.ml  *)
let call_actuals auto =
    C.outgoing ~growth:growth ~sp:sp
        ~mkauto:(mk_automaton "out call parms" auto.A.call)
        ~autosp:(fun r  -> std_sp_location)
        ~postsp:(fun a sp -> std_sp_location)
(*x: sparccall.ml  *)
(* claude: unlike epilog below (which reuses auto.A.results, and so picks
 * up "C--"'s extra overflow_up stage via Sparccc.cmm_results), this
 * always uses the bare 2-register call_results_via_o with no overflow
 * stage - a call returning more values than fit in %o0/%o1 isn't
 * handled yet. Not believed to be exercised by anything simple-program-
 * shaped (a "C" convention runtime call returns at most one pointer/int
 * here); flagged rather than solved so a real failure is loud instead of
 * silently wrong. *)
let call_results auto =
    let autosp = (fun a -> std_sp_location) in
    C.incoming ~growth:growth ~sp:sp
        ~mkauto:(mk_automaton "in ovfl results" call_results_via_o)
        ~autosp:autosp
        ~postsp:(fun _ _ -> std_sp_location)
        ~insp:(fun a _ _ -> std_sp_location)
(*x: sparccall.ml  *)
let also_cuts_to auto =
    let autosp = (fun r   -> std_sp_location) in
    C.incoming ~growth:growth ~sp:sp
        ~mkauto:(mk_automaton "in cont parms" auto.A.cutto)
        ~autosp:autosp
        ~postsp:(fun _ _ -> std_sp_location) 
        ~insp:(fun a _ _ -> autosp a)

let cut_actuals auto base =
    C.outgoing ~growth:growth ~sp:sp
        ~mkauto:(fun ()   -> A.at mspace ~start:base auto.A.cutto)
        ~autosp:(fun r    -> std_sp_location)
        ~postsp:(fun a sp -> std_sp_location)
(*x: sparccall.ml  *)
let ra_on_entry block = R.fetch ra wordsize
let where_to_save_ra ra_on_entry temp = Talloc.Multiple.loc temp 't' 32
(* ra *)
(*x: sparccall.ml  *)
let rtn ccname return_to k n ~ra =
  if k > n then impossf "Return <k/n> has k:%d > n:%d\n" k n
  else return_to ra
(*
    if k = 0 && n = 0 then return_to ra
    else Impossible.impossible ("alternate return using "^ccname^" calling convention")
*)
(*x: sparccall.ml  *)
(* Really inteded for the C convention *)
let to_call ~cutto ~return_to ~altret ccname auto =
    (*s: [[Sparccall]] it's impossible for the stack pointer to be a useful register too *)
    if Register.Set.mem spr all_regs then
      Impossible.impossible
        (Printf.sprintf "In convention \"%s\", stack pointer is also an ordinary register"
           ccname)
    else
      ()
    (*e: [[Sparccall]] it's impossible for the stack pointer to be a useful register too *)
    ;
    let return k n ~ra =
      if altret then rtn ccname return_to k n ~ra
      else if k = 0 && n = 0 then return_to ra
      else impossf "alternate return not allowed in %s calling convention" ccname
    in
    { C.name           = ccname
    ; C.overflow_alloc = c_overflow
    ; C.call_parms     = {C.in'=prolog       auto; C.out=call_actuals auto}
    ; C.cut_parms      = {C.in'=also_cuts_to auto; C.out=cut_actuals  auto}
    ; C.results        = {C.in'=call_results auto; C.out=epilog       auto}
    
    ; C.stack_growth     = growth
    ; C.stable_sp_loc    = std_sp_location
    (* claude: reserved for indirect jumps (ast2ir.ml stores the target here
     * before jumping through it); r5 is otherwise unused by this backend -
     * not zero/sp/fp/ra, not in vol_regs/nv_regs. Mirrors ppc.ml's rreg 7
     * and x86call.ml's edx, both similarly arbitrary volatile picks. *)
    ; C.jump_tgt_reg     = R.reg (r 5)
    ; C.replace_vfp      = Vfprewrite.replace_with ~sp:sp
    ; C.sp_align         = sp_align
    ; C.ra_on_entry      = ra_on_entry
    ; C.where_to_save_ra = where_to_save_ra
    ; C.ra_on_exit       = (fun _ _ t -> ra)
    ; C.sp_on_unwind     = (fun e -> RU.store sp e)
    (* claude: was "fun _ _ -> Rtl.null" - a genuine no-op, same bug
     * ppc.ml's sp_on_jump had (see its own comment): a tail call
     * ("jump") never restored sp before re-entering the target's own
     * prologue, which unconditionally does its own "save %sp,-N,%sp".
     * Every tail-recursive iteration would therefore shrink sp by
     * another frame instead of reusing the current one. Mirrors
     * ppc.ml's fix (RU.store sp (addk (Block.base b) (-24)), its own
     * linkage-area constant) using this backend's -96 reserved
     * register-window-save + minimum-outgoing-arg area (see
     * arch/sparc/sparcbackend.ml's window_and_arg_area_size - not
     * referenced directly to avoid a backwards dependency, but must
     * stay in sync with it). *)
    ; C.sp_on_jump       = (fun b _ -> RU.store sp (RU.addk wordsize (Block.base b) (-96)))
    ; C.pre_nvregs       = nv_regs
    ; C.volregs          = Register.Set.diff all_regs nv_regs
    ; C.saved_nvr        = saved_nvr
    ; C.return           = return
    }
(*x: sparccall.ml  *)
let c ~altret ~return_to cut ccname auto =
  to_call ~altret ~cutto:cut ~return_to ccname auto (* postprocess auto *)

let cconv ~return_to cut ccname stage =
  let f =
    match ccname with
    | "C--"        -> c ~altret:true
    | "notail"     -> c ~altret:true
    | "C"          -> c ~altret:false
    | "C-- thread" -> c ~altret:false (* damn lies *)
    | _            -> Impossible.unimp ccname
  in f ~return_to cut ccname stage
(*e: sparccall.ml  *)
(*e: sparccall.ml *)
