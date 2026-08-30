
# 1 "arch/arm64/arm64rec.mlb"

  (* claude: no upstream arm64rec.nw exists to port - qc-- predates AArch64
   * entirely (see arm64.ml's header comment). Modeled on arch/arm/
   * armrec.mlb's shape (%head/%tail structure, single flags-pseudo-location
   * cc model - AArch64 keeps ARM32's NZCV design) crossed with arch/riscv64/
   * riscv64rec.mlb's 64-bit width. Only the subset of AArch64 actually
   * needed by demos/hello_arm64.c-- is covered: base+offset addressing at
   * the full 64-bit width only (no sub-word loads/stores - see arm64.ml's
   * T.capabilities comment, T.memory is [64] only, not [8;16;32;64] the way
   * arm.ml's/riscv64.ml's own are, specifically because getting AArch64's
   * W-register-vs-X-register distinction right for ldrb/ldrh/ldrsb/ldrsh/
   * ldrsw sub-word loads needs care this pass doesn't verify empirically -
   * left uncovered rather than risk a silently-wrong rule), add/sub/and/
   * mul/quot, the ten eq/ne/lt/le/gt/ge/ltu/leu/gtu/geu comparisons, and
   * plain direct/indirect calls and branches.
   *
   * Immediate materialization: unlike armrec.mlb's "ldr rX, =imm" (a GNU-as
   * pseudo-op that, for the 64-bit register form on Apple's LLVM-based
   * assembler, was empirically confirmed to mis-size its literal-pool entry
   * - see arm64.ml's header comment for the `otool -tv` evidence), every
   * immediate here is built explicitly via a movz + up to three movk
   * instructions (li64 below) - always correct, needs no assembler pseudo-
   * op cooperation, and works identically on both GNU as and LLVM's
   * integrated assembler. No native 12-bit add/sub-immediate encoding is
   * used either (AArch64 does have one, unlike ARM32's fully rotated 8-bit
   * encoding) - every arithmetic immediate goes through the same li64 +
   * x16-scratch materialization, matching armrec.mlb's original "always
   * materialize, no direct-encode fast path" simplicity; worth revisiting
   * once real programs (not just hello_arm64.c--) exercise this backend.
   *)
  open Nopoly
  module RP = Rtl.Private
  module RU = Rtlutil
  module Up = Rtl.Up
  module Dn = Rtl.Dn
  module SS = Space.Standard64

  exception Error of string
  let error msg = raise (Error msg)
  let sprintf   = Printf.sprintf

  let guard p = if p then 0 else Camlburg.inf_cost

  (* claude: mutable syntax-select flag, set from to_string's ~mach
   * argument (see arm64mach.ml vs arm64asm.ml's own #instruction) - only
   * the address-of-symbol idiom below actually differs between the two
   * object formats; everything else this recognizer emits assembles
   * identically on both. Defaults to false (GNU-as/ELF syntax), not true:
   * ELF/Linux, not Mach-O, is this fork's actual target platform for
   * arm64/amd64 (see driver/main.ml's use_arm64 comment for the same
   * "bare name = ELF" convention at the CLI-flag level) - unlike
   * arch/ppc/ppcrec.mlb's own elf_syntax, which defaults to false (Mach-O)
   * for the opposite reason: -ppc's Mach-O is upstream's own original
   * target. *)
  let mach_syntax = ref false

  (* claude: computing a global/import symbol's ADDRESS via the adrp+add
   * page/page-offset idiom - Mach-O/ld64 spells the two relocations
   * "symbol@PAGE"/"symbol@PAGEOFF" (suffixes on the symbol operand); GNU
   * as/ELF spells the same two relocations as a bare "adrp reg, symbol"
   * (no suffix - R_AARCH64_ADR_PREL_PG_HI21) followed by
   * "add reg, reg, :lo12:symbol" (a prefix on the symbol operand instead
   * of a suffix - R_AARCH64_ADD_ABS_LO12_NC). Empirically confirmed: the
   * Mach-O-only original spelling was the FIRST bug hit bringing up
   * arm64asm.ml (aarch64-linux-gnu-as rejected "my_data@PAGE" outright:
   * "unexpected characters following instruction"). *)
  let adrp_add regl symbol =
    if !mach_syntax
    then sprintf "adrp %s, %s@PAGE\n\tadd %s, %s, %s@PAGEOFF" regl symbol regl regl symbol
    else sprintf "adrp %s, %s\n\tadd %s, %s, :lo12:%s" regl symbol regl regl symbol

  (* claude: signed decimal - used only for the "[reg, #imm]" load/store
   * offset field (a real AArch64 immediate operand, needs to print
   * negative offsets as "-8" not a huge unsigned value) and as the "next"
   * witness match below (whose string value is never actually used). *)
  let const64 b =
      assert (Bits.width b = 64);
      Int64.to_string (Bits.S.to_int64 b)

  let cat     = String.concat ""
  let sprintf = Printf.sprintf

  let reg n   = if n = 31 then "sp" else "x" ^ string_of_int n

  (* claude: the 32-bit ("W") alias of the same physical register - needed
   * for narrow (8/16/32-bit) stores: strb/strh/str(32-bit form) all take a
   * W-register source operand, there is no "strb x0,..." spelling on
   * AArch64 at all. Only used for stores (see the %head comment on why
   * sub-word loads remain out of scope: this is not needed for them since
   * the exp function below has no "sx"/"zx" case at all - the same latent,
   * pre-existing gap armrec.mlb's/riscv64rec.mlb's own Sx/Zx grammar rules
   * have, per notes_arm.txt - so those rules would never actually fire
   * regardless of register-name spelling). *)
  let wreg n  = if n = 31 then "wsp" else "w" ^ string_of_int n

  (* claude: AArch64's load/store size suffixes for the sub-word case are
   * NOT used here (see the %head comment above on why sub-word memory
   * access is out of scope for this pass) - kept only as a placeholder in
   * case that gap gets picked up later.
   *)

  (* claude: builds a movz + up to three movk instructions materializing an
   * arbitrary 64-bit constant into `dst` - see the %head comment above for
   * why this replaces armrec.mlb's "ldr rX, =imm" trick here. Always
   * correct: movz sets the first non-zero 16-bit chunk (or #0 if the whole
   * value is zero) and clears the rest, each subsequent non-zero chunk is
   * folded in with movk (which does not disturb the other bits). *)
  let li64 dst v =
    let chunk i = Int64.to_int (Int64.logand (Int64.shift_right_logical v (i*16)) 0xFFFFL) in
    let chunks = [ chunk 0; chunk 1; chunk 2; chunk 3 ] in
    if Int64.equal v 0L then sprintf "movz %s, #0" dst
    else
      let lines = ref [] in
      let first = ref true in
      List.iteri (fun i c ->
        if c <> 0 then begin
          if !first then begin
            lines := sprintf "movz %s, #%d, lsl #%d" dst c (i*16) :: !lines;
            first := false
          end else
            lines := sprintf "movk %s, #%d, lsl #%d" dst c (i*16) :: !lines
        end) chunks;
      String.concat "\n\t" (List.rev !lines)

  (* claude: maps arm64.ml's Post.arm64_cond names (arm64_eq/arm64_ne/.../
   * arm64_hi - the exact operator names Post.subflags/bc_guard build, see
   * arm64.ml lines ~190-230) to AArch64's real b.<cond> mnemonics. LO/LS/
   * HI/HS are AArch64's own spelling of the four unsigned comparisons
   * (arm.ml's ARM32 equivalent used LS/HI too, but under the CS/CC naming
   * AArch64 replaced with HS/LO for the carry-based pair - only LTU/GEU
   * map differently here, via arm64.ml's bc_guard swap to GTU/LEU exactly
   * as arm.ml's own does, so only LE u/GTu ever need a direct mnemonic). *)
  let arm64_bcond = function
      | "arm64_eq" -> "b.eq"
      | "arm64_ne" -> "b.ne"
      | "arm64_lt" -> "b.lt"
      | "arm64_le" -> "b.le"
      | "arm64_gt" -> "b.gt"
      | "arm64_ge" -> "b.ge"
      | "arm64_ls" -> "b.ls"
      | "arm64_hi" -> "b.hi"
      | op       -> error (sprintf "not an AArch64 condition: %s" op)


# 000 "/dev/stdout"


type
    (
        't38,
        't37,
        't36,
        't35,
        't34,
        't33,
        't32,
        't31,
        't30,
        't29,
        't28,
        't27,
        't26,
        't25,
        't24,
        't23,
        't22,
        't21,
        't20,
        't19,
        't18,
        't17,
        't16,
        't15,
        't14,
        't13,
        't12,
        't11,
        't10,
        't9,
        't8,
        't7,
        't6,
        't5,
        't4,
        't3,
        't2,
        't1,
        't0
    )
nonterm
=
    {
        _Subcc6: ( 't38 ) Camlburg.nt;
        _Subcc5: ( 't37 ) Camlburg.nt;
        _Sub9: ( 't36 ) Camlburg.nt;
        _Sub10: ( 't35 ) Camlburg.nt;
        _Store4: ( 't34 ) Camlburg.nt;
        _Store2: ( 't33 ) Camlburg.nt;
        _Quot16: ( 't32 ) Camlburg.nt;
        _Quot15: ( 't31 ) Camlburg.nt;
        _Mul14: ( 't30 ) Camlburg.nt;
        _Mul13: ( 't29 ) Camlburg.nt;
        _Goto3: ( 't28 ) Camlburg.nt;
        _Goto1: ( 't27 ) Camlburg.nt;
        _And12: ( 't26 ) Camlburg.nt;
        _And11: ( 't25 ) Camlburg.nt;
        _Add8: ( 't24 ) Camlburg.nt;
        _Add7: ( 't23 ) Camlburg.nt;
        wregl: ( 't22 ) Camlburg.nt;
        wreg32: ( 't21 ) Camlburg.nt;
        symbol: ( 't20 ) Camlburg.nt;
        spl: ( 't19 ) Camlburg.nt;
        sp: ( 't18 ) Camlburg.nt;
        regl: ( 't17 ) Camlburg.nt;
        reg: ( 't16 ) Camlburg.nt;
        ral: ( 't15 ) Camlburg.nt;
        ra: ( 't14 ) Camlburg.nt;
        r: ( 't13 ) Camlburg.nt;
        pcl: ( 't12 ) Camlburg.nt;
        pc: ( 't11 ) Camlburg.nt;
        next: ( 't10 ) Camlburg.nt;
        meml: ( 't9 ) Camlburg.nt;
        mem: ( 't8 ) Camlburg.nt;
        inst: ( 't7 ) Camlburg.nt;
        constv: ( 't6 ) Camlburg.nt;
        const: ( 't5 ) Camlburg.nt;
        cond: ( 't4 ) Camlburg.nt;
        ccval: ( 't3 ) Camlburg.nt;
        ccl: ( 't2 ) Camlburg.nt;
        any: ( 't1 ) Camlburg.nt;
        addr: ( 't0 ) Camlburg.nt
    }

let rec
inf =
    {addr = (Camlburg.infinity)
    ;any = (Camlburg.infinity)
    ;ccl = (Camlburg.infinity)
    ;ccval = (Camlburg.infinity)
    ;cond = (Camlburg.infinity)
    ;const = (Camlburg.infinity)
    ;constv = (Camlburg.infinity)
    ;inst = (Camlburg.infinity)
    ;mem = (Camlburg.infinity)
    ;meml = (Camlburg.infinity)
    ;next = (Camlburg.infinity)
    ;pc = (Camlburg.infinity)
    ;pcl = (Camlburg.infinity)
    ;r = (Camlburg.infinity)
    ;ra = (Camlburg.infinity)
    ;ral = (Camlburg.infinity)
    ;reg = (Camlburg.infinity)
    ;regl = (Camlburg.infinity)
    ;sp = (Camlburg.infinity)
    ;spl = (Camlburg.infinity)
    ;symbol = (Camlburg.infinity)
    ;wreg32 = (Camlburg.infinity)
    ;wregl = (Camlburg.infinity)
    ;_Add7 = (Camlburg.infinity)
    ;_Add8 = (Camlburg.infinity)
    ;_And11 = (Camlburg.infinity)
    ;_And12 = (Camlburg.infinity)
    ;_Goto1 = (Camlburg.infinity)
    ;_Goto3 = (Camlburg.infinity)
    ;_Mul13 = (Camlburg.infinity)
    ;_Mul14 = (Camlburg.infinity)
    ;_Quot15 = (Camlburg.infinity)
    ;_Quot16 = (Camlburg.infinity)
    ;_Store2 = (Camlburg.infinity)
    ;_Store4 = (Camlburg.infinity)
    ;_Sub10 = (Camlburg.infinity)
    ;_Sub9 = (Camlburg.infinity)
    ;_Subcc5 = (Camlburg.infinity)
    ;_Subcc6 = (Camlburg.infinity)
    }


let rec
update_addr =
    fun nt x ->
        if nt.Camlburg.cost >= x.addr.Camlburg.cost then
            x
        else
            { x with addr = (nt) }
and update_any =
    fun nt x ->
        if nt.Camlburg.cost >= x.any.Camlburg.cost then
            x
        else
            (fun x ->
                (update_inst
                    {Camlburg.cost = (nt.Camlburg.cost + 100)
                    ;Camlburg.action =
                        (fun () ->
                            let any = x.any.Camlburg.action ()
                            in
                                
# 344 "arch/arm64/arm64rec.mlb"
                                ( cat ["<";any;">"] )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with any = (nt) }
and update_ccl =
    fun nt x ->
        if nt.Camlburg.cost >= x.ccl.Camlburg.cost then
            x
        else
            { x with ccl = (nt) }
and update_ccval =
    fun nt x ->
        if nt.Camlburg.cost >= x.ccval.Camlburg.cost then
            x
        else
            { x with ccval = (nt) }
and update_cond =
    fun nt x ->
        if nt.Camlburg.cost >= x.cond.Camlburg.cost then
            x
        else
            { x with cond = (nt) }
and update_const =
    fun nt x ->
        if nt.Camlburg.cost >= x.const.Camlburg.cost then
            x
        else
            { x with const = (nt) }
and update_constv =
    fun nt x ->
        if nt.Camlburg.cost >= x.constv.Camlburg.cost then
            x
        else
            { x with constv = (nt) }
and update_inst =
    fun nt x ->
        if nt.Camlburg.cost >= x.inst.Camlburg.cost then
            x
        else
            { x with inst = (nt) }
and update_mem =
    fun nt x ->
        if nt.Camlburg.cost >= x.mem.Camlburg.cost then
            x
        else
            { x with mem = (nt) }
and update_meml =
    fun nt x ->
        if nt.Camlburg.cost >= x.meml.Camlburg.cost then
            x
        else
            { x with meml = (nt) }
and update_next =
    fun nt x ->
        if nt.Camlburg.cost >= x.next.Camlburg.cost then
            x
        else
            { x with next = (nt) }
and update_pc =
    fun nt x ->
        if nt.Camlburg.cost >= x.pc.Camlburg.cost then
            x
        else
            { x with pc = (nt) }
and update_pcl =
    fun nt x ->
        if nt.Camlburg.cost >= x.pcl.Camlburg.cost then
            x
        else
            { x with pcl = (nt) }
and update_r =
    fun nt x ->
        if nt.Camlburg.cost >= x.r.Camlburg.cost then
            x
        else
            (fun x ->
                (update_regl
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let r = x.r.Camlburg.action ()
                            in
                                
# 234 "arch/arm64/arm64rec.mlb"
                                ( reg r )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                ((fun x ->
                    (update_wregl
                        {Camlburg.cost = (nt.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let r = x.r.Camlburg.action ()
                                in
                                    
# 238 "arch/arm64/arm64rec.mlb"
                                    ( wreg r )
                                    
# 000 "/dev/stdout"
)
                        })
                        x)
                    { x with r = (nt) })
and update_ra =
    fun nt x ->
        if nt.Camlburg.cost >= x.ra.Camlburg.cost then
            x
        else
            { x with ra = (nt) }
and update_ral =
    fun nt x ->
        if nt.Camlburg.cost >= x.ral.Camlburg.cost then
            x
        else
            { x with ral = (nt) }
and update_reg =
    fun nt x ->
        if nt.Camlburg.cost >= x.reg.Camlburg.cost then
            x
        else
            (fun x ->
                (update_addr
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let reg = x.reg.Camlburg.action ()
                            in
                                
# 255 "arch/arm64/arm64rec.mlb"
                                ( cat ["["; reg; "]"] )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with reg = (nt) }
and update_regl =
    fun nt x ->
        if nt.Camlburg.cost >= x.regl.Camlburg.cost then
            x
        else
            { x with regl = (nt) }
and update_sp =
    fun nt x ->
        if nt.Camlburg.cost >= x.sp.Camlburg.cost then
            x
        else
            { x with sp = (nt) }
and update_spl =
    fun nt x ->
        if nt.Camlburg.cost >= x.spl.Camlburg.cost then
            x
        else
            { x with spl = (nt) }
and update_symbol =
    fun nt x ->
        if nt.Camlburg.cost >= x.symbol.Camlburg.cost then
            x
        else
            { x with symbol = (nt) }
and update_wreg32 =
    fun nt x ->
        if nt.Camlburg.cost >= x.wreg32.Camlburg.cost then
            x
        else
            { x with wreg32 = (nt) }
and update_wregl =
    fun nt x ->
        if nt.Camlburg.cost >= x.wregl.Camlburg.cost then
            x
        else
            { x with wregl = (nt) }
and update__Add7 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add7.Camlburg.cost then
            x
        else
            { x with _Add7 = (nt) }
and update__Add8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add8.Camlburg.cost then
            x
        else
            { x with _Add8 = (nt) }
and update__And11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And11.Camlburg.cost then
            x
        else
            { x with _And11 = (nt) }
and update__And12 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And12.Camlburg.cost then
            x
        else
            { x with _And12 = (nt) }
and update__Goto1 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto1.Camlburg.cost then
            x
        else
            { x with _Goto1 = (nt) }
and update__Goto3 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto3.Camlburg.cost then
            x
        else
            { x with _Goto3 = (nt) }
and update__Mul13 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul13.Camlburg.cost then
            x
        else
            { x with _Mul13 = (nt) }
and update__Mul14 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul14.Camlburg.cost then
            x
        else
            { x with _Mul14 = (nt) }
and update__Quot15 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Quot15.Camlburg.cost then
            x
        else
            { x with _Quot15 = (nt) }
and update__Quot16 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Quot16.Camlburg.cost then
            x
        else
            { x with _Quot16 = (nt) }
and update__Store2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store2.Camlburg.cost then
            x
        else
            { x with _Store2 = (nt) }
and update__Store4 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store4.Camlburg.cost then
            x
        else
            { x with _Store4 = (nt) }
and update__Sub10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub10.Camlburg.cost then
            x
        else
            { x with _Sub10 = (nt) }
and update__Sub9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub9.Camlburg.cost then
            x
        else
            { x with _Sub9 = (nt) }
and update__Subcc5 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc5.Camlburg.cost then
            x
        else
            { x with _Subcc5 = (nt) }
and update__Subcc6 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc6.Camlburg.cost then
            x
        else
            { x with _Subcc6 = (nt) }


let rec
conZx =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 362 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Zx(";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conTrue =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 346 "arch/arm64/arm64rec.mlb"
                    ( cat [ "True"  ] )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conSx =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 361 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Sx(";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conSubcc =
    fun arg1 arg2 ->
        (update__Subcc5
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Subcc6
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.constv.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.constv.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 359 "arch/arm64/arm64rec.mlb"
                                ( cat [ "Subcc(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conSub =
    fun arg1 arg2 ->
        (update__Sub10
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.constv.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.constv.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Sub9
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.reg.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 355 "arch/arm64/arm64rec.mlb"
                                ( cat [ "Sub(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conStore =
    fun arg1 arg2 arg3 ->
        (update__Store2
            {Camlburg.cost =
                (arg1.ral.Camlburg.cost + arg2.next.Camlburg.cost
                +
                (Camlburg.matches 64) arg3)
            ;Camlburg.action =
                (fun () ->
                    let ral = arg1.ral.Camlburg.action ()
                    and next = arg2.next.Camlburg.action ()
                    in
                        (ral ,next))
            })
            ((update__Store4
                {Camlburg.cost =
                    (arg1.spl.Camlburg.cost + arg2.reg.Camlburg.cost
                    +
                    (Camlburg.matches 64) arg3)
                ;Camlburg.action =
                    (fun () ->
                        let spl = arg1.spl.Camlburg.action ()
                        and nsp = arg2.reg.Camlburg.action ()
                        in
                            (spl ,nsp))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.any.Camlburg.action ()
                            and src = arg2.any.Camlburg.action ()
                            and w = arg3
                            in
                                
# 369 "arch/arm64/arm64rec.mlb"
                                ( cat [ "Store(";dst;",";src;",";string_of_int w;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_inst
                        (Camlburg.choice
                            [{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.symbol.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and
                                        symbol =
                                        arg2.symbol.Camlburg.action ()
                                    in
                                        
# 260 "arch/arm64/arm64rec.mlb"
                                        ( adrp_add regl symbol )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.constv.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and
                                        constv =
                                        arg2.constv.Camlburg.action ()
                                    in
                                        
# 263 "arch/arm64/arm64rec.mlb"
                                        ( li64 regl constv )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.mem.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and mem = arg2.mem.Camlburg.action ()
                                    in
                                        
# 266 "arch/arm64/arm64rec.mlb"
                                        ( cat ["ldr"; " "; regl; ", "; mem] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.meml.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let meml = arg1.meml.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    in
                                        
# 269 "arch/arm64/arm64rec.mlb"
                                        ( cat ["str"; " "; reg; ", "; meml] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.meml.Camlburg.cost
                                +
                                arg2.wreg32.Camlburg.cost
                                +
                                (Camlburg.matches 8) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let meml = arg1.meml.Camlburg.action ()
                                    and
                                        wreg32 =
                                        arg2.wreg32.Camlburg.action ()
                                    in
                                        
# 273 "arch/arm64/arm64rec.mlb"
                                        ( cat ["strb"; " "; wreg32; ", "; meml] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.meml.Camlburg.cost
                                +
                                arg2.wreg32.Camlburg.cost
                                +
                                (Camlburg.matches 16) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let meml = arg1.meml.Camlburg.action ()
                                    and
                                        wreg32 =
                                        arg2.wreg32.Camlburg.action ()
                                    in
                                        
# 275 "arch/arm64/arm64rec.mlb"
                                        ( cat ["strh"; " "; wreg32; ", "; meml] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.meml.Camlburg.cost
                                +
                                arg2.wreg32.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let meml = arg1.meml.Camlburg.action ()
                                    and
                                        wreg32 =
                                        arg2.wreg32.Camlburg.action ()
                                    in
                                        
# 277 "arch/arm64/arm64rec.mlb"
                                        ( cat ["str"; " "; wreg32; ", "; meml] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    in
                                        
# 280 "arch/arm64/arm64rec.mlb"
                                        ( cat ["mov"; " "; regl; ", "; reg] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.ccl.Camlburg.cost
                                +
                                arg2._Subcc5.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let ccl = arg1.ccl.Camlburg.action ()
                                    and _v1 = arg2._Subcc5.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 302 "arch/arm64/arm64rec.mlb"
                                            ( cat ["cmp"; " "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.ccl.Camlburg.cost
                                +
                                arg2._Subcc6.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let ccl = arg1.ccl.Camlburg.action ()
                                    and _v1 = arg2._Subcc6.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 304 "arch/arm64/arm64rec.mlb"
                                            ( sprintf "%s\n\tcmp %s, x16" (li64 "x16" y) x )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add7.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add7.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 312 "arch/arm64/arm64rec.mlb"
                                            ( cat ["add"; " "; dst; ", "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add8.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add8.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 315 "arch/arm64/arm64rec.mlb"
                                            ( sprintf "%s\n\tadd %s, %s, x16" (li64 "x16" y) dst x )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sub9.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sub9.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 318 "arch/arm64/arm64rec.mlb"
                                            ( cat ["sub"; " "; dst; ", "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sub10.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sub10.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 321 "arch/arm64/arm64rec.mlb"
                                            ( sprintf "%s\n\tsub %s, %s, x16" (li64 "x16" y) dst x )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._And11.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._And11.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 324 "arch/arm64/arm64rec.mlb"
                                            ( cat ["and"; " "; dst; ", "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._And12.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._And12.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 327 "arch/arm64/arm64rec.mlb"
                                            ( sprintf "%s\n\tand %s, %s, x16" (li64 "x16" y) dst x )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Mul13.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Mul13.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 331 "arch/arm64/arm64rec.mlb"
                                            ( cat ["mul"; " "; dst; ", "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Mul14.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Mul14.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 334 "arch/arm64/arm64rec.mlb"
                                            ( sprintf "%s\n\tmul %s, %s, x16" (li64 "x16" y) dst x )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Quot15.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Quot15.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 337 "arch/arm64/arm64rec.mlb"
                                            ( cat ["sdiv"; " "; dst; ", "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Quot16.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Quot16.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 340 "arch/arm64/arm64rec.mlb"
                                            ( sprintf "%s\n\tsdiv %s, %s, x16" (li64 "x16" y) dst x )
                                            
# 000 "/dev/stdout"
)
                            }]))
                        inf)))
and conReg =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let char = arg1
                    and n = arg2
                    in
                        
# 366 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Reg('";Char.escaped char;"',"; string_of_int n;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_ccl
                {Camlburg.cost =
                    ((Camlburg.matches 'c') arg1 + (Camlburg.matches 2) arg2)
                ;Camlburg.action =
                    (fun () ->
                        
# 242 "arch/arm64/arm64rec.mlb"
                        ( () )
                        
# 000 "/dev/stdout"
)
                })
                ((update_pcl
                    {Camlburg.cost =
                        ((Camlburg.matches 'c') arg1
                        +
                        (Camlburg.matches 0) arg2)
                    ;Camlburg.action =
                        (fun () ->
                            
# 241 "arch/arm64/arm64rec.mlb"
                            ( () )
                            
# 000 "/dev/stdout"
)
                    })
                    ((update_r
                        {Camlburg.cost = ((Camlburg.matches 'r') arg1)
                        ;Camlburg.action =
                            (fun () ->
                                let n = arg2
                                in
                                    
# 233 "arch/arm64/arm64rec.mlb"
                                    ( n )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_ral
                            {Camlburg.cost =
                                ((Camlburg.matches 'r') arg1
                                +
                                (Camlburg.matches 30) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    
# 244 "arch/arm64/arm64rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                            })
                            ((update_spl
                                {Camlburg.cost =
                                    ((Camlburg.matches 'r') arg1
                                    +
                                    (Camlburg.matches 31) arg2)
                                ;Camlburg.action =
                                    (fun () ->
                                        
# 243 "arch/arm64/arm64rec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                                })
                                inf)))))
and conQuot =
    fun arg1 arg2 ->
        (update__Quot15
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Quot16
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.constv.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.constv.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 358 "arch/arm64/arm64rec.mlb"
                                ( cat [ "Quot(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conPar =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let l = arg1.any.Camlburg.action ()
                    and r = arg2.any.Camlburg.action ()
                    in
                        
# 374 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Par(";l;",";r;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1._Goto1.Camlburg.cost
                        +
                        arg2._Store2.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto1.Camlburg.action ()
                            and _v2 = arg2._Store2.Camlburg.action ()
                            in
                                let (ral, next) = _v2
                                in
                                    let symbol = _v1
                                    in
                                        
# 291 "arch/arm64/arm64rec.mlb"
                                        ( cat ["bl"; " "; symbol] )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto3.Camlburg.cost
                        +
                        arg2._Store2.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto3.Camlburg.action ()
                            and _v2 = arg2._Store2.Camlburg.action ()
                            in
                                let (ral, next) = _v2
                                in
                                    let target = _v1
                                    in
                                        
# 294 "arch/arm64/arm64rec.mlb"
                                        ( cat ["blr"; " "; target] )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto3.Camlburg.cost
                        +
                        arg2._Store4.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto3.Camlburg.action ()
                            and _v2 = arg2._Store4.Camlburg.action ()
                            in
                                let (spl, nsp) = _v2
                                in
                                    let target = _v1
                                    in
                                        
# 298 "arch/arm64/arm64rec.mlb"
                                        ( cat ["mov sp, "; nsp; "\n\tbr "; target] )
                                        
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conNop =
    fun () ->
        (update_inst
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 342 "arch/arm64/arm64rec.mlb"
                    ( "nop" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update__Mul13
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Mul14
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.constv.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.constv.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 357 "arch/arm64/arm64rec.mlb"
                                ( cat [ "Mul(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conMem =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 365 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Mem(";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_meml
                {Camlburg.cost = (arg1.addr.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let addr = arg1.addr.Camlburg.action ()
                        in
                            
# 251 "arch/arm64/arm64rec.mlb"
                            ( addr )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conLobits =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 363 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Lobits(";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conLink =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1
                    and w = arg2
                    in
                        
# 348 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Link(";x#mangled_text;",";string_of_int w;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_symbol
                {Camlburg.cost = (0)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1
                        and w = arg2
                        in
                            
# 231 "arch/arm64/arm64rec.mlb"
                            ( x#mangled_text )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conLate =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let string = arg1
                    and w = arg2
                    in
                        
# 349 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Late(";string;",";string_of_int w;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conKill =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 370 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Kill(";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conGuarded =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let guard = arg1.any.Camlburg.action ()
                    and any = arg2.any.Camlburg.action ()
                    in
                        
# 373 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Guarded(";guard;",";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                {Camlburg.cost =
                    (arg1.cond.Camlburg.cost + arg2._Goto1.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let cond = arg1.cond.Camlburg.action ()
                        and _v1 = arg2._Goto1.Camlburg.action ()
                        in
                            let symbol = _v1
                            in
                                
# 309 "arch/arm64/arm64rec.mlb"
                                ( cat [arm64_bcond cond; " "; symbol] )
                                
# 000 "/dev/stdout"
)
                })
                inf)
and conGoto =
    fun arg1 ->
        (update__Goto1
            {Camlburg.cost = (arg1.symbol.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let symbol = arg1.symbol.Camlburg.action () in symbol)
            })
            ((update__Goto3
                {Camlburg.cost = (arg1.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let target = arg1.reg.Camlburg.action () in target)
                })
                ((update_any
                    {Camlburg.cost = (arg1.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let any = arg1.any.Camlburg.action ()
                            in
                                
# 375 "arch/arm64/arm64rec.mlb"
                                ( cat [ "Goto(";any;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_inst
                        (Camlburg.choice
                            [{Camlburg.cost = (arg1.symbol.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let
                                        symbol =
                                        arg1.symbol.Camlburg.action ()
                                    in
                                        
# 283 "arch/arm64/arm64rec.mlb"
                                        ( cat ["b"; " "; symbol] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost = (arg1.reg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let reg = arg1.reg.Camlburg.action ()
                                    in
                                        
# 286 "arch/arm64/arm64rec.mlb"
                                        ( cat ["br"; " "; reg] )
                                        
# 000 "/dev/stdout"
)
                            }]))
                        inf)))
and conFetch =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    and w = arg2
                    in
                        
# 352 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Fetch(";any;",";string_of_int w;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_ccval
                {Camlburg.cost =
                    (arg1.ccl.Camlburg.cost + (Camlburg.matches 64) arg2)
                ;Camlburg.action =
                    (fun () ->
                        let ccl = arg1.ccl.Camlburg.action ()
                        in
                            
# 247 "arch/arm64/arm64rec.mlb"
                            ( () )
                            
# 000 "/dev/stdout"
)
                })
                ((update_mem
                    {Camlburg.cost = (arg1.meml.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let meml = arg1.meml.Camlburg.action ()
                            and w = arg2
                            in
                                
# 252 "arch/arm64/arm64rec.mlb"
                                ( meml )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_pc
                        {Camlburg.cost =
                            (arg1.pcl.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg2)
                        ;Camlburg.action =
                            (fun () ->
                                let pcl = arg1.pcl.Camlburg.action ()
                                in
                                    
# 246 "arch/arm64/arm64rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_ra
                            {Camlburg.cost =
                                (arg1.ral.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    let ral = arg1.ral.Camlburg.action ()
                                    in
                                        
# 249 "arch/arm64/arm64rec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_reg
                                {Camlburg.cost = (arg1.regl.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            regl =
                                            arg1.regl.Camlburg.action ()
                                        and w = arg2
                                        in
                                            
# 235 "arch/arm64/arm64rec.mlb"
                                            ( regl )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_sp
                                    {Camlburg.cost =
                                        (arg1.spl.Camlburg.cost
                                        +
                                        (Camlburg.matches 64) arg2)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                spl =
                                                arg1.spl.Camlburg.action ()
                                            in
                                                
# 248 "arch/arm64/arm64rec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_wreg32
                                        {Camlburg.cost =
                                            (arg1.wregl.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    wregl =
                                                    arg1.wregl.Camlburg.action
                                                        ()
                                                and w = arg2
                                                in
                                                    
# 239 "arch/arm64/arm64rec.mlb"
                                                    ( wregl )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        inf)))))))
and conFalse =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 347 "arch/arm64/arm64rec.mlb"
                    ( cat [ "False" ] )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conCond =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let op = arg1
                    and any = arg2.any.Camlburg.action ()
                    in
                        
# 360 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Cond(";op;",";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_cond
                {Camlburg.cost = (arg2.ccval.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let op = arg1
                        and ccval = arg2.ccval.Camlburg.action ()
                        in
                            
# 307 "arch/arm64/arm64rec.mlb"
                            ( op )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conBits =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let bits = arg1
                    in
                        
# 350 "arch/arm64/arm64rec.mlb"
                        ( cat [ "Bits(b)" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost =
                    (let bits = arg1
                    in
                        
# 229 "arch/arm64/arm64rec.mlb"
                        ( guard (Bits.width bits = 64) )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let bits = arg1
                        in
                            
# 229 "arch/arm64/arm64rec.mlb"
                            ( const64 bits )
                            
# 000 "/dev/stdout"
)
                })
                ((update_constv
                    {Camlburg.cost =
                        (let bits = arg1
                        in
                            
# 230 "arch/arm64/arm64rec.mlb"
                            ( guard (Bits.width bits = 64) )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let bits = arg1
                            in
                                
# 230 "arch/arm64/arm64rec.mlb"
                                ( Bits.U.to_int64 bits )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAnd =
    fun arg1 arg2 ->
        (update__And11
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__And12
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.constv.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.constv.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 356 "arch/arm64/arm64rec.mlb"
                                ( cat [ "And(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAdd =
    fun arg1 arg2 ->
        (update__Add7
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Add8
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.constv.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.constv.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update_addr
                    (Camlburg.choice
                        [{Camlburg.cost =
                            (arg1.const.Camlburg.cost
                            +
                            arg2.reg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let const = arg1.const.Camlburg.action ()
                                and reg = arg2.reg.Camlburg.action ()
                                in
                                    
# 256 "arch/arm64/arm64rec.mlb"
                                    ( cat ["["; reg; ", #"; const; "]"] )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.reg.Camlburg.cost
                            +
                            arg2.const.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let reg = arg1.reg.Camlburg.action ()
                                and const = arg2.const.Camlburg.action ()
                                in
                                    
# 257 "arch/arm64/arm64rec.mlb"
                                    ( cat ["["; reg; ", #"; const; "]"] )
                                    
# 000 "/dev/stdout"
)
                        }]))
                    ((update_any
                        {Camlburg.cost =
                            (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let x = arg1.any.Camlburg.action ()
                                and y = arg2.any.Camlburg.action ()
                                in
                                    
# 354 "arch/arm64/arm64rec.mlb"
                                    ( cat [ "Add(";x;", ";y;")" ] )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_next
                            {Camlburg.cost =
                                (arg1.pc.Camlburg.cost
                                +
                                arg2.const.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let pc = arg1.pc.Camlburg.action ()
                                    and const = arg2.const.Camlburg.action ()
                                    in
                                        
# 289 "arch/arm64/arm64rec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            inf))))



# 144 "arch/arm64/arm64rec.mlb"

  let const = function
      | RP.Bool _                 -> error "boolean found"
      | RP.Link(s,_,w)            -> conLink s w
      | RP.Diff _                 -> error "PIC not supported"
      | RP.Bits(b)                -> conBits b
      | RP.Late(s,w)              -> error (sprintf "late constant %s found" s)

  let rec exp = function
      | RP.Const(k)               -> const k
      | RP.Fetch(l,w)             -> conFetch (loc l) w
      | RP.App(("add", [w]), [x; y])  -> conAdd (exp x) (exp y)
      | RP.App(("sub", [w]), [x; y])  -> conSub (exp x) (exp y)
      | RP.App(("and", [w]), [x; y])  -> conAnd (exp x) (exp y)
      | RP.App(("mul", [w]), [x; y])  -> conMul (exp x) (exp y)
      | RP.App(("quot", [w]), [x; y]) -> conQuot (exp x) (exp y)
      | RP.App(("lobits", [_;_]), [x]) -> exp x
      | RP.App((("arm64_subcc"), [w]), [x; y]) -> conSubcc (exp x) (exp y)
      | RP.App((("arm64_eq"|"arm64_ne"|"arm64_lt"|"arm64_le"|"arm64_gt"|"arm64_ge"
                |"arm64_ls"|"arm64_hi") as op, [w]), [c]) ->
          conCond op (exp c)
      | RP.App((o,_),_)           -> error (sprintf "unknown operator %s" o)

  and loc = function
      | RP.Reg((sp,_,_),i,w)      -> conReg sp i
      | RP.Mem(('m',_,_),w,e,ass) -> conMem (exp e)
      | RP.Mem((sp,_,_),_,_,_)    -> error (sprintf "mem-space space %c" sp)
      | RP.Var   (s,i,w)          -> error (sprintf "var %s found" s)
      | RP.Global(s,i,w)          -> error (sprintf "var %s found" s)
      | RP.Slice _                -> error "cannot handle slice"

  let effect = function
      | RP.Store(RP.Reg(('c',_,_),i,w),r,_)
        when i = SS.indices.SS.pc  -> conGoto (exp r)
      | RP.Store(l,e,w)           -> conStore (loc l) (exp e) w
      | RP.Kill(l)                -> error "cannot handle kill"

  let guarded g stmt = match g with
      | RP.Const(RP.Bool b)       -> if b then effect stmt else conNop ()
      | _                         -> conGuarded (exp g) (effect stmt)

  let rec geffects = function
      | []                        -> conNop ()
      | [g, s]                    -> guarded g s
      | (g, s) :: t               -> conPar (guarded g s) (geffects t)

  let rtl (RP.Rtl es) = geffects es

  let rtl_to_string = RU.ToString.rtl

  let dump msg rtl =
      List.iter prerr_string
      [ "error in recognizer: "
      ; msg
      ; " on "
      ; rtl_to_string rtl
      ; "\n"
      ]

  let to_string ?(mach=false) r =
      let _ = mach_syntax := mach in
      try
          let plan = rtl (Dn.rtl r) in
          Printf.sprintf "\t%s" (plan.inst.Camlburg.action ())
      with
          | Camlburg.Uncovered -> cat ["not an instruction: "
                                      ; rtl_to_string r
                                      ]
          | Error msg          -> ( dump msg r
                                  ; sprintf "error: %s" (rtl_to_string r)
                                  )

  let is_instruction r =
      try
          let plan = rtl (Dn.rtl r) in
          plan.inst.Camlburg.cost < 100
      with
          | Camlburg.Uncovered -> false
          | Error msg          -> ( dump msg r
                                  ; false
                                  )


# 000 "/dev/stdout"
