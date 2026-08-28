
# 1 "arch/amd64/amd64rec.mlb"

  (* claude: no upstream amd64rec.nw exists to port - see amd64.ml's header
   * comment for why this does NOT reuse arch/x86/x86rec.mlb's design
   * despite x86-64 being the same ISA family as 32-bit x86: x86rec.mlb is
   * shaped the way it is (its large "Withaflags"/"Withcarryflags"/
   * "Pairdestwithflags" pattern-match family) specifically to fuse a
   * result store with a PAIRED EFLAGS-register store into one real
   * instruction, because arch/x86/x86.ml's Post always emits both. This
   * backend's own amd64.ml Post never emits a paired flags store for
   * ordinary arithmetic (see its header comment for why that is still
   * correct) - so this grammar is modeled on arch/arm64/arm64rec.mlb's
   * much simpler shape instead (%head/%tail structure, one dedicated
   * comparison-sets-a-pseudo-location design), crossed with real x86-64
   * AT&T-syntax instruction text and a real (not link-register-based)
   * call/ret protocol.
   *
   * Only the subset actually needed by demos/hello_amd64.c-- is covered,
   * same "cover exactly what's needed, document the rest as a known gap"
   * discipline arm64rec.mlb's own header comment describes: base and
   * base+displacement addressing at the full 64-bit width only (no
   * sub-word loads/stores - amd64.ml declares T.memory = [64] only, same
   * reason arm64rec.mlb's own first pass did), add/sub/and/mul, the ten
   * eq/ne/lt/le/gt/ge/ltu/leu/gtu/geu comparisons, direct/indirect calls
   * via a real "call"/"ret" (not arm64's link-register bl/blr - see
   * amd64.ml's header comment), and unconditional/conditional branches.
   * No division (see amd64.ml's T.capabilities comment for why - it needs
   * the same rdx:rax "regpair" fusion x86rec.mlb's own Withundefflags/
   * RegPair machinery implements, not attempted here), no shifts, no
   * or/xor, no float.
   *
   * Immediate materialization is much simpler than arm64rec.mlb's li64:
   * x86-64 has a native one-instruction "movabsq $imm64, %reg" for any
   * full 64-bit immediate - no movz/movk multi-instruction dance needed
   * (that was specifically an AArch64/ARM limitation). Loading a global
   * symbol's ADDRESS (needed under macOS's PIE requirement) uses the
   * standard one-instruction "leaq symbol(%rip), %reg" RIP-relative idiom
   * - the x86-64 analogue of arm64rec.mlb's two-instruction adrp/add
   * @PAGE/@PAGEOFF dance, but simpler (no page-relative split needed,
   * leaq's displacement covers the full 64-bit RIP-relative range the
   * linker/loader can resolve).
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

  let const64 b =
      assert (Bits.width b = 64);
      Int64.to_string (Bits.S.to_int64 b)

  let cat     = String.concat ""
  let sprintf = Printf.sprintf

  let reg n   = "%" ^ Amd64regs.regname n

  (* claude: maps amd64.ml's Post.amd64_cond names (amd64_e/amd64_ne/.../
   * amd64_ae - the exact operator names Post.subflags/bc_guard build, see
   * amd64.ml's bc_guard/amd64_cond) to real x86-64 Jcc mnemonics. Unlike
   * arm64rec.mlb's arm64_bcond, no ltu/geu operand-swap trick is needed:
   * x86-64's b/be/a/ae mnemonics cover all four unsigned comparisons
   * directly (see amd64.ml's own bc_guard comment). *)
  let amd64_jcc = function
      | "amd64_e"  -> "je"
      | "amd64_ne" -> "jne"
      | "amd64_l"  -> "jl"
      | "amd64_le" -> "jle"
      | "amd64_g"  -> "jg"
      | "amd64_ge" -> "jge"
      | "amd64_b"  -> "jb"
      | "amd64_be" -> "jbe"
      | "amd64_a"  -> "ja"
      | "amd64_ae" -> "jae"
      | op       -> error (sprintf "not an x86-64 condition: %s" op)


# 000 "/dev/stdout"


type
    (
        't58,
        't57,
        't56,
        't55,
        't54,
        't53,
        't52,
        't51,
        't50,
        't49,
        't48,
        't47,
        't46,
        't45,
        't44,
        't43,
        't42,
        't41,
        't40,
        't39,
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
        _Subcc4: ( 't58 ) Camlburg.nt;
        _Subcc3: ( 't57 ) Camlburg.nt;
        _Sub9: ( 't56 ) Camlburg.nt;
        _Sub8: ( 't55 ) Camlburg.nt;
        _Sub18: ( 't54 ) Camlburg.nt;
        _Store2: ( 't53 ) Camlburg.nt;
        _Store17: ( 't52 ) Camlburg.nt;
        _Store16: ( 't51 ) Camlburg.nt;
        _Store15: ( 't50 ) Camlburg.nt;
        _Store14: ( 't49 ) Camlburg.nt;
        _Par22: ( 't48 ) Camlburg.nt;
        _Mul13: ( 't47 ) Camlburg.nt;
        _Mul12: ( 't46 ) Camlburg.nt;
        _Goto5: ( 't45 ) Camlburg.nt;
        _Goto20: ( 't44 ) Camlburg.nt;
        _Goto1: ( 't43 ) Camlburg.nt;
        _Fetch21: ( 't42 ) Camlburg.nt;
        _And11: ( 't41 ) Camlburg.nt;
        _And10: ( 't40 ) Camlburg.nt;
        _Add7: ( 't39 ) Camlburg.nt;
        _Add6: ( 't38 ) Camlburg.nt;
        _Add19: ( 't37 ) Camlburg.nt;
        wregl8: ( 't36 ) Camlburg.nt;
        wregl32: ( 't35 ) Camlburg.nt;
        wregl16: ( 't34 ) Camlburg.nt;
        wreg8: ( 't33 ) Camlburg.nt;
        wreg32: ( 't32 ) Camlburg.nt;
        wreg16: ( 't31 ) Camlburg.nt;
        symbol: ( 't30 ) Camlburg.nt;
        storeretaddr: ( 't29 ) Camlburg.nt;
        stacktop: ( 't28 ) Camlburg.nt;
        stacknext: ( 't27 ) Camlburg.nt;
        rspv: ( 't26 ) Camlburg.nt;
        rspl: ( 't25 ) Camlburg.nt;
        rem3val: ( 't24 ) Camlburg.nt;
        regl: ( 't23 ) Camlburg.nt;
        reg: ( 't22 ) Camlburg.nt;
        rdxl: ( 't21 ) Camlburg.nt;
        raxl: ( 't20 ) Camlburg.nt;
        r: ( 't19 ) Camlburg.nt;
        quot3val: ( 't18 ) Camlburg.nt;
        push: ( 't17 ) Camlburg.nt;
        pop: ( 't16 ) Camlburg.nt;
        pcv: ( 't15 ) Camlburg.nt;
        pcl: ( 't14 ) Camlburg.nt;
        modu3val: ( 't13 ) Camlburg.nt;
        meml: ( 't12 ) Camlburg.nt;
        mem: ( 't11 ) Camlburg.nt;
        inst: ( 't10 ) Camlburg.nt;
        eightc: ( 't9 ) Camlburg.nt;
        divu3val: ( 't8 ) Camlburg.nt;
        cqtoval: ( 't7 ) Camlburg.nt;
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
    ;cqtoval = (Camlburg.infinity)
    ;divu3val = (Camlburg.infinity)
    ;eightc = (Camlburg.infinity)
    ;inst = (Camlburg.infinity)
    ;mem = (Camlburg.infinity)
    ;meml = (Camlburg.infinity)
    ;modu3val = (Camlburg.infinity)
    ;pcl = (Camlburg.infinity)
    ;pcv = (Camlburg.infinity)
    ;pop = (Camlburg.infinity)
    ;push = (Camlburg.infinity)
    ;quot3val = (Camlburg.infinity)
    ;r = (Camlburg.infinity)
    ;raxl = (Camlburg.infinity)
    ;rdxl = (Camlburg.infinity)
    ;reg = (Camlburg.infinity)
    ;regl = (Camlburg.infinity)
    ;rem3val = (Camlburg.infinity)
    ;rspl = (Camlburg.infinity)
    ;rspv = (Camlburg.infinity)
    ;stacknext = (Camlburg.infinity)
    ;stacktop = (Camlburg.infinity)
    ;storeretaddr = (Camlburg.infinity)
    ;symbol = (Camlburg.infinity)
    ;wreg16 = (Camlburg.infinity)
    ;wreg32 = (Camlburg.infinity)
    ;wreg8 = (Camlburg.infinity)
    ;wregl16 = (Camlburg.infinity)
    ;wregl32 = (Camlburg.infinity)
    ;wregl8 = (Camlburg.infinity)
    ;_Add19 = (Camlburg.infinity)
    ;_Add6 = (Camlburg.infinity)
    ;_Add7 = (Camlburg.infinity)
    ;_And10 = (Camlburg.infinity)
    ;_And11 = (Camlburg.infinity)
    ;_Fetch21 = (Camlburg.infinity)
    ;_Goto1 = (Camlburg.infinity)
    ;_Goto20 = (Camlburg.infinity)
    ;_Goto5 = (Camlburg.infinity)
    ;_Mul12 = (Camlburg.infinity)
    ;_Mul13 = (Camlburg.infinity)
    ;_Par22 = (Camlburg.infinity)
    ;_Store14 = (Camlburg.infinity)
    ;_Store15 = (Camlburg.infinity)
    ;_Store16 = (Camlburg.infinity)
    ;_Store17 = (Camlburg.infinity)
    ;_Store2 = (Camlburg.infinity)
    ;_Sub18 = (Camlburg.infinity)
    ;_Sub8 = (Camlburg.infinity)
    ;_Sub9 = (Camlburg.infinity)
    ;_Subcc3 = (Camlburg.infinity)
    ;_Subcc4 = (Camlburg.infinity)
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
                                
# 311 "arch/amd64/amd64rec.mlb"
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
and update_cqtoval =
    fun nt x ->
        if nt.Camlburg.cost >= x.cqtoval.Camlburg.cost then
            x
        else
            { x with cqtoval = (nt) }
and update_divu3val =
    fun nt x ->
        if nt.Camlburg.cost >= x.divu3val.Camlburg.cost then
            x
        else
            { x with divu3val = (nt) }
and update_eightc =
    fun nt x ->
        if nt.Camlburg.cost >= x.eightc.Camlburg.cost then
            x
        else
            { x with eightc = (nt) }
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
and update_modu3val =
    fun nt x ->
        if nt.Camlburg.cost >= x.modu3val.Camlburg.cost then
            x
        else
            { x with modu3val = (nt) }
and update_pcl =
    fun nt x ->
        if nt.Camlburg.cost >= x.pcl.Camlburg.cost then
            x
        else
            { x with pcl = (nt) }
and update_pcv =
    fun nt x ->
        if nt.Camlburg.cost >= x.pcv.Camlburg.cost then
            x
        else
            { x with pcv = (nt) }
and update_pop =
    fun nt x ->
        if nt.Camlburg.cost >= x.pop.Camlburg.cost then
            x
        else
            { x with pop = (nt) }
and update_push =
    fun nt x ->
        if nt.Camlburg.cost >= x.push.Camlburg.cost then
            x
        else
            { x with push = (nt) }
and update_quot3val =
    fun nt x ->
        if nt.Camlburg.cost >= x.quot3val.Camlburg.cost then
            x
        else
            { x with quot3val = (nt) }
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
                                
# 176 "arch/amd64/amd64rec.mlb"
                                ( reg r )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                ((fun x ->
                    (update_wregl8
                        {Camlburg.cost = (nt.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let r = x.r.Camlburg.action ()
                                in
                                    
# 212 "arch/amd64/amd64rec.mlb"
                                    ( "%" ^ Amd64regs.regname8 r )
                                    
# 000 "/dev/stdout"
)
                        })
                        x)
                    ((fun x ->
                        (update_wregl16
                            {Camlburg.cost = (nt.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let r = x.r.Camlburg.action ()
                                    in
                                        
# 214 "arch/amd64/amd64rec.mlb"
                                        ( "%" ^ Amd64regs.regname16 r )
                                        
# 000 "/dev/stdout"
)
                            })
                            x)
                        ((fun x ->
                            (update_wregl32
                                {Camlburg.cost = (nt.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let r = x.r.Camlburg.action ()
                                        in
                                            
# 216 "arch/amd64/amd64rec.mlb"
                                            ( "%" ^ Amd64regs.regname32 r )
                                            
# 000 "/dev/stdout"
)
                                })
                                x)
                            { x with r = (nt) })))
and update_raxl =
    fun nt x ->
        if nt.Camlburg.cost >= x.raxl.Camlburg.cost then
            x
        else
            { x with raxl = (nt) }
and update_rdxl =
    fun nt x ->
        if nt.Camlburg.cost >= x.rdxl.Camlburg.cost then
            x
        else
            { x with rdxl = (nt) }
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
                                
# 191 "arch/amd64/amd64rec.mlb"
                                ( cat ["("; reg; ")"] )
                                
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
and update_rem3val =
    fun nt x ->
        if nt.Camlburg.cost >= x.rem3val.Camlburg.cost then
            x
        else
            { x with rem3val = (nt) }
and update_rspl =
    fun nt x ->
        if nt.Camlburg.cost >= x.rspl.Camlburg.cost then
            x
        else
            { x with rspl = (nt) }
and update_rspv =
    fun nt x ->
        if nt.Camlburg.cost >= x.rspv.Camlburg.cost then
            x
        else
            { x with rspv = (nt) }
and update_stacknext =
    fun nt x ->
        if nt.Camlburg.cost >= x.stacknext.Camlburg.cost then
            x
        else
            { x with stacknext = (nt) }
and update_stacktop =
    fun nt x ->
        if nt.Camlburg.cost >= x.stacktop.Camlburg.cost then
            x
        else
            { x with stacktop = (nt) }
and update_storeretaddr =
    fun nt x ->
        if nt.Camlburg.cost >= x.storeretaddr.Camlburg.cost then
            x
        else
            { x with storeretaddr = (nt) }
and update_symbol =
    fun nt x ->
        if nt.Camlburg.cost >= x.symbol.Camlburg.cost then
            x
        else
            { x with symbol = (nt) }
and update_wreg16 =
    fun nt x ->
        if nt.Camlburg.cost >= x.wreg16.Camlburg.cost then
            x
        else
            { x with wreg16 = (nt) }
and update_wreg32 =
    fun nt x ->
        if nt.Camlburg.cost >= x.wreg32.Camlburg.cost then
            x
        else
            { x with wreg32 = (nt) }
and update_wreg8 =
    fun nt x ->
        if nt.Camlburg.cost >= x.wreg8.Camlburg.cost then
            x
        else
            { x with wreg8 = (nt) }
and update_wregl16 =
    fun nt x ->
        if nt.Camlburg.cost >= x.wregl16.Camlburg.cost then
            x
        else
            { x with wregl16 = (nt) }
and update_wregl32 =
    fun nt x ->
        if nt.Camlburg.cost >= x.wregl32.Camlburg.cost then
            x
        else
            { x with wregl32 = (nt) }
and update_wregl8 =
    fun nt x ->
        if nt.Camlburg.cost >= x.wregl8.Camlburg.cost then
            x
        else
            { x with wregl8 = (nt) }
and update__Add19 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add19.Camlburg.cost then
            x
        else
            { x with _Add19 = (nt) }
and update__Add6 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add6.Camlburg.cost then
            x
        else
            { x with _Add6 = (nt) }
and update__Add7 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add7.Camlburg.cost then
            x
        else
            { x with _Add7 = (nt) }
and update__And10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And10.Camlburg.cost then
            x
        else
            { x with _And10 = (nt) }
and update__And11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And11.Camlburg.cost then
            x
        else
            { x with _And11 = (nt) }
and update__Fetch21 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch21.Camlburg.cost then
            x
        else
            { x with _Fetch21 = (nt) }
and update__Goto1 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto1.Camlburg.cost then
            x
        else
            { x with _Goto1 = (nt) }
and update__Goto20 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto20.Camlburg.cost then
            x
        else
            { x with _Goto20 = (nt) }
and update__Goto5 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto5.Camlburg.cost then
            x
        else
            { x with _Goto5 = (nt) }
and update__Mul12 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul12.Camlburg.cost then
            x
        else
            { x with _Mul12 = (nt) }
and update__Mul13 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul13.Camlburg.cost then
            x
        else
            { x with _Mul13 = (nt) }
and update__Par22 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Par22.Camlburg.cost then
            x
        else
            { x with _Par22 = (nt) }
and update__Store14 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store14.Camlburg.cost then
            x
        else
            { x with _Store14 = (nt) }
and update__Store15 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store15.Camlburg.cost then
            x
        else
            { x with _Store15 = (nt) }
and update__Store16 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store16.Camlburg.cost then
            x
        else
            { x with _Store16 = (nt) }
and update__Store17 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store17.Camlburg.cost then
            x
        else
            { x with _Store17 = (nt) }
and update__Store2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store2.Camlburg.cost then
            x
        else
            { x with _Store2 = (nt) }
and update__Sub18 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub18.Camlburg.cost then
            x
        else
            { x with _Sub18 = (nt) }
and update__Sub8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub8.Camlburg.cost then
            x
        else
            { x with _Sub8 = (nt) }
and update__Sub9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub9.Camlburg.cost then
            x
        else
            { x with _Sub9 = (nt) }
and update__Subcc3 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc3.Camlburg.cost then
            x
        else
            { x with _Subcc3 = (nt) }
and update__Subcc4 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc4.Camlburg.cost then
            x
        else
            { x with _Subcc4 = (nt) }


let rec
conZx =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 328 "arch/amd64/amd64rec.mlb"
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
                    
# 313 "arch/amd64/amd64rec.mlb"
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
                        
# 327 "arch/amd64/amd64rec.mlb"
                        ( cat [ "Sx(";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conSubcc =
    fun arg1 arg2 ->
        (update__Subcc3
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Subcc4
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
                                
# 325 "arch/amd64/amd64rec.mlb"
                                ( cat [ "Subcc(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conSub =
    fun arg1 arg2 ->
        (update__Sub18
            {Camlburg.cost =
                (arg1.rspv.Camlburg.cost + arg2.eightc.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let rspv = arg1.rspv.Camlburg.action ()
                    and eightc = arg2.eightc.Camlburg.action ()
                    in
                        (rspv ,eightc))
            })
            ((update__Sub8
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.reg.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update__Sub9
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
                                    
# 322 "arch/amd64/amd64rec.mlb"
                                    ( cat [ "Sub(";x;", ";y;")" ] )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conStore =
    fun arg1 arg2 arg3 ->
        (update__Store14
            {Camlburg.cost =
                (arg1.raxl.Camlburg.cost + arg2.quot3val.Camlburg.cost
                +
                (Camlburg.matches 64) arg3)
            ;Camlburg.action =
                (fun () ->
                    let raxl = arg1.raxl.Camlburg.action ()
                    and quot3val = arg2.quot3val.Camlburg.action ()
                    in
                        (raxl ,quot3val))
            })
            ((update__Store15
                {Camlburg.cost =
                    (arg1.rdxl.Camlburg.cost + arg2.rem3val.Camlburg.cost
                    +
                    (Camlburg.matches 64) arg3)
                ;Camlburg.action =
                    (fun () ->
                        let rdxl = arg1.rdxl.Camlburg.action ()
                        and rem3val = arg2.rem3val.Camlburg.action ()
                        in
                            (rdxl ,rem3val))
                })
                ((update__Store16
                    {Camlburg.cost =
                        (arg1.raxl.Camlburg.cost
                        +
                        arg2.divu3val.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let raxl = arg1.raxl.Camlburg.action ()
                            and divu3val = arg2.divu3val.Camlburg.action ()
                            in
                                (raxl ,divu3val))
                    })
                    ((update__Store17
                        {Camlburg.cost =
                            (arg1.rdxl.Camlburg.cost
                            +
                            arg2.modu3val.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let rdxl = arg1.rdxl.Camlburg.action ()
                                and
                                    modu3val =
                                    arg2.modu3val.Camlburg.action ()
                                in
                                    (rdxl ,modu3val))
                        })
                        ((update__Store2
                            {Camlburg.cost =
                                (arg1.rspl.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let rspl = arg1.rspl.Camlburg.action ()
                                    and nsp = arg2.reg.Camlburg.action ()
                                    in
                                        (rspl ,nsp))
                            })
                            ((update_any
                                {Camlburg.cost =
                                    (arg1.any.Camlburg.cost
                                    +
                                    arg2.any.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let dst = arg1.any.Camlburg.action ()
                                        and src = arg2.any.Camlburg.action ()
                                        and w = arg3
                                        in
                                            
# 335 "arch/amd64/amd64rec.mlb"
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
                                                let
                                                    regl =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    symbol =
                                                    arg2.symbol.Camlburg.action
                                                        ()
                                                in
                                                    
# 197 "arch/amd64/amd64rec.mlb"
                                                    ( sprintf "leaq %s(%%rip), %s" symbol regl )
                                                    
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
                                                let
                                                    regl =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    constv =
                                                    arg2.constv.Camlburg.action
                                                        ()
                                                in
                                                    
# 200 "arch/amd64/amd64rec.mlb"
                                                    ( sprintf "movabsq $%Ld, %s" constv regl )
                                                    
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
                                                let
                                                    regl =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    mem =
                                                    arg2.mem.Camlburg.action
                                                        ()
                                                in
                                                    
# 203 "arch/amd64/amd64rec.mlb"
                                                    ( cat ["movq"; " "; mem; ", "; regl] )
                                                    
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
                                                let
                                                    meml =
                                                    arg1.meml.Camlburg.action
                                                        ()
                                                and
                                                    reg =
                                                    arg2.reg.Camlburg.action
                                                        ()
                                                in
                                                    
# 206 "arch/amd64/amd64rec.mlb"
                                                    ( cat ["movq"; " "; reg; ", "; meml] )
                                                    
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
                                                let
                                                    regl =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    reg =
                                                    arg2.reg.Camlburg.action
                                                        ()
                                                in
                                                    
# 209 "arch/amd64/amd64rec.mlb"
                                                    ( cat ["movq"; " "; reg; ", "; regl] )
                                                    
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.meml.Camlburg.cost
                                            +
                                            arg2.wreg8.Camlburg.cost
                                            +
                                            (Camlburg.matches 8) arg3)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    meml =
                                                    arg1.meml.Camlburg.action
                                                        ()
                                                and
                                                    wreg8 =
                                                    arg2.wreg8.Camlburg.action
                                                        ()
                                                in
                                                    
# 220 "arch/amd64/amd64rec.mlb"
                                                    ( cat ["movb"; " "; wreg8; ", "; meml] )
                                                    
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.meml.Camlburg.cost
                                            +
                                            arg2.wreg16.Camlburg.cost
                                            +
                                            (Camlburg.matches 16) arg3)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    meml =
                                                    arg1.meml.Camlburg.action
                                                        ()
                                                and
                                                    wreg16 =
                                                    arg2.wreg16.Camlburg.action
                                                        ()
                                                in
                                                    
# 222 "arch/amd64/amd64rec.mlb"
                                                    ( cat ["movw"; " "; wreg16; ", "; meml] )
                                                    
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
                                                let
                                                    meml =
                                                    arg1.meml.Camlburg.action
                                                        ()
                                                and
                                                    wreg32 =
                                                    arg2.wreg32.Camlburg.action
                                                        ()
                                                in
                                                    
# 224 "arch/amd64/amd64rec.mlb"
                                                    ( cat ["movl"; " "; wreg32; ", "; meml] )
                                                    
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.ccl.Camlburg.cost
                                            +
                                            arg2._Subcc3.Camlburg.cost
                                            +
                                            (Camlburg.matches 64) arg3)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    ccl =
                                                    arg1.ccl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Subcc3.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 238 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "cmpq %s, %s" y x )
                                                        
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.ccl.Camlburg.cost
                                            +
                                            arg2._Subcc4.Camlburg.cost
                                            +
                                            (Camlburg.matches 64) arg3)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    ccl =
                                                    arg1.ccl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Subcc4.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 240 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "cmpq $%Ld, %s" y x )
                                                        
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.regl.Camlburg.cost
                                            +
                                            arg2._Add6.Camlburg.cost
                                            +
                                            (Camlburg.matches 64) arg3)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    dst =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Add6.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 248 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "movq %s, %s\n\taddq %s, %s" x dst y dst )
                                                        
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
                                                let
                                                    dst =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Add7.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 251 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "movq %s, %s\n\taddq $%Ld, %s" x dst y dst )
                                                        
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.regl.Camlburg.cost
                                            +
                                            arg2._Sub8.Camlburg.cost
                                            +
                                            (Camlburg.matches 64) arg3)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    dst =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Sub8.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 254 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "movq %s, %s\n\tsubq %s, %s" x dst y dst )
                                                        
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
                                                let
                                                    dst =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Sub9.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 257 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "movq %s, %s\n\tsubq $%Ld, %s" x dst y dst )
                                                        
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.regl.Camlburg.cost
                                            +
                                            arg2._And10.Camlburg.cost
                                            +
                                            (Camlburg.matches 64) arg3)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    dst =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._And10.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 260 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "movq %s, %s\n\tandq %s, %s" x dst y dst )
                                                        
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
                                                let
                                                    dst =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._And11.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 263 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "movq %s, %s\n\tandq $%Ld, %s" x dst y dst )
                                                        
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.regl.Camlburg.cost
                                            +
                                            arg2._Mul12.Camlburg.cost
                                            +
                                            (Camlburg.matches 64) arg3)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    dst =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Mul12.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 267 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "movq %s, %s\n\timulq %s, %s" x dst y dst )
                                                        
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
                                                let
                                                    dst =
                                                    arg1.regl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Mul13.Camlburg.action
                                                        ()
                                                in
                                                    let (x, y) = _v1
                                                    in
                                                        
# 270 "arch/amd64/amd64rec.mlb"
                                                        ( sprintf "imulq $%Ld, %s, %s" y x dst )
                                                        
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.rdxl.Camlburg.cost
                                            +
                                            arg2.cqtoval.Camlburg.cost
                                            +
                                            (Camlburg.matches 64) arg3)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    rdxl =
                                                    arg1.rdxl.Camlburg.action
                                                        ()
                                                and
                                                    cqtoval =
                                                    arg2.cqtoval.Camlburg.action
                                                        ()
                                                in
                                                    
# 278 "arch/amd64/amd64rec.mlb"
                                                    ( "cqto" )
                                                    
# 000 "/dev/stdout"
)
                                        }]))
                                    ((update_pop
                                        {Camlburg.cost =
                                            (arg1.rspl.Camlburg.cost
                                            +
                                            arg2._Add19.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    rspl =
                                                    arg1.rspl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Add19.Camlburg.action
                                                        ()
                                                and w = arg3
                                                in
                                                    let (rspv, eightc) = _v1
                                                    in
                                                        
# 297 "arch/amd64/amd64rec.mlb"
                                                        ( () )
                                                        
# 000 "/dev/stdout"
)
                                        })
                                        ((update_push
                                            {Camlburg.cost =
                                                (arg1.rspl.Camlburg.cost
                                                +
                                                arg2._Sub18.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        rspl =
                                                        arg1.rspl.Camlburg.action
                                                            ()
                                                    and
                                                        _v1 =
                                                        arg2._Sub18.Camlburg.action
                                                            ()
                                                    and w = arg3
                                                    in
                                                        let
                                                            (rspv, eightc) =
                                                            _v1
                                                        in
                                                            
# 296 "arch/amd64/amd64rec.mlb"
                                                            ( () )
                                                            
# 000 "/dev/stdout"
)
                                            })
                                            ((update_storeretaddr
                                                {Camlburg.cost =
                                                    (arg1.stacknext.Camlburg.cost
                                                    +
                                                    arg2.pcv.Camlburg.cost)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        let
                                                            stacknext =
                                                            arg1.stacknext.Camlburg.action
                                                                ()
                                                        and
                                                            pcv =
                                                            arg2.pcv.Camlburg.action
                                                                ()
                                                        and w = arg3
                                                        in
                                                            
# 300 "arch/amd64/amd64rec.mlb"
                                                            ( () )
                                                            
# 000 "/dev/stdout"
)
                                                })
                                                inf)))))))))
and conRem3 =
    fun arg1 arg2 arg3 ->
        (update_rem3val
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost
                +
                arg3.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let hi = arg1.reg.Camlburg.action ()
                    and lo = arg2.reg.Camlburg.action ()
                    and y = arg3.reg.Camlburg.action ()
                    in
                        
# 281 "arch/amd64/amd64rec.mlb"
                        ( y )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conReg =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let char = arg1
                    and n = arg2
                    in
                        
# 332 "arch/amd64/amd64rec.mlb"
                        ( cat [ "Reg('";Char.escaped char;"',"; string_of_int n;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_ccl
                {Camlburg.cost =
                    ((Camlburg.matches 'c') arg1 + (Camlburg.matches 2) arg2)
                ;Camlburg.action =
                    (fun () ->
                        
# 180 "arch/amd64/amd64rec.mlb"
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
                            
# 179 "arch/amd64/amd64rec.mlb"
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
                                    
# 175 "arch/amd64/amd64rec.mlb"
                                    ( n )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_raxl
                            {Camlburg.cost =
                                ((Camlburg.matches 'r') arg1
                                +
                                (Camlburg.matches 0) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    
# 273 "arch/amd64/amd64rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                            })
                            ((update_rdxl
                                {Camlburg.cost =
                                    ((Camlburg.matches 'r') arg1
                                    +
                                    (Camlburg.matches 2) arg2)
                                ;Camlburg.action =
                                    (fun () ->
                                        
# 274 "arch/amd64/amd64rec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                                })
                                ((update_rspl
                                    {Camlburg.cost =
                                        ((Camlburg.matches 'r') arg1
                                        +
                                        (Camlburg.matches 4) arg2)
                                    ;Camlburg.action =
                                        (fun () ->
                                            
# 181 "arch/amd64/amd64rec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                    })
                                    inf))))))
and conQuot3 =
    fun arg1 arg2 arg3 ->
        (update_quot3val
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost
                +
                arg3.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let hi = arg1.reg.Camlburg.action ()
                    and lo = arg2.reg.Camlburg.action ()
                    and y = arg3.reg.Camlburg.action ()
                    in
                        
# 280 "arch/amd64/amd64rec.mlb"
                        ( y )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conPar =
    fun arg1 arg2 ->
        (update__Par22
            {Camlburg.cost =
                (arg1.storeretaddr.Camlburg.cost + arg2.push.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let storeretaddr = arg1.storeretaddr.Camlburg.action ()
                    and push = arg2.push.Camlburg.action ()
                    in
                        (storeretaddr ,push))
            })
            ((update_any
                {Camlburg.cost =
                    (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let l = arg1.any.Camlburg.action ()
                        and r = arg2.any.Camlburg.action ()
                        in
                            
# 340 "arch/amd64/amd64rec.mlb"
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
                                    let (rspl, nsp) = _v2
                                    in
                                        let target = _v1
                                        in
                                            
# 234 "arch/amd64/amd64rec.mlb"
                                            ( sprintf "movq %s, %%rsp\n\tjmp *%s" nsp target )
                                            
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Store14.Camlburg.cost
                            +
                            arg2._Store15.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Store14.Camlburg.action ()
                                and _v2 = arg2._Store15.Camlburg.action ()
                                in
                                    let (rdxl, rem3val) = _v2
                                    in
                                        let (raxl, quot3val) = _v1
                                        in
                                            
# 283 "arch/amd64/amd64rec.mlb"
                                            ( sprintf "idivq %s" quot3val )
                                            
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Store16.Camlburg.cost
                            +
                            arg2._Store17.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Store16.Camlburg.action ()
                                and _v2 = arg2._Store17.Camlburg.action ()
                                in
                                    let (rdxl, modu3val) = _v2
                                    in
                                        let (raxl, divu3val) = _v1
                                        in
                                            
# 288 "arch/amd64/amd64rec.mlb"
                                            ( sprintf "divq %s" divu3val )
                                            
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Goto20.Camlburg.cost
                            +
                            arg2.pop.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto20.Camlburg.action ()
                                and pop = arg2.pop.Camlburg.action ()
                                in
                                    let (stacktop, w) = _v1
                                    in
                                        
# 303 "arch/amd64/amd64rec.mlb"
                                        ( "ret" )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Goto1.Camlburg.cost
                            +
                            arg2._Par22.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto1.Camlburg.action ()
                                and _v2 = arg2._Par22.Camlburg.action ()
                                in
                                    let (storeretaddr, push) = _v2
                                    in
                                        let target = _v1
                                        in
                                            
# 306 "arch/amd64/amd64rec.mlb"
                                            ( cat ["call"; " *"; target] )
                                            
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Goto5.Camlburg.cost
                            +
                            arg2._Par22.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto5.Camlburg.action ()
                                and _v2 = arg2._Par22.Camlburg.action ()
                                in
                                    let (storeretaddr, push) = _v2
                                    in
                                        let target = _v1
                                        in
                                            
# 309 "arch/amd64/amd64rec.mlb"
                                            ( cat ["call"; " "; target] )
                                            
# 000 "/dev/stdout"
)
                        }]))
                    inf))
and conNop =
    fun () ->
        (update_inst
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 290 "arch/amd64/amd64rec.mlb"
                    ( "nop" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update__Mul12
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Mul13
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
                                
# 324 "arch/amd64/amd64rec.mlb"
                                ( cat [ "Mul(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conModu3 =
    fun arg1 arg2 arg3 ->
        (update_modu3val
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost
                +
                arg3.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let hi = arg1.reg.Camlburg.action ()
                    and lo = arg2.reg.Camlburg.action ()
                    and y = arg3.reg.Camlburg.action ()
                    in
                        
# 286 "arch/amd64/amd64rec.mlb"
                        ( y )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conMem =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 331 "arch/amd64/amd64rec.mlb"
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
                            
# 187 "arch/amd64/amd64rec.mlb"
                            ( addr )
                            
# 000 "/dev/stdout"
)
                })
                ((update_stacknext
                    {Camlburg.cost = (arg1._Sub18.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sub18.Camlburg.action ()
                            in
                                let (rspv, eightc) = _v1
                                in
                                    
# 299 "arch/amd64/amd64rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                    })
                    ((update_stacktop
                        {Camlburg.cost = (arg1.rspv.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let rspv = arg1.rspv.Camlburg.action ()
                                in
                                    
# 298 "arch/amd64/amd64rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conLobits =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 329 "arch/amd64/amd64rec.mlb"
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
                        
# 315 "arch/amd64/amd64rec.mlb"
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
                            
# 173 "arch/amd64/amd64rec.mlb"
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
                        
# 316 "arch/amd64/amd64rec.mlb"
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
                        
# 336 "arch/amd64/amd64rec.mlb"
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
                        
# 339 "arch/amd64/amd64rec.mlb"
                        ( cat [ "Guarded(";guard;",";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                {Camlburg.cost =
                    (arg1.cond.Camlburg.cost + arg2._Goto5.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let cond = arg1.cond.Camlburg.action ()
                        and _v1 = arg2._Goto5.Camlburg.action ()
                        in
                            let symbol = _v1
                            in
                                
# 245 "arch/amd64/amd64rec.mlb"
                                ( cat [amd64_jcc cond; " "; symbol] )
                                
# 000 "/dev/stdout"
)
                })
                inf)
and conGoto =
    fun arg1 ->
        (update__Goto1
            {Camlburg.cost = (arg1.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let target = arg1.reg.Camlburg.action () in target)
            })
            ((update__Goto20
                {Camlburg.cost = (arg1._Fetch21.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch21.Camlburg.action ()
                        in
                            let (stacktop, w) = _v1 in (stacktop ,w))
                })
                ((update__Goto5
                    {Camlburg.cost = (arg1.symbol.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let symbol = arg1.symbol.Camlburg.action ()
                            in
                                symbol)
                    })
                    ((update_any
                        {Camlburg.cost = (arg1.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let any = arg1.any.Camlburg.action ()
                                in
                                    
# 341 "arch/amd64/amd64rec.mlb"
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
                                            
# 227 "arch/amd64/amd64rec.mlb"
                                            ( cat ["jmp"; " "; symbol] )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost = (arg1.reg.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let reg = arg1.reg.Camlburg.action ()
                                        in
                                            
# 230 "arch/amd64/amd64rec.mlb"
                                            ( cat ["jmp"; " *"; reg] )
                                            
# 000 "/dev/stdout"
)
                                }]))
                            inf))))
and conFetch =
    fun arg1 arg2 ->
        (update__Fetch21
            {Camlburg.cost = (arg1.stacktop.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let stacktop = arg1.stacktop.Camlburg.action ()
                    and w = arg2
                    in
                        (stacktop ,w))
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        and w = arg2
                        in
                            
# 319 "arch/amd64/amd64rec.mlb"
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
                                
# 185 "arch/amd64/amd64rec.mlb"
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
                                    
# 188 "arch/amd64/amd64rec.mlb"
                                    ( meml )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_pcv
                            {Camlburg.cost =
                                (arg1.pcl.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    let pcl = arg1.pcl.Camlburg.action ()
                                    in
                                        
# 184 "arch/amd64/amd64rec.mlb"
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
                                            
# 177 "arch/amd64/amd64rec.mlb"
                                            ( regl )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_rspv
                                    {Camlburg.cost =
                                        (arg1.rspl.Camlburg.cost
                                        +
                                        (Camlburg.matches 64) arg2)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                rspl =
                                                arg1.rspl.Camlburg.action ()
                                            in
                                                
# 182 "arch/amd64/amd64rec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_wreg16
                                        {Camlburg.cost =
                                            (arg1.wregl16.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    wregl16 =
                                                    arg1.wregl16.Camlburg.action
                                                        ()
                                                and w = arg2
                                                in
                                                    
# 215 "arch/amd64/amd64rec.mlb"
                                                    ( wregl16 )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        ((update_wreg32
                                            {Camlburg.cost =
                                                (arg1.wregl32.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        wregl32 =
                                                        arg1.wregl32.Camlburg.action
                                                            ()
                                                    and w = arg2
                                                    in
                                                        
# 217 "arch/amd64/amd64rec.mlb"
                                                        ( wregl32 )
                                                        
# 000 "/dev/stdout"
)
                                            })
                                            ((update_wreg8
                                                {Camlburg.cost =
                                                    (arg1.wregl8.Camlburg.cost)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        let
                                                            wregl8 =
                                                            arg1.wregl8.Camlburg.action
                                                                ()
                                                        and w = arg2
                                                        in
                                                            
# 213 "arch/amd64/amd64rec.mlb"
                                                            ( wregl8 )
                                                            
# 000 "/dev/stdout"
)
                                                })
                                                inf)))))))))
and conFalse =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 314 "arch/amd64/amd64rec.mlb"
                    ( cat [ "False" ] )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conDivu3 =
    fun arg1 arg2 arg3 ->
        (update_divu3val
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost
                +
                arg3.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let hi = arg1.reg.Camlburg.action ()
                    and lo = arg2.reg.Camlburg.action ()
                    and y = arg3.reg.Camlburg.action ()
                    in
                        
# 285 "arch/amd64/amd64rec.mlb"
                        ( y )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conCqto =
    fun arg1 ->
        (update_cqtoval
            {Camlburg.cost = (arg1.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let reg = arg1.reg.Camlburg.action ()
                    in
                        
# 276 "arch/amd64/amd64rec.mlb"
                        ( () )
                        
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
                        
# 326 "arch/amd64/amd64rec.mlb"
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
                            
# 243 "arch/amd64/amd64rec.mlb"
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
                        
# 317 "arch/amd64/amd64rec.mlb"
                        ( cat [ "Bits(b)" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost =
                    (let bits = arg1
                    in
                        
# 171 "arch/amd64/amd64rec.mlb"
                        ( guard (Bits.width bits = 64) )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let bits = arg1
                        in
                            
# 171 "arch/amd64/amd64rec.mlb"
                            ( const64 bits )
                            
# 000 "/dev/stdout"
)
                })
                ((update_constv
                    {Camlburg.cost =
                        (let bits = arg1
                        in
                            
# 172 "arch/amd64/amd64rec.mlb"
                            ( guard (Bits.width bits = 64) )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let bits = arg1
                            in
                                
# 172 "arch/amd64/amd64rec.mlb"
                                ( Bits.U.to_int64 bits )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_eightc
                        {Camlburg.cost =
                            (let b = arg1
                            in
                                
# 294 "arch/amd64/amd64rec.mlb"
                                ( guard (Bits.width b > 3 && Bits.Ops.eq (Bits.U.of_int 8 (Bits.width b)) b) )
                                
# 000 "/dev/stdout"
)
                        ;Camlburg.action =
                            (fun () ->
                                let b = arg1
                                in
                                    
# 295 "arch/amd64/amd64rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conAnd =
    fun arg1 arg2 ->
        (update__And10
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__And11
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
                                
# 323 "arch/amd64/amd64rec.mlb"
                                ( cat [ "And(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAdd =
    fun arg1 arg2 ->
        (update__Add19
            {Camlburg.cost =
                (arg1.rspv.Camlburg.cost + arg2.eightc.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let rspv = arg1.rspv.Camlburg.action ()
                    and eightc = arg2.eightc.Camlburg.action ()
                    in
                        (rspv ,eightc))
            })
            ((update__Add6
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.reg.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update__Add7
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
                                        
# 192 "arch/amd64/amd64rec.mlb"
                                        ( cat [const; "("; reg; ")"] )
                                        
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
                                        
# 193 "arch/amd64/amd64rec.mlb"
                                        ( cat [const; "("; reg; ")"] )
                                        
# 000 "/dev/stdout"
)
                            }]))
                        ((update_any
                            {Camlburg.cost =
                                (arg1.any.Camlburg.cost
                                +
                                arg2.any.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg1.any.Camlburg.action ()
                                    and y = arg2.any.Camlburg.action ()
                                    in
                                        
# 321 "arch/amd64/amd64rec.mlb"
                                        ( cat [ "Add(";x;", ";y;")" ] )
                                        
# 000 "/dev/stdout"
)
                            })
                            inf))))



# 83 "arch/amd64/amd64rec.mlb"

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
      | RP.App(("lobits", [_;_]), [x]) -> exp x
      | RP.App((("amd64_subcc"), [w]), [x; y]) -> conSubcc (exp x) (exp y)
      | RP.App((("amd64_e"|"amd64_ne"|"amd64_l"|"amd64_le"|"amd64_g"|"amd64_ge"
                |"amd64_b"|"amd64_be"|"amd64_a"|"amd64_ae") as op, [w]), [c]) ->
          conCond op (exp c)
      | RP.App(("amd64_cqto", [w]), [x])          -> conCqto (exp x)
      | RP.App(("amd64_quot", [w]), [hi; lo; y])  -> conQuot3 (exp hi) (exp lo) (exp y)
      | RP.App(("amd64_rem",  [w]), [hi; lo; y])  -> conRem3  (exp hi) (exp lo) (exp y)
      | RP.App(("amd64_divu", [w]), [hi; lo; y])  -> conDivu3 (exp hi) (exp lo) (exp y)
      | RP.App(("amd64_modu", [w]), [hi; lo; y])  -> conModu3 (exp hi) (exp lo) (exp y)
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

  let to_string r =
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
