
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
        _Subcc2: ( 't40 ) Camlburg.nt;
        _Subcc1: ( 't39 ) Camlburg.nt;
        _Sub7: ( 't38 ) Camlburg.nt;
        _Sub6: ( 't37 ) Camlburg.nt;
        _Sub12: ( 't36 ) Camlburg.nt;
        _Par17: ( 't35 ) Camlburg.nt;
        _Mul11: ( 't34 ) Camlburg.nt;
        _Mul10: ( 't33 ) Camlburg.nt;
        _Goto3: ( 't32 ) Camlburg.nt;
        _Goto16: ( 't31 ) Camlburg.nt;
        _Goto14: ( 't30 ) Camlburg.nt;
        _Fetch15: ( 't29 ) Camlburg.nt;
        _And9: ( 't28 ) Camlburg.nt;
        _And8: ( 't27 ) Camlburg.nt;
        _Add5: ( 't26 ) Camlburg.nt;
        _Add4: ( 't25 ) Camlburg.nt;
        _Add13: ( 't24 ) Camlburg.nt;
        symbol: ( 't23 ) Camlburg.nt;
        storeretaddr: ( 't22 ) Camlburg.nt;
        stacktop: ( 't21 ) Camlburg.nt;
        stacknext: ( 't20 ) Camlburg.nt;
        rspv: ( 't19 ) Camlburg.nt;
        rspl: ( 't18 ) Camlburg.nt;
        regl: ( 't17 ) Camlburg.nt;
        reg: ( 't16 ) Camlburg.nt;
        r: ( 't15 ) Camlburg.nt;
        push: ( 't14 ) Camlburg.nt;
        pop: ( 't13 ) Camlburg.nt;
        pcv: ( 't12 ) Camlburg.nt;
        pcl: ( 't11 ) Camlburg.nt;
        meml: ( 't10 ) Camlburg.nt;
        mem: ( 't9 ) Camlburg.nt;
        inst: ( 't8 ) Camlburg.nt;
        eightc: ( 't7 ) Camlburg.nt;
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
    ;eightc = (Camlburg.infinity)
    ;inst = (Camlburg.infinity)
    ;mem = (Camlburg.infinity)
    ;meml = (Camlburg.infinity)
    ;pcl = (Camlburg.infinity)
    ;pcv = (Camlburg.infinity)
    ;pop = (Camlburg.infinity)
    ;push = (Camlburg.infinity)
    ;r = (Camlburg.infinity)
    ;reg = (Camlburg.infinity)
    ;regl = (Camlburg.infinity)
    ;rspl = (Camlburg.infinity)
    ;rspv = (Camlburg.infinity)
    ;stacknext = (Camlburg.infinity)
    ;stacktop = (Camlburg.infinity)
    ;storeretaddr = (Camlburg.infinity)
    ;symbol = (Camlburg.infinity)
    ;_Add13 = (Camlburg.infinity)
    ;_Add4 = (Camlburg.infinity)
    ;_Add5 = (Camlburg.infinity)
    ;_And8 = (Camlburg.infinity)
    ;_And9 = (Camlburg.infinity)
    ;_Fetch15 = (Camlburg.infinity)
    ;_Goto14 = (Camlburg.infinity)
    ;_Goto16 = (Camlburg.infinity)
    ;_Goto3 = (Camlburg.infinity)
    ;_Mul10 = (Camlburg.infinity)
    ;_Mul11 = (Camlburg.infinity)
    ;_Par17 = (Camlburg.infinity)
    ;_Sub12 = (Camlburg.infinity)
    ;_Sub6 = (Camlburg.infinity)
    ;_Sub7 = (Camlburg.infinity)
    ;_Subcc1 = (Camlburg.infinity)
    ;_Subcc2 = (Camlburg.infinity)
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
                                
# 269 "arch/amd64/amd64rec.mlb"
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
                                
# 171 "arch/amd64/amd64rec.mlb"
                                ( reg r )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with r = (nt) }
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
                                
# 186 "arch/amd64/amd64rec.mlb"
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
and update__Add13 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add13.Camlburg.cost then
            x
        else
            { x with _Add13 = (nt) }
and update__Add4 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add4.Camlburg.cost then
            x
        else
            { x with _Add4 = (nt) }
and update__Add5 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add5.Camlburg.cost then
            x
        else
            { x with _Add5 = (nt) }
and update__And8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And8.Camlburg.cost then
            x
        else
            { x with _And8 = (nt) }
and update__And9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And9.Camlburg.cost then
            x
        else
            { x with _And9 = (nt) }
and update__Fetch15 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch15.Camlburg.cost then
            x
        else
            { x with _Fetch15 = (nt) }
and update__Goto14 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto14.Camlburg.cost then
            x
        else
            { x with _Goto14 = (nt) }
and update__Goto16 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto16.Camlburg.cost then
            x
        else
            { x with _Goto16 = (nt) }
and update__Goto3 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto3.Camlburg.cost then
            x
        else
            { x with _Goto3 = (nt) }
and update__Mul10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul10.Camlburg.cost then
            x
        else
            { x with _Mul10 = (nt) }
and update__Mul11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul11.Camlburg.cost then
            x
        else
            { x with _Mul11 = (nt) }
and update__Par17 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Par17.Camlburg.cost then
            x
        else
            { x with _Par17 = (nt) }
and update__Sub12 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub12.Camlburg.cost then
            x
        else
            { x with _Sub12 = (nt) }
and update__Sub6 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub6.Camlburg.cost then
            x
        else
            { x with _Sub6 = (nt) }
and update__Sub7 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub7.Camlburg.cost then
            x
        else
            { x with _Sub7 = (nt) }
and update__Subcc1 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc1.Camlburg.cost then
            x
        else
            { x with _Subcc1 = (nt) }
and update__Subcc2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc2.Camlburg.cost then
            x
        else
            { x with _Subcc2 = (nt) }


let rec
conZx =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 286 "arch/amd64/amd64rec.mlb"
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
                    
# 271 "arch/amd64/amd64rec.mlb"
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
                        
# 285 "arch/amd64/amd64rec.mlb"
                        ( cat [ "Sx(";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conSubcc =
    fun arg1 arg2 ->
        (update__Subcc1
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Subcc2
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
                                
# 283 "arch/amd64/amd64rec.mlb"
                                ( cat [ "Subcc(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conSub =
    fun arg1 arg2 ->
        (update__Sub12
            {Camlburg.cost =
                (arg1.rspv.Camlburg.cost + arg2.eightc.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let rspv = arg1.rspv.Camlburg.action ()
                    and eightc = arg2.eightc.Camlburg.action ()
                    in
                        (rspv ,eightc))
            })
            ((update__Sub6
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.reg.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update__Sub7
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
                                    
# 280 "arch/amd64/amd64rec.mlb"
                                    ( cat [ "Sub(";x;", ";y;")" ] )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conStore =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let dst = arg1.any.Camlburg.action ()
                    and src = arg2.any.Camlburg.action ()
                    and w = arg3
                    in
                        
# 293 "arch/amd64/amd64rec.mlb"
                        ( cat [ "Store(";dst;",";src;",";string_of_int w;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2.symbol.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let regl = arg1.regl.Camlburg.action ()
                            and symbol = arg2.symbol.Camlburg.action ()
                            in
                                
# 192 "arch/amd64/amd64rec.mlb"
                                ( sprintf "leaq %s(%%rip), %s" symbol regl )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2.constv.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let regl = arg1.regl.Camlburg.action ()
                            and constv = arg2.constv.Camlburg.action ()
                            in
                                
# 195 "arch/amd64/amd64rec.mlb"
                                ( sprintf "movabsq $%Ld, %s" constv regl )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2.mem.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let regl = arg1.regl.Camlburg.action ()
                            and mem = arg2.mem.Camlburg.action ()
                            in
                                
# 198 "arch/amd64/amd64rec.mlb"
                                ( cat ["movq"; " "; mem; ", "; regl] )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.meml.Camlburg.cost + arg2.reg.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let meml = arg1.meml.Camlburg.action ()
                            and reg = arg2.reg.Camlburg.action ()
                            in
                                
# 201 "arch/amd64/amd64rec.mlb"
                                ( cat ["movq"; " "; reg; ", "; meml] )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2.reg.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let regl = arg1.regl.Camlburg.action ()
                            and reg = arg2.reg.Camlburg.action ()
                            in
                                
# 204 "arch/amd64/amd64rec.mlb"
                                ( cat ["movq"; " "; reg; ", "; regl] )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.ccl.Camlburg.cost + arg2._Subcc1.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let ccl = arg1.ccl.Camlburg.action ()
                            and _v1 = arg2._Subcc1.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 214 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "cmpq %s, %s" y x )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.ccl.Camlburg.cost + arg2._Subcc2.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let ccl = arg1.ccl.Camlburg.action ()
                            and _v1 = arg2._Subcc2.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 216 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "cmpq $%Ld, %s" y x )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2._Add4.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg2._Add4.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 224 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "movq %s, %s\n\taddq %s, %s" x dst y dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2._Add5.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg2._Add5.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 227 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "movq %s, %s\n\taddq $%Ld, %s" x dst y dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2._Sub6.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg2._Sub6.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 230 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "movq %s, %s\n\tsubq %s, %s" x dst y dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2._Sub7.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg2._Sub7.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 233 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "movq %s, %s\n\tsubq $%Ld, %s" x dst y dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2._And8.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg2._And8.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 236 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "movq %s, %s\n\tandq %s, %s" x dst y dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2._And9.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg2._And9.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 239 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "movq %s, %s\n\tandq $%Ld, %s" x dst y dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2._Mul10.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg2._Mul10.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 243 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "movq %s, %s\n\timulq %s, %s" x dst y dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost + arg2._Mul11.Camlburg.cost
                        +
                        (Camlburg.matches 64) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg2._Mul11.Camlburg.action ()
                            in
                                let (x, y) = _v1
                                in
                                    
# 246 "arch/amd64/amd64rec.mlb"
                                    ( sprintf "imulq $%Ld, %s, %s" y x dst )
                                    
# 000 "/dev/stdout"
)
                    }]))
                ((update_pop
                    {Camlburg.cost =
                        (arg1.rspl.Camlburg.cost + arg2._Add13.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let rspl = arg1.rspl.Camlburg.action ()
                            and _v1 = arg2._Add13.Camlburg.action ()
                            and w = arg3
                            in
                                let (rspv, eightc) = _v1
                                in
                                    
# 255 "arch/amd64/amd64rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                    })
                    ((update_push
                        {Camlburg.cost =
                            (arg1.rspl.Camlburg.cost
                            +
                            arg2._Sub12.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let rspl = arg1.rspl.Camlburg.action ()
                                and _v1 = arg2._Sub12.Camlburg.action ()
                                and w = arg3
                                in
                                    let (rspv, eightc) = _v1
                                    in
                                        
# 254 "arch/amd64/amd64rec.mlb"
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
                                        arg1.stacknext.Camlburg.action ()
                                    and pcv = arg2.pcv.Camlburg.action ()
                                    and w = arg3
                                    in
                                        
# 258 "arch/amd64/amd64rec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            inf))))
and conReg =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let char = arg1
                    and n = arg2
                    in
                        
# 290 "arch/amd64/amd64rec.mlb"
                        ( cat [ "Reg('";Char.escaped char;"',"; string_of_int n;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_ccl
                {Camlburg.cost =
                    ((Camlburg.matches 'c') arg1 + (Camlburg.matches 2) arg2)
                ;Camlburg.action =
                    (fun () ->
                        
# 175 "arch/amd64/amd64rec.mlb"
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
                            
# 174 "arch/amd64/amd64rec.mlb"
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
                                    
# 170 "arch/amd64/amd64rec.mlb"
                                    ( n )
                                    
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
                                    
# 176 "arch/amd64/amd64rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                            })
                            inf))))
and conPar =
    fun arg1 arg2 ->
        (update__Par17
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
                            
# 298 "arch/amd64/amd64rec.mlb"
                            ( cat [ "Par(";l;",";r;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                ((update_inst
                    (Camlburg.choice
                        [{Camlburg.cost =
                            (arg1._Goto14.Camlburg.cost
                            +
                            arg2.pop.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto14.Camlburg.action ()
                                and pop = arg2.pop.Camlburg.action ()
                                in
                                    let (stacktop, w) = _v1
                                    in
                                        
# 261 "arch/amd64/amd64rec.mlb"
                                        ( "ret" )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Goto16.Camlburg.cost
                            +
                            arg2._Par17.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto16.Camlburg.action ()
                                and _v2 = arg2._Par17.Camlburg.action ()
                                in
                                    let (storeretaddr, push) = _v2
                                    in
                                        let target = _v1
                                        in
                                            
# 264 "arch/amd64/amd64rec.mlb"
                                            ( cat ["call"; " *"; target] )
                                            
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Goto3.Camlburg.cost
                            +
                            arg2._Par17.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto3.Camlburg.action ()
                                and _v2 = arg2._Par17.Camlburg.action ()
                                in
                                    let (storeretaddr, push) = _v2
                                    in
                                        let target = _v1
                                        in
                                            
# 267 "arch/amd64/amd64rec.mlb"
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
                    
# 248 "arch/amd64/amd64rec.mlb"
                    ( "nop" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update__Mul10
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Mul11
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
                                
# 282 "arch/amd64/amd64rec.mlb"
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
                        
# 289 "arch/amd64/amd64rec.mlb"
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
                            
# 182 "arch/amd64/amd64rec.mlb"
                            ( addr )
                            
# 000 "/dev/stdout"
)
                })
                ((update_stacknext
                    {Camlburg.cost = (arg1._Sub12.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sub12.Camlburg.action ()
                            in
                                let (rspv, eightc) = _v1
                                in
                                    
# 257 "arch/amd64/amd64rec.mlb"
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
                                    
# 256 "arch/amd64/amd64rec.mlb"
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
                        
# 287 "arch/amd64/amd64rec.mlb"
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
                        
# 273 "arch/amd64/amd64rec.mlb"
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
                            
# 168 "arch/amd64/amd64rec.mlb"
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
                        
# 274 "arch/amd64/amd64rec.mlb"
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
                        
# 294 "arch/amd64/amd64rec.mlb"
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
                        
# 297 "arch/amd64/amd64rec.mlb"
                        ( cat [ "Guarded(";guard;",";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                {Camlburg.cost =
                    (arg1.cond.Camlburg.cost + arg2._Goto3.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let cond = arg1.cond.Camlburg.action ()
                        and _v1 = arg2._Goto3.Camlburg.action ()
                        in
                            let symbol = _v1
                            in
                                
# 221 "arch/amd64/amd64rec.mlb"
                                ( cat [amd64_jcc cond; " "; symbol] )
                                
# 000 "/dev/stdout"
)
                })
                inf)
and conGoto =
    fun arg1 ->
        (update__Goto14
            {Camlburg.cost = (arg1._Fetch15.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch15.Camlburg.action ()
                    in
                        let (stacktop, w) = _v1 in (stacktop ,w))
            })
            ((update__Goto16
                {Camlburg.cost = (arg1.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let target = arg1.reg.Camlburg.action () in target)
                })
                ((update__Goto3
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
                                    
# 299 "arch/amd64/amd64rec.mlb"
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
                                            
# 207 "arch/amd64/amd64rec.mlb"
                                            ( cat ["jmp"; " "; symbol] )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost = (arg1.reg.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let reg = arg1.reg.Camlburg.action ()
                                        in
                                            
# 210 "arch/amd64/amd64rec.mlb"
                                            ( cat ["jmp"; " *"; reg] )
                                            
# 000 "/dev/stdout"
)
                                }]))
                            inf))))
and conFetch =
    fun arg1 arg2 ->
        (update__Fetch15
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
                            
# 277 "arch/amd64/amd64rec.mlb"
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
                                
# 180 "arch/amd64/amd64rec.mlb"
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
                                    
# 183 "arch/amd64/amd64rec.mlb"
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
                                        
# 179 "arch/amd64/amd64rec.mlb"
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
                                            
# 172 "arch/amd64/amd64rec.mlb"
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
                                                
# 177 "arch/amd64/amd64rec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    inf))))))
and conFalse =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 272 "arch/amd64/amd64rec.mlb"
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
                        
# 284 "arch/amd64/amd64rec.mlb"
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
                            
# 219 "arch/amd64/amd64rec.mlb"
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
                        
# 275 "arch/amd64/amd64rec.mlb"
                        ( cat [ "Bits(b)" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost =
                    (let bits = arg1
                    in
                        
# 166 "arch/amd64/amd64rec.mlb"
                        ( guard (Bits.width bits = 64) )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let bits = arg1
                        in
                            
# 166 "arch/amd64/amd64rec.mlb"
                            ( const64 bits )
                            
# 000 "/dev/stdout"
)
                })
                ((update_constv
                    {Camlburg.cost =
                        (let bits = arg1
                        in
                            
# 167 "arch/amd64/amd64rec.mlb"
                            ( guard (Bits.width bits = 64) )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let bits = arg1
                            in
                                
# 167 "arch/amd64/amd64rec.mlb"
                                ( Bits.U.to_int64 bits )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_eightc
                        {Camlburg.cost =
                            (let b = arg1
                            in
                                
# 252 "arch/amd64/amd64rec.mlb"
                                ( guard (Bits.width b > 3 && Bits.Ops.eq (Bits.U.of_int 8 (Bits.width b)) b) )
                                
# 000 "/dev/stdout"
)
                        ;Camlburg.action =
                            (fun () ->
                                let b = arg1
                                in
                                    
# 253 "arch/amd64/amd64rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conAnd =
    fun arg1 arg2 ->
        (update__And8
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__And9
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
                                
# 281 "arch/amd64/amd64rec.mlb"
                                ( cat [ "And(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAdd =
    fun arg1 arg2 ->
        (update__Add13
            {Camlburg.cost =
                (arg1.rspv.Camlburg.cost + arg2.eightc.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let rspv = arg1.rspv.Camlburg.action ()
                    and eightc = arg2.eightc.Camlburg.action ()
                    in
                        (rspv ,eightc))
            })
            ((update__Add4
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.reg.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update__Add5
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
                                        
# 187 "arch/amd64/amd64rec.mlb"
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
                                        
# 188 "arch/amd64/amd64rec.mlb"
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
                                        
# 279 "arch/amd64/amd64rec.mlb"
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
