
# 1 "arch/arm/armrec.mlb"

  (* claude: no upstream armrec.nw exists to port (see arm.ml's header
   * comment: upstream's src/ only ever had arm.nw, the target-description
   * module alone) - this grammar is written from scratch, modeled on
   * arch/mips/mipsrec.mlb's shape (%head/%tail structure, exp/loc/effect/
   * guarded dispatch) and arch/sparc/sparcrec.mlb's cc-register handling
   * (ARM's condition codes are a single flags pseudo-location, like
   * SPARC's, not MIPS's direct register-vs-register branch). Only the
   * subset of ARM actually needed by demos/hello.c-- (and, incidentally,
   * the tiger runtime's simple integer/pointer code) is covered: base+
   * immediate-offset addressing, add/sub/and, the arm_subcc/arm_<cond>
   * comparison family arm.ml's Post already commits to, and plain direct
   * calls/branches. No barrel-shifter operand2, no multiply/divide.
   *)
  open Nopoly
  module RP = Rtl.Private
  module RU = Rtlutil
  module Up = Rtl.Up
  module Dn = Rtl.Dn
  module SS = Space.Standard32

  exception Error of string
  let error msg = raise (Error msg)
  let sprintf   = Printf.sprintf

  let guard p = if p then 0 else Camlburg.inf_cost

  (* claude: signed, not unsigned, decimal - two reasons: (1) the "[reg,
   * #imm]" load/store addressing rules below print this same "imm" as a
   * real 12-bit signed offset field, where a negative offset must read
   * as e.g. "-4", not "4294967292" (arm-linux-gnueabihf-as rejects the
   * latter as out of range - same class of bug mipsrec.mlb's own
   * const32 comment documents for MIPS's imm16); (2) GNU as's ARM port
   * auto-swaps add<->sub (and cmp<->cmn, mov<->mvn) when a negative
   * immediate's positive form fits the rotated-8-bit encoding but the
   * negative one doesn't (e.g. "add r13,r13,#-8" -> "sub r13,r13,#8"),
   * which only fires if the immediate is actually written as negative. *)
  let const32 b =
      assert (Bits.width b = 32);
      Nativeint.to_string (Bits.S.to_native b)

  let cat     = String.concat ""
  let sprintf = Printf.sprintf

  let reg n   = "r" ^ string_of_int n

  (* claude: ARM's load/store suffixes: plain "ldr"/"str" are the 32-bit
   * (word) forms (empty suffix), "b" is byte, "h" is halfword - unlike
   * MIPS, where every width always gets an explicit suffix letter
   * (lw/lb/lh). *)
  let ldst_suffix = function
      | 8  -> "b"
      | 16 -> "h"
      | 32 -> ""
      | w  -> error (sprintf "not an ARM load/store width: %d" w)

  (* claude: sign-extending sub-word loads need a distinct mnemonic
   * ("ldrsb"/"ldrsh" - ARM has no plain 32-bit sign-extend load, so 32
   * never reaches this), whereas a zero-extending load is just the
   * ordinary "ldrb"/"ldrh" (ARM zero-extends sub-word loads by default). *)
  let sx_suffix = function
      | 8  -> "sb"
      | 16 -> "sh"
      | w  -> error (sprintf "not an ARM sign-extend load width: %d" w)

  (* claude: maps arm.ml's Post.arm_cond names (arm_eq/arm_ne/.../arm_hi -
   * the exact operator names Post.subflags/bc_guard build, see arm.ml
   * lines ~150-198) to the real ARM branch-suffix mnemonics. arm_vs/
   * arm_vc (overflow) are listed for completeness even though arm.ml's
   * arm_cond never actually produces them yet (Impossible.unimp there). *)
  let arm_bcond = function
      | "arm_eq" -> "beq"
      | "arm_ne" -> "bne"
      | "arm_lt" -> "blt"
      | "arm_le" -> "ble"
      | "arm_gt" -> "bgt"
      | "arm_ge" -> "bge"
      | "arm_ls" -> "bls"
      | "arm_hi" -> "bhi"
      | "arm_vs" -> "bvs"
      | "arm_vc" -> "bvc"
      | op       -> error (sprintf "not an ARM condition: %s" op)


# 000 "/dev/stdout"


type
    (
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
        _Zx3: ( 't36 ) Camlburg.nt;
        _Sx1: ( 't35 ) Camlburg.nt;
        _Subcc9: ( 't34 ) Camlburg.nt;
        _Subcc8: ( 't33 ) Camlburg.nt;
        _Sub13: ( 't32 ) Camlburg.nt;
        _Sub12: ( 't31 ) Camlburg.nt;
        _Store7: ( 't30 ) Camlburg.nt;
        _Store5: ( 't29 ) Camlburg.nt;
        _Goto6: ( 't28 ) Camlburg.nt;
        _Goto4: ( 't27 ) Camlburg.nt;
        _Fetch2: ( 't26 ) Camlburg.nt;
        _And15: ( 't25 ) Camlburg.nt;
        _And14: ( 't24 ) Camlburg.nt;
        _Add11: ( 't23 ) Camlburg.nt;
        _Add10: ( 't22 ) Camlburg.nt;
        symbol: ( 't21 ) Camlburg.nt;
        spl: ( 't20 ) Camlburg.nt;
        sp: ( 't19 ) Camlburg.nt;
        regl: ( 't18 ) Camlburg.nt;
        reg: ( 't17 ) Camlburg.nt;
        ral: ( 't16 ) Camlburg.nt;
        ra: ( 't15 ) Camlburg.nt;
        r: ( 't14 ) Camlburg.nt;
        pcl: ( 't13 ) Camlburg.nt;
        pc: ( 't12 ) Camlburg.nt;
        next: ( 't11 ) Camlburg.nt;
        meml: ( 't10 ) Camlburg.nt;
        mem: ( 't9 ) Camlburg.nt;
        limm: ( 't8 ) Camlburg.nt;
        inst: ( 't7 ) Camlburg.nt;
        imm: ( 't6 ) Camlburg.nt;
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
    ;imm = (Camlburg.infinity)
    ;inst = (Camlburg.infinity)
    ;limm = (Camlburg.infinity)
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
    ;_Add10 = (Camlburg.infinity)
    ;_Add11 = (Camlburg.infinity)
    ;_And14 = (Camlburg.infinity)
    ;_And15 = (Camlburg.infinity)
    ;_Fetch2 = (Camlburg.infinity)
    ;_Goto4 = (Camlburg.infinity)
    ;_Goto6 = (Camlburg.infinity)
    ;_Store5 = (Camlburg.infinity)
    ;_Store7 = (Camlburg.infinity)
    ;_Sub12 = (Camlburg.infinity)
    ;_Sub13 = (Camlburg.infinity)
    ;_Subcc8 = (Camlburg.infinity)
    ;_Subcc9 = (Camlburg.infinity)
    ;_Sx1 = (Camlburg.infinity)
    ;_Zx3 = (Camlburg.infinity)
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
                                
# 273 "arch/arm/armrec.mlb"
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
            (fun x ->
                (update_imm
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let const = x.const.Camlburg.action ()
                            in
                                
# 198 "arch/arm/armrec.mlb"
                                ( const )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                ((fun x ->
                    (update_limm
                        {Camlburg.cost = (nt.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let const = x.const.Camlburg.action ()
                                in
                                    
# 201 "arch/arm/armrec.mlb"
                                    ( const  )
                                    
# 000 "/dev/stdout"
)
                        })
                        x)
                    { x with const = (nt) })
and update_imm =
    fun nt x ->
        if nt.Camlburg.cost >= x.imm.Camlburg.cost then
            x
        else
            { x with imm = (nt) }
and update_inst =
    fun nt x ->
        if nt.Camlburg.cost >= x.inst.Camlburg.cost then
            x
        else
            { x with inst = (nt) }
and update_limm =
    fun nt x ->
        if nt.Camlburg.cost >= x.limm.Camlburg.cost then
            x
        else
            { x with limm = (nt) }
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
                                
# 176 "arch/arm/armrec.mlb"
                                ( reg r )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with r = (nt) }
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
                                
# 193 "arch/arm/armrec.mlb"
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
            (fun x ->
                (update_limm
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let symbol = x.symbol.Camlburg.action ()
                            in
                                
# 202 "arch/arm/armrec.mlb"
                                ( symbol )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with symbol = (nt) }
and update__Add10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add10.Camlburg.cost then
            x
        else
            { x with _Add10 = (nt) }
and update__Add11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add11.Camlburg.cost then
            x
        else
            { x with _Add11 = (nt) }
and update__And14 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And14.Camlburg.cost then
            x
        else
            { x with _And14 = (nt) }
and update__And15 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And15.Camlburg.cost then
            x
        else
            { x with _And15 = (nt) }
and update__Fetch2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch2.Camlburg.cost then
            x
        else
            { x with _Fetch2 = (nt) }
and update__Goto4 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto4.Camlburg.cost then
            x
        else
            { x with _Goto4 = (nt) }
and update__Goto6 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto6.Camlburg.cost then
            x
        else
            { x with _Goto6 = (nt) }
and update__Store5 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store5.Camlburg.cost then
            x
        else
            { x with _Store5 = (nt) }
and update__Store7 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store7.Camlburg.cost then
            x
        else
            { x with _Store7 = (nt) }
and update__Sub12 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub12.Camlburg.cost then
            x
        else
            { x with _Sub12 = (nt) }
and update__Sub13 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub13.Camlburg.cost then
            x
        else
            { x with _Sub13 = (nt) }
and update__Subcc8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc8.Camlburg.cost then
            x
        else
            { x with _Subcc8 = (nt) }
and update__Subcc9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc9.Camlburg.cost then
            x
        else
            { x with _Subcc9 = (nt) }
and update__Sx1 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sx1.Camlburg.cost then
            x
        else
            { x with _Sx1 = (nt) }
and update__Zx3 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zx3.Camlburg.cost then
            x
        else
            { x with _Zx3 = (nt) }


let rec
conZx =
    fun arg1 ->
        (update__Zx3
            {Camlburg.cost = (arg1._Fetch2.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch2.Camlburg.action ()
                    in
                        let (mem, x) = _v1 in (mem ,x))
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        in
                            
# 289 "arch/arm/armrec.mlb"
                            ( cat [ "Zx(";any;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conTrue =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 275 "arch/arm/armrec.mlb"
                    ( cat [ "True"  ] )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conSx =
    fun arg1 ->
        (update__Sx1
            {Camlburg.cost = (arg1._Fetch2.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch2.Camlburg.action ()
                    in
                        let (mem, x) = _v1 in (mem ,x))
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        in
                            
# 288 "arch/arm/armrec.mlb"
                            ( cat [ "Sx(";any;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conSubcc =
    fun arg1 arg2 ->
        (update__Subcc8
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Subcc9
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.imm.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.imm.Camlburg.action ()
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
                                
# 286 "arch/arm/armrec.mlb"
                                ( cat [ "Subcc(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conSub =
    fun arg1 arg2 ->
        (update__Sub12
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Sub13
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.imm.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.imm.Camlburg.action ()
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
                                
# 284 "arch/arm/armrec.mlb"
                                ( cat [ "Sub(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conStore =
    fun arg1 arg2 arg3 ->
        (update__Store5
            {Camlburg.cost =
                (arg1.ral.Camlburg.cost + arg2.next.Camlburg.cost
                +
                (Camlburg.matches 32) arg3)
            ;Camlburg.action =
                (fun () ->
                    let ral = arg1.ral.Camlburg.action ()
                    and next = arg2.next.Camlburg.action ()
                    in
                        (ral ,next))
            })
            ((update__Store7
                {Camlburg.cost =
                    (arg1.spl.Camlburg.cost + arg2.reg.Camlburg.cost
                    +
                    (Camlburg.matches 32) arg3)
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
                                
# 296 "arch/arm/armrec.mlb"
                                ( cat [ "Store(";dst;",";src;",";string_of_int w;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_inst
                        (Camlburg.choice
                            [{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.limm.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and limm = arg2.limm.Camlburg.action ()
                                    in
                                        
# 206 "arch/arm/armrec.mlb"
                                        ( cat ["ldr"; " "; regl; ", ="; limm] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.mem.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and mem = arg2.mem.Camlburg.action ()
                                    in
                                        
# 209 "arch/arm/armrec.mlb"
                                        ( cat ["ldr"; " "; regl; ", "; mem] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sx1.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sx1.Camlburg.action ()
                                    and w = arg3
                                    in
                                        let (mem, x) = _v1
                                        in
                                            
# 212 "arch/arm/armrec.mlb"
                                            ( cat ["ldr"; sx_suffix w; " "; regl; ", "; mem] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Zx3.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Zx3.Camlburg.action ()
                                    and w = arg3
                                    in
                                        let (mem, x) = _v1
                                        in
                                            
# 215 "arch/arm/armrec.mlb"
                                            ( cat ["ldr"; ldst_suffix w; " "; regl; ", "; mem] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.meml.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let meml = arg1.meml.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    and w = arg3
                                    in
                                        
# 218 "arch/arm/armrec.mlb"
                                        ( cat ["str"; ldst_suffix w; " "; reg; ", "; meml] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    in
                                        
# 221 "arch/arm/armrec.mlb"
                                        ( cat ["mov"; " "; regl; ", "; reg] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.ccl.Camlburg.cost
                                +
                                arg2._Subcc8.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let ccl = arg1.ccl.Camlburg.action ()
                                    and _v1 = arg2._Subcc8.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 243 "arch/arm/armrec.mlb"
                                            ( cat ["cmp"; " "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.ccl.Camlburg.cost
                                +
                                arg2._Subcc9.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let ccl = arg1.ccl.Camlburg.action ()
                                    and _v1 = arg2._Subcc9.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 245 "arch/arm/armrec.mlb"
                                            ( cat ["ldr ip, ="; y; "\n\tcmp "; x; ", ip"] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add10.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add10.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 253 "arch/arm/armrec.mlb"
                                            ( cat ["add"; " "; dst; ", "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add11.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add11.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 257 "arch/arm/armrec.mlb"
                                            ( cat ["ldr ip, ="; y; "\n\tadd "; dst; ", "; x; ", ip"] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sub12.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sub12.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 260 "arch/arm/armrec.mlb"
                                            ( cat ["sub"; " "; dst; ", "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sub13.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sub13.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 263 "arch/arm/armrec.mlb"
                                            ( cat ["ldr ip, ="; y; "\n\tsub "; dst; ", "; x; ", ip"] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._And14.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._And14.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 266 "arch/arm/armrec.mlb"
                                            ( cat ["and"; " "; dst; ", "; x; ", "; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._And15.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._And15.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 269 "arch/arm/armrec.mlb"
                                            ( cat ["ldr ip, ="; y; "\n\tand "; dst; ", "; x; ", ip"] )
                                            
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
                        
# 293 "arch/arm/armrec.mlb"
                        ( cat [ "Reg('";Char.escaped char;"',"; string_of_int n;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_ccl
                {Camlburg.cost =
                    ((Camlburg.matches 'c') arg1 + (Camlburg.matches 2) arg2)
                ;Camlburg.action =
                    (fun () ->
                        
# 180 "arch/arm/armrec.mlb"
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
                            
# 179 "arch/arm/armrec.mlb"
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
                                    
# 175 "arch/arm/armrec.mlb"
                                    ( n )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_ral
                            {Camlburg.cost =
                                ((Camlburg.matches 'r') arg1
                                +
                                (Camlburg.matches 14) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    
# 182 "arch/arm/armrec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                            })
                            ((update_spl
                                {Camlburg.cost =
                                    ((Camlburg.matches 'r') arg1
                                    +
                                    (Camlburg.matches 13) arg2)
                                ;Camlburg.action =
                                    (fun () ->
                                        
# 181 "arch/arm/armrec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                                })
                                inf)))))
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
                        
# 301 "arch/arm/armrec.mlb"
                        ( cat [ "Par(";l;",";r;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1._Goto4.Camlburg.cost
                        +
                        arg2._Store5.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto4.Camlburg.action ()
                            and _v2 = arg2._Store5.Camlburg.action ()
                            in
                                let (ral, next) = _v2
                                in
                                    let symbol = _v1
                                    in
                                        
# 232 "arch/arm/armrec.mlb"
                                        ( cat ["bl"; " "; symbol] )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto6.Camlburg.cost
                        +
                        arg2._Store5.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto6.Camlburg.action ()
                            and _v2 = arg2._Store5.Camlburg.action ()
                            in
                                let (ral, next) = _v2
                                in
                                    let target = _v1
                                    in
                                        
# 235 "arch/arm/armrec.mlb"
                                        ( cat ["blx"; " "; target] )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto6.Camlburg.cost
                        +
                        arg2._Store7.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto6.Camlburg.action ()
                            and _v2 = arg2._Store7.Camlburg.action ()
                            in
                                let (spl, nsp) = _v2
                                in
                                    let target = _v1
                                    in
                                        
# 239 "arch/arm/armrec.mlb"
                                        ( cat ["mov sp, "; nsp; "\n\tbx "; target] )
                                        
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
                    
# 271 "arch/arm/armrec.mlb"
                    ( "nop" )
                    
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
                        
# 292 "arch/arm/armrec.mlb"
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
                            
# 189 "arch/arm/armrec.mlb"
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
                        
# 290 "arch/arm/armrec.mlb"
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
                        
# 277 "arch/arm/armrec.mlb"
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
                            
# 173 "arch/arm/armrec.mlb"
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
                        
# 278 "arch/arm/armrec.mlb"
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
                        
# 297 "arch/arm/armrec.mlb"
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
                        
# 300 "arch/arm/armrec.mlb"
                        ( cat [ "Guarded(";guard;",";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                {Camlburg.cost =
                    (arg1.cond.Camlburg.cost + arg2._Goto4.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let cond = arg1.cond.Camlburg.action ()
                        and _v1 = arg2._Goto4.Camlburg.action ()
                        in
                            let symbol = _v1
                            in
                                
# 250 "arch/arm/armrec.mlb"
                                ( cat [arm_bcond cond; " "; symbol] )
                                
# 000 "/dev/stdout"
)
                })
                inf)
and conGoto =
    fun arg1 ->
        (update__Goto4
            {Camlburg.cost = (arg1.symbol.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let symbol = arg1.symbol.Camlburg.action () in symbol)
            })
            ((update__Goto6
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
                                
# 302 "arch/arm/armrec.mlb"
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
                                        
# 224 "arch/arm/armrec.mlb"
                                        ( cat ["b"; " "; symbol] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost = (arg1.reg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let reg = arg1.reg.Camlburg.action ()
                                    in
                                        
# 227 "arch/arm/armrec.mlb"
                                        ( cat ["bx"; " "; reg] )
                                        
# 000 "/dev/stdout"
)
                            }]))
                        inf)))
and conFetch =
    fun arg1 arg2 ->
        (update__Fetch2
            {Camlburg.cost = (arg1.mem.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let mem = arg1.mem.Camlburg.action ()
                    and x = arg2
                    in
                        (mem ,x))
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        and w = arg2
                        in
                            
# 281 "arch/arm/armrec.mlb"
                            ( cat [ "Fetch(";any;",";string_of_int w;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                ((update_ccval
                    {Camlburg.cost =
                        (arg1.ccl.Camlburg.cost + (Camlburg.matches 32) arg2)
                    ;Camlburg.action =
                        (fun () ->
                            let ccl = arg1.ccl.Camlburg.action ()
                            in
                                
# 185 "arch/arm/armrec.mlb"
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
                                    
# 190 "arch/arm/armrec.mlb"
                                    ( meml )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_pc
                            {Camlburg.cost =
                                (arg1.pcl.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    let pcl = arg1.pcl.Camlburg.action ()
                                    in
                                        
# 184 "arch/arm/armrec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_ra
                                {Camlburg.cost =
                                    (arg1.ral.Camlburg.cost
                                    +
                                    (Camlburg.matches 32) arg2)
                                ;Camlburg.action =
                                    (fun () ->
                                        let ral = arg1.ral.Camlburg.action ()
                                        in
                                            
# 187 "arch/arm/armrec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_reg
                                    {Camlburg.cost =
                                        (arg1.regl.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                regl =
                                                arg1.regl.Camlburg.action ()
                                            and w = arg2
                                            in
                                                
# 177 "arch/arm/armrec.mlb"
                                                ( regl )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_sp
                                        {Camlburg.cost =
                                            (arg1.spl.Camlburg.cost
                                            +
                                            (Camlburg.matches 32) arg2)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    spl =
                                                    arg1.spl.Camlburg.action
                                                        ()
                                                in
                                                    
# 186 "arch/arm/armrec.mlb"
                                                    ( () )
                                                    
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
                    
# 276 "arch/arm/armrec.mlb"
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
                        
# 287 "arch/arm/armrec.mlb"
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
                            
# 248 "arch/arm/armrec.mlb"
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
                        
# 279 "arch/arm/armrec.mlb"
                        ( cat [ "Bits(b)" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost =
                    (let bits = arg1
                    in
                        
# 172 "arch/arm/armrec.mlb"
                        ( guard (Bits.width bits = 32)  )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let bits = arg1
                        in
                            
# 172 "arch/arm/armrec.mlb"
                            ( const32 bits )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conAnd =
    fun arg1 arg2 ->
        (update__And14
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__And15
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.imm.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.imm.Camlburg.action ()
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
                                
# 285 "arch/arm/armrec.mlb"
                                ( cat [ "And(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAdd =
    fun arg1 arg2 ->
        (update__Add10
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Add11
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.imm.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.imm.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update_addr
                    (Camlburg.choice
                        [{Camlburg.cost =
                            (arg1.imm.Camlburg.cost + arg2.reg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let imm = arg1.imm.Camlburg.action ()
                                and reg = arg2.reg.Camlburg.action ()
                                in
                                    
# 194 "arch/arm/armrec.mlb"
                                    ( cat ["["; reg; ", #"; imm; "]"] )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.reg.Camlburg.cost + arg2.imm.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let reg = arg1.reg.Camlburg.action ()
                                and imm = arg2.imm.Camlburg.action ()
                                in
                                    
# 195 "arch/arm/armrec.mlb"
                                    ( cat ["["; reg; ", #"; imm; "]"] )
                                    
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
                                    
# 283 "arch/arm/armrec.mlb"
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
                                        
# 230 "arch/arm/armrec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            inf))))



# 84 "arch/arm/armrec.mlb"

  let const = function
      | RP.Bool _                 -> error "boolean found"
      | RP.Link(s,_,w)            -> conLink s w
      | RP.Diff _                 -> error "PIC not supported"
      | RP.Bits(b)                -> conBits b
      | RP.Late(s,w)              -> error (sprintf "late constant %s found" s)

  (* claude: the arithmetic ops arm.ml's Post.binop actually reaches
   * (add/sub/and - Post.dblop/wrdop/wrdrop are all Unsupported.xxx, no
   * mul/div/or/xor/shift yet) plus the arm_subcc/arm_<cond> pair
   * Post.subflags/bc_guard build for a conditional branch, and %lobits
   * (Post.lostore's only producer, a free no-op onto a narrow-width
   * store, same reasoning as mipsrec.mlb's identical case). *)
  let rec exp = function
      | RP.Const(k)               -> const k
      | RP.Fetch(l,w)             -> conFetch (loc l) w
      | RP.App(("add", [w]), [x; y])  -> conAdd (exp x) (exp y)
      | RP.App(("sub", [w]), [x; y])  -> conSub (exp x) (exp y)
      | RP.App(("and", [w]), [x; y])  -> conAnd (exp x) (exp y)
      | RP.App(("lobits", [_;_]), [x]) -> exp x
      | RP.App((("arm_subcc"), [w]), [x; y]) -> conSubcc (exp x) (exp y)
      | RP.App((("arm_eq"|"arm_ne"|"arm_lt"|"arm_le"|"arm_gt"|"arm_ge"
                |"arm_ls"|"arm_hi"|"arm_vs"|"arm_vc") as op, [32]), [c]) ->
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
        when i = SS.indices.SS.pc   -> conGoto (exp r)
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
