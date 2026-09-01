
# 1 "arch/m68k/m68krec.mlb"

  (* claude: no upstream m68krec.nw exists to port (see m68kregs.ml's header
   * comment: there is no upstream qc-- m68k code at all, and no TODO/
   * staging either). This grammar is written from scratch, modeled on
   * arch/arm/armrec.mlb's shape (%head/%tail structure, exp/loc/effect/
   * guarded dispatch, the opaque cc-register comparison family) crossed
   * with arch/x86/x86rec.mlb's call/return shape (m68k's "jsr"/"rts" push/
   * pop the return address on the stack in hardware, like x86's "call"/
   * "ret" - see m68k.ml's header comment). Only the subset of m68k actually
   * needed by demos/hello_m68k.c-- (and, incidentally, the tiger runtime's
   * simple integer/pointer code) is covered: immediate/symbol
   * materialization into a data register, register-to-register move,
   * frame/stack-relative addressing (base must be a6/a7 - real m68k
   * hardware requires an *address* register as a memory-addressing base,
   * never a data register, see m68kregs.ml's header comment on the
   * flattened register space), jsr/rts, and the two-address add/sub/and/
   * mul/quot family (an explicit "move.l srcdst,dst" is always emitted
   * before the op itself, unconditionally - see that rule's own comment
   * for why an equality guard doesn't work and isn't needed either).
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

  (* claude: signed decimal, not unsigned - same reasoning as
   * armrec.mlb's const32: a negative displacement/immediate must print as
   * e.g. "-4", not "4294967292", or m68k-linux-gnu-as rejects/misreads it. *)
  let const32 b =
      assert (Bits.width b = 32);
      Nativeint.to_string (Bits.S.to_native b)

  let const_eq_int b n =
      Bits.width b = 32 && Nativeint.to_int (Bits.S.to_native b) = n

  let cat     = String.concat ""

  let reg n   = M68kregs.regname n

  let m68k_bcond = function
      | "m68k_eq" -> "beq"
      | "m68k_ne" -> "bne"
      | "m68k_lt" -> "blt"
      | "m68k_le" -> "ble"
      | "m68k_gt" -> "bgt"
      | "m68k_ge" -> "bge"
      | "m68k_ls" -> "bls"
      | "m68k_hi" -> "bhi"
      | op        -> error (sprintf "not an m68k condition: %s" op)


# 000 "/dev/stdout"


type
    (
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
        _Zx3: ( 't51 ) Camlburg.nt;
        _Sx1: ( 't50 ) Camlburg.nt;
        _Subcc18: ( 't49 ) Camlburg.nt;
        _Subcc17: ( 't48 ) Camlburg.nt;
        _Sub22: ( 't47 ) Camlburg.nt;
        _Sub21: ( 't46 ) Camlburg.nt;
        _Store16: ( 't45 ) Camlburg.nt;
        _Store14: ( 't44 ) Camlburg.nt;
        _Store11: ( 't43 ) Camlburg.nt;
        _Store10: ( 't42 ) Camlburg.nt;
        _Quot28: ( 't41 ) Camlburg.nt;
        _Quot27: ( 't40 ) Camlburg.nt;
        _Par9: ( 't39 ) Camlburg.nt;
        _Mul26: ( 't38 ) Camlburg.nt;
        _Mul25: ( 't37 ) Camlburg.nt;
        _Mem5: ( 't36 ) Camlburg.nt;
        _Goto8: ( 't35 ) Camlburg.nt;
        _Goto13: ( 't34 ) Camlburg.nt;
        _Goto12: ( 't33 ) Camlburg.nt;
        _Fetch7: ( 't32 ) Camlburg.nt;
        _Fetch6: ( 't31 ) Camlburg.nt;
        _Fetch4: ( 't30 ) Camlburg.nt;
        _Fetch2: ( 't29 ) Camlburg.nt;
        _Fetch15: ( 't28 ) Camlburg.nt;
        _And24: ( 't27 ) Camlburg.nt;
        _And23: ( 't26 ) Camlburg.nt;
        _Add20: ( 't25 ) Camlburg.nt;
        _Add19: ( 't24 ) Camlburg.nt;
        symbol: ( 't23 ) Camlburg.nt;
        spinc: ( 't22 ) Camlburg.nt;
        spdec: ( 't21 ) Camlburg.nt;
        regl: ( 't20 ) Camlburg.nt;
        reg: ( 't19 ) Camlburg.nt;
        r: ( 't18 ) Camlburg.nt;
        pcv: ( 't17 ) Camlburg.nt;
        pcl: ( 't16 ) Camlburg.nt;
        pc: ( 't15 ) Camlburg.nt;
        minus_four: ( 't14 ) Camlburg.nt;
        meml: ( 't13 ) Camlburg.nt;
        mem: ( 't12 ) Camlburg.nt;
        limm: ( 't11 ) Camlburg.nt;
        inst: ( 't10 ) Camlburg.nt;
        imm: ( 't9 ) Camlburg.nt;
        four: ( 't8 ) Camlburg.nt;
        const: ( 't7 ) Camlburg.nt;
        cond: ( 't6 ) Camlburg.nt;
        ccval: ( 't5 ) Camlburg.nt;
        ccl: ( 't4 ) Camlburg.nt;
        areg: ( 't3 ) Camlburg.nt;
        any: ( 't2 ) Camlburg.nt;
        addr: ( 't1 ) Camlburg.nt;
        abase: ( 't0 ) Camlburg.nt
    }

let rec
inf =
    {abase = (Camlburg.infinity)
    ;addr = (Camlburg.infinity)
    ;any = (Camlburg.infinity)
    ;areg = (Camlburg.infinity)
    ;ccl = (Camlburg.infinity)
    ;ccval = (Camlburg.infinity)
    ;cond = (Camlburg.infinity)
    ;const = (Camlburg.infinity)
    ;four = (Camlburg.infinity)
    ;imm = (Camlburg.infinity)
    ;inst = (Camlburg.infinity)
    ;limm = (Camlburg.infinity)
    ;mem = (Camlburg.infinity)
    ;meml = (Camlburg.infinity)
    ;minus_four = (Camlburg.infinity)
    ;pc = (Camlburg.infinity)
    ;pcl = (Camlburg.infinity)
    ;pcv = (Camlburg.infinity)
    ;r = (Camlburg.infinity)
    ;reg = (Camlburg.infinity)
    ;regl = (Camlburg.infinity)
    ;spdec = (Camlburg.infinity)
    ;spinc = (Camlburg.infinity)
    ;symbol = (Camlburg.infinity)
    ;_Add19 = (Camlburg.infinity)
    ;_Add20 = (Camlburg.infinity)
    ;_And23 = (Camlburg.infinity)
    ;_And24 = (Camlburg.infinity)
    ;_Fetch15 = (Camlburg.infinity)
    ;_Fetch2 = (Camlburg.infinity)
    ;_Fetch4 = (Camlburg.infinity)
    ;_Fetch6 = (Camlburg.infinity)
    ;_Fetch7 = (Camlburg.infinity)
    ;_Goto12 = (Camlburg.infinity)
    ;_Goto13 = (Camlburg.infinity)
    ;_Goto8 = (Camlburg.infinity)
    ;_Mem5 = (Camlburg.infinity)
    ;_Mul25 = (Camlburg.infinity)
    ;_Mul26 = (Camlburg.infinity)
    ;_Par9 = (Camlburg.infinity)
    ;_Quot27 = (Camlburg.infinity)
    ;_Quot28 = (Camlburg.infinity)
    ;_Store10 = (Camlburg.infinity)
    ;_Store11 = (Camlburg.infinity)
    ;_Store14 = (Camlburg.infinity)
    ;_Store16 = (Camlburg.infinity)
    ;_Sub21 = (Camlburg.infinity)
    ;_Sub22 = (Camlburg.infinity)
    ;_Subcc17 = (Camlburg.infinity)
    ;_Subcc18 = (Camlburg.infinity)
    ;_Sx1 = (Camlburg.infinity)
    ;_Zx3 = (Camlburg.infinity)
    }


let rec
update_abase =
    fun nt x ->
        if nt.Camlburg.cost >= x.abase.Camlburg.cost then
            x
        else
            { x with abase = (nt) }
and update_addr =
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
                                
# 274 "arch/m68k/m68krec.mlb"
                                ( cat ["<";any;">"] )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with any = (nt) }
and update_areg =
    fun nt x ->
        if nt.Camlburg.cost >= x.areg.Camlburg.cost then
            x
        else
            (fun x ->
                (update_addr
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let areg = x.areg.Camlburg.action ()
                            in
                                
# 165 "arch/m68k/m68krec.mlb"
                                ( cat ["("; areg; ")"] )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with areg = (nt) }
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
                                
# 169 "arch/m68k/m68krec.mlb"
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
                                    
# 172 "arch/m68k/m68krec.mlb"
                                    ( const  )
                                    
# 000 "/dev/stdout"
)
                        })
                        x)
                    { x with const = (nt) })
and update_four =
    fun nt x ->
        if nt.Camlburg.cost >= x.four.Camlburg.cost then
            x
        else
            { x with four = (nt) }
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
and update_minus_four =
    fun nt x ->
        if nt.Camlburg.cost >= x.minus_four.Camlburg.cost then
            x
        else
            { x with minus_four = (nt) }
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
and update_pcv =
    fun nt x ->
        if nt.Camlburg.cost >= x.pcv.Camlburg.cost then
            x
        else
            { x with pcv = (nt) }
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
                                
# 147 "arch/m68k/m68krec.mlb"
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
            { x with reg = (nt) }
and update_regl =
    fun nt x ->
        if nt.Camlburg.cost >= x.regl.Camlburg.cost then
            x
        else
            { x with regl = (nt) }
and update_spdec =
    fun nt x ->
        if nt.Camlburg.cost >= x.spdec.Camlburg.cost then
            x
        else
            { x with spdec = (nt) }
and update_spinc =
    fun nt x ->
        if nt.Camlburg.cost >= x.spinc.Camlburg.cost then
            x
        else
            { x with spinc = (nt) }
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
                                
# 173 "arch/m68k/m68krec.mlb"
                                ( symbol )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with symbol = (nt) }
and update__Add19 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add19.Camlburg.cost then
            x
        else
            { x with _Add19 = (nt) }
and update__Add20 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add20.Camlburg.cost then
            x
        else
            { x with _Add20 = (nt) }
and update__And23 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And23.Camlburg.cost then
            x
        else
            { x with _And23 = (nt) }
and update__And24 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And24.Camlburg.cost then
            x
        else
            { x with _And24 = (nt) }
and update__Fetch15 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch15.Camlburg.cost then
            x
        else
            { x with _Fetch15 = (nt) }
and update__Fetch2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch2.Camlburg.cost then
            x
        else
            { x with _Fetch2 = (nt) }
and update__Fetch4 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch4.Camlburg.cost then
            x
        else
            { x with _Fetch4 = (nt) }
and update__Fetch6 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch6.Camlburg.cost then
            x
        else
            { x with _Fetch6 = (nt) }
and update__Fetch7 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch7.Camlburg.cost then
            x
        else
            { x with _Fetch7 = (nt) }
and update__Goto12 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto12.Camlburg.cost then
            x
        else
            { x with _Goto12 = (nt) }
and update__Goto13 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto13.Camlburg.cost then
            x
        else
            { x with _Goto13 = (nt) }
and update__Goto8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto8.Camlburg.cost then
            x
        else
            { x with _Goto8 = (nt) }
and update__Mem5 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mem5.Camlburg.cost then
            x
        else
            { x with _Mem5 = (nt) }
and update__Mul25 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul25.Camlburg.cost then
            x
        else
            { x with _Mul25 = (nt) }
and update__Mul26 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul26.Camlburg.cost then
            x
        else
            { x with _Mul26 = (nt) }
and update__Par9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Par9.Camlburg.cost then
            x
        else
            { x with _Par9 = (nt) }
and update__Quot27 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Quot27.Camlburg.cost then
            x
        else
            { x with _Quot27 = (nt) }
and update__Quot28 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Quot28.Camlburg.cost then
            x
        else
            { x with _Quot28 = (nt) }
and update__Store10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store10.Camlburg.cost then
            x
        else
            { x with _Store10 = (nt) }
and update__Store11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store11.Camlburg.cost then
            x
        else
            { x with _Store11 = (nt) }
and update__Store14 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store14.Camlburg.cost then
            x
        else
            { x with _Store14 = (nt) }
and update__Store16 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store16.Camlburg.cost then
            x
        else
            { x with _Store16 = (nt) }
and update__Sub21 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub21.Camlburg.cost then
            x
        else
            { x with _Sub21 = (nt) }
and update__Sub22 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub22.Camlburg.cost then
            x
        else
            { x with _Sub22 = (nt) }
and update__Subcc17 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc17.Camlburg.cost then
            x
        else
            { x with _Subcc17 = (nt) }
and update__Subcc18 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subcc18.Camlburg.cost then
            x
        else
            { x with _Subcc18 = (nt) }
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
                            
# 292 "arch/m68k/m68krec.mlb"
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
                    
# 276 "arch/m68k/m68krec.mlb"
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
                            
# 291 "arch/m68k/m68krec.mlb"
                            ( cat [ "Sx(";any;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conSubcc =
    fun arg1 arg2 ->
        (update__Subcc17
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Subcc18
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
                                
# 289 "arch/m68k/m68krec.mlb"
                                ( cat [ "Subcc(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conSub =
    fun arg1 arg2 ->
        (update__Sub21
            {Camlburg.cost =
                (arg1._Fetch6.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch6.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        let srcdst = _v1 in (srcdst ,y))
            })
            ((update__Sub22
                {Camlburg.cost =
                    (arg1._Fetch6.Camlburg.cost + arg2.imm.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch6.Camlburg.action ()
                        and y = arg2.imm.Camlburg.action ()
                        in
                            let srcdst = _v1 in (srcdst ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 285 "arch/m68k/m68krec.mlb"
                                ( cat [ "Sub(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conStore =
    fun arg1 arg2 arg3 ->
        (update__Store10
            {Camlburg.cost =
                (arg1.meml.Camlburg.cost + arg2.pcv.Camlburg.cost
                +
                (Camlburg.matches 32) arg3)
            ;Camlburg.action =
                (fun () ->
                    let meml = arg1.meml.Camlburg.action ()
                    and pcv = arg2.pcv.Camlburg.action ()
                    in
                        (meml ,pcv))
            })
            ((update__Store11
                {Camlburg.cost =
                    (arg1.abase.Camlburg.cost + arg2.spdec.Camlburg.cost
                    +
                    (Camlburg.matches 32) arg3)
                ;Camlburg.action =
                    (fun () ->
                        let abase = arg1.abase.Camlburg.action ()
                        and spdec = arg2.spdec.Camlburg.action ()
                        in
                            (abase ,spdec))
                })
                ((update__Store14
                    {Camlburg.cost =
                        (arg1.abase.Camlburg.cost + arg2.spinc.Camlburg.cost
                        +
                        (Camlburg.matches 32) arg3)
                    ;Camlburg.action =
                        (fun () ->
                            let abase = arg1.abase.Camlburg.action ()
                            and spinc = arg2.spinc.Camlburg.action ()
                            in
                                (abase ,spinc))
                    })
                    ((update__Store16
                        {Camlburg.cost =
                            (arg1.abase.Camlburg.cost
                            +
                            arg2.reg.Camlburg.cost
                            +
                            (Camlburg.matches 32) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let abase = arg1.abase.Camlburg.action ()
                                and nsp = arg2.reg.Camlburg.action ()
                                in
                                    (abase ,nsp))
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
                                        
# 299 "arch/m68k/m68krec.mlb"
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
                                            let
                                                regl =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                limm =
                                                arg2.limm.Camlburg.action ()
                                            in
                                                
# 177 "arch/m68k/m68krec.mlb"
                                                ( cat ["move.l #"; limm; ","; regl] )
                                                
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
                                            let
                                                regl =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                mem =
                                                arg2.mem.Camlburg.action ()
                                            in
                                                
# 180 "arch/m68k/m68krec.mlb"
                                                ( cat ["move.l "; mem; ","; regl] )
                                                
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Sx1.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                regl =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Sx1.Camlburg.action ()
                                            and w = arg3
                                            in
                                                let (mem, x) = _v1
                                                in
                                                    
# 183 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; mem; ","; regl; " | sign-extend TODO"] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Zx3.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                regl =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Zx3.Camlburg.action ()
                                            and w = arg3
                                            in
                                                let (mem, x) = _v1
                                                in
                                                    
# 186 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; mem; ","; regl; " | zero-extend TODO"] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.meml.Camlburg.cost
                                        +
                                        arg2.reg.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                meml =
                                                arg1.meml.Camlburg.action ()
                                            and
                                                reg =
                                                arg2.reg.Camlburg.action ()
                                            and w = arg3
                                            in
                                                
# 189 "arch/m68k/m68krec.mlb"
                                                ( cat ["move.l "; reg; ","; meml] )
                                                
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
                                            let
                                                regl =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                reg =
                                                arg2.reg.Camlburg.action ()
                                            in
                                                
# 192 "arch/m68k/m68krec.mlb"
                                                ( cat ["move.l "; reg; ","; regl] )
                                                
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Fetch4.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Fetch4.Camlburg.action
                                                    ()
                                            in
                                                let base = _v1
                                                in
                                                    
# 196 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; base; ",%a0\n\tmove.l (%a0),"; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Mem5.Camlburg.cost
                                        +
                                        arg2.reg.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Mem5.Camlburg.action ()
                                            and
                                                src =
                                                arg2.reg.Camlburg.action ()
                                            in
                                                let base = _v1
                                                in
                                                    
# 199 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; base; ",%a0\n\tmove.l "; src; ",(%a0)"] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.ccl.Camlburg.cost
                                        +
                                        arg2._Subcc17.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                ccl =
                                                arg1.ccl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Subcc17.Camlburg.action
                                                    ()
                                            in
                                                let (x, y) = _v1
                                                in
                                                    
# 232 "arch/m68k/m68krec.mlb"
                                                    ( cat ["cmp.l "; y; ","; x] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.ccl.Camlburg.cost
                                        +
                                        arg2._Subcc18.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                ccl =
                                                arg1.ccl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Subcc18.Camlburg.action
                                                    ()
                                            in
                                                let (x, y) = _v1
                                                in
                                                    
# 234 "arch/m68k/m68krec.mlb"
                                                    ( cat ["cmp.l #"; y; ","; x] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Add19.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Add19.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 242 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tadd.l "; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Add20.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Add20.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 245 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tadd.l #"; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Sub21.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Sub21.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 248 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tsub.l "; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Sub22.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Sub22.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 251 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tsub.l #"; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._And23.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._And23.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 254 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tand.l "; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._And24.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._And24.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 257 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tand.l #"; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Mul25.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Mul25.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 261 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tmuls.l "; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Mul26.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Mul26.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 264 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tmuls.l #"; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Quot27.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Quot27.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 267 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tdivs.l "; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Quot28.Camlburg.cost
                                        +
                                        (Camlburg.matches 32) arg3)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dst =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Quot28.Camlburg.action
                                                    ()
                                            in
                                                let (srcdst, y) = _v1
                                                in
                                                    
# 270 "arch/m68k/m68krec.mlb"
                                                    ( cat ["move.l "; srcdst; ","; dst; "\n\tdivs.l #"; y; ","; dst] )
                                                    
# 000 "/dev/stdout"
)
                                    }]))
                                inf)))))
and conReg =
    fun arg1 arg2 ->
        (update_abase
            {Camlburg.cost =
                (let n = arg2
                in
                    
# 154 "arch/m68k/m68krec.mlb"
                    ( guard (n = M68kregs.fp_ix || n = M68kregs.sp_ix) )
                    
# 000 "/dev/stdout"

                +
                (Camlburg.matches 'r') arg1)
            ;Camlburg.action =
                (fun () ->
                    let n = arg2
                    in
                        
# 155 "arch/m68k/m68krec.mlb"
                        ( reg n )
                        
# 000 "/dev/stdout"
)
            })
            ((update_any
                {Camlburg.cost = (0)
                ;Camlburg.action =
                    (fun () ->
                        let char = arg1
                        and n = arg2
                        in
                            
# 296 "arch/m68k/m68krec.mlb"
                            ( cat [ "Reg('";Char.escaped char;"',"; string_of_int n;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                ((update_ccl
                    {Camlburg.cost =
                        ((Camlburg.matches 'c') arg1
                        +
                        (Camlburg.matches 2) arg2)
                    ;Camlburg.action =
                        (fun () ->
                            
# 151 "arch/m68k/m68krec.mlb"
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
                                
# 150 "arch/m68k/m68krec.mlb"
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
                                        
# 146 "arch/m68k/m68krec.mlb"
                                        ( n )
                                        
# 000 "/dev/stdout"
)
                            })
                            inf))))
and conQuot =
    fun arg1 arg2 ->
        (update__Quot27
            {Camlburg.cost =
                (arg1._Fetch6.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch6.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        let srcdst = _v1 in (srcdst ,y))
            })
            ((update__Quot28
                {Camlburg.cost =
                    (arg1._Fetch6.Camlburg.cost + arg2.imm.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch6.Camlburg.action ()
                        and y = arg2.imm.Camlburg.action ()
                        in
                            let srcdst = _v1 in (srcdst ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 288 "arch/m68k/m68krec.mlb"
                                ( cat [ "Quot(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conPar =
    fun arg1 arg2 ->
        (update__Par9
            {Camlburg.cost =
                (arg1._Store10.Camlburg.cost + arg2._Store11.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Store10.Camlburg.action ()
                    and _v2 = arg2._Store11.Camlburg.action ()
                    in
                        let (abase, spdec) = _v2
                        in
                            let (meml, pcv) = _v1
                            in
                                (meml ,pcv ,abase ,spdec))
            })
            ((update_any
                {Camlburg.cost =
                    (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let l = arg1.any.Camlburg.action ()
                        and r = arg2.any.Camlburg.action ()
                        in
                            
# 304 "arch/m68k/m68krec.mlb"
                            ( cat [ "Par(";l;",";r;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                ((update_inst
                    (Camlburg.choice
                        [{Camlburg.cost =
                            (arg1._Goto8.Camlburg.cost
                            +
                            arg2._Par9.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto8.Camlburg.action ()
                                and _v2 = arg2._Par9.Camlburg.action ()
                                in
                                    let (meml, pcv, abase, spdec) = _v2
                                    in
                                        let symbol = _v1
                                        in
                                            
# 216 "arch/m68k/m68krec.mlb"
                                            ( cat ["jsr "; symbol] )
                                            
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Goto12.Camlburg.cost
                            +
                            arg2._Par9.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto12.Camlburg.action ()
                                and _v2 = arg2._Par9.Camlburg.action ()
                                in
                                    let (meml, pcv, abase, spdec) = _v2
                                    in
                                        let target = _v1
                                        in
                                            
# 219 "arch/m68k/m68krec.mlb"
                                            ( cat ["jsr ("; target; ")"] )
                                            
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Goto13.Camlburg.cost
                            +
                            arg2._Store14.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto13.Camlburg.action ()
                                and _v2 = arg2._Store14.Camlburg.action ()
                                in
                                    let (abase, spinc) = _v2
                                    in
                                        let meml = _v1
                                        in
                                            
# 224 "arch/m68k/m68krec.mlb"
                                            ( "rts" )
                                            
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1._Goto12.Camlburg.cost
                            +
                            arg2._Store16.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Goto12.Camlburg.action ()
                                and _v2 = arg2._Store16.Camlburg.action ()
                                in
                                    let (abase, nsp) = _v2
                                    in
                                        let target = _v1
                                        in
                                            
# 228 "arch/m68k/m68krec.mlb"
                                            ( cat ["move.l "; nsp; ",%a7\n\tjmp ("; target; ")"] )
                                            
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
                    
# 272 "arch/m68k/m68krec.mlb"
                    ( "nop" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update__Mul25
            {Camlburg.cost =
                (arg1._Fetch6.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch6.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        let srcdst = _v1 in (srcdst ,y))
            })
            ((update__Mul26
                {Camlburg.cost =
                    (arg1._Fetch6.Camlburg.cost + arg2.imm.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch6.Camlburg.action ()
                        and y = arg2.imm.Camlburg.action ()
                        in
                            let srcdst = _v1 in (srcdst ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 287 "arch/m68k/m68krec.mlb"
                                ( cat [ "Mul(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conMem =
    fun arg1 ->
        (update__Mem5
            {Camlburg.cost = (arg1._Fetch6.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch6.Camlburg.action ()
                    in
                        let base = _v1 in base)
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        in
                            
# 295 "arch/m68k/m68krec.mlb"
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
                                
# 161 "arch/m68k/m68krec.mlb"
                                ( addr )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conLobits =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 293 "arch/m68k/m68krec.mlb"
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
                        
# 278 "arch/m68k/m68krec.mlb"
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
                            
# 144 "arch/m68k/m68krec.mlb"
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
                        
# 279 "arch/m68k/m68krec.mlb"
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
                        
# 300 "arch/m68k/m68krec.mlb"
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
                        
# 303 "arch/m68k/m68krec.mlb"
                        ( cat [ "Guarded(";guard;",";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                {Camlburg.cost =
                    (arg1.cond.Camlburg.cost + arg2._Goto8.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let cond = arg1.cond.Camlburg.action ()
                        and _v1 = arg2._Goto8.Camlburg.action ()
                        in
                            let symbol = _v1
                            in
                                
# 238 "arch/m68k/m68krec.mlb"
                                ( cat [m68k_bcond cond; " "; symbol] )
                                
# 000 "/dev/stdout"
)
                })
                inf)
and conGoto =
    fun arg1 ->
        (update__Goto12
            {Camlburg.cost = (arg1.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let target = arg1.reg.Camlburg.action () in target)
            })
            ((update__Goto13
                {Camlburg.cost = (arg1._Fetch15.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch15.Camlburg.action ()
                        in
                            let meml = _v1 in meml)
                })
                ((update__Goto8
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
                                    
# 305 "arch/m68k/m68krec.mlb"
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
                                            
# 202 "arch/m68k/m68krec.mlb"
                                            ( cat ["bra "; symbol] )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost = (arg1.reg.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let reg = arg1.reg.Camlburg.action ()
                                        in
                                            
# 205 "arch/m68k/m68krec.mlb"
                                            ( cat ["jmp ("; reg; ")"] )
                                            
# 000 "/dev/stdout"
)
                                }]))
                            inf))))
and conFetch =
    fun arg1 arg2 ->
        (update__Fetch15
            {Camlburg.cost =
                (arg1.meml.Camlburg.cost + (Camlburg.matches 32) arg2)
            ;Camlburg.action =
                (fun () -> let meml = arg1.meml.Camlburg.action () in meml)
            })
            ((update__Fetch2
                {Camlburg.cost = (arg1.mem.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let mem = arg1.mem.Camlburg.action ()
                        and x = arg2
                        in
                            (mem ,x))
                })
                ((update__Fetch4
                    {Camlburg.cost =
                        (arg1._Mem5.Camlburg.cost
                        +
                        (Camlburg.matches 32) arg2)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Mem5.Camlburg.action ()
                            in
                                let base = _v1 in base)
                    })
                    ((update__Fetch6
                        {Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            (Camlburg.matches 32) arg2)
                        ;Camlburg.action =
                            (fun () ->
                                let base = arg1.regl.Camlburg.action ()
                                in
                                    base)
                        })
                        ((update__Fetch7
                            {Camlburg.cost =
                                (arg1.abase.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    let abase = arg1.abase.Camlburg.action ()
                                    in
                                        abase)
                            })
                            ((update_any
                                {Camlburg.cost = (arg1.any.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let any = arg1.any.Camlburg.action ()
                                        and w = arg2
                                        in
                                            
# 282 "arch/m68k/m68krec.mlb"
                                            ( cat [ "Fetch(";any;",";string_of_int w;")" ] )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_areg
                                    {Camlburg.cost =
                                        (arg1.abase.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                abase =
                                                arg1.abase.Camlburg.action ()
                                            and w = arg2
                                            in
                                                
# 156 "arch/m68k/m68krec.mlb"
                                                ( abase )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_ccval
                                        {Camlburg.cost =
                                            (arg1.ccl.Camlburg.cost
                                            +
                                            (Camlburg.matches 32) arg2)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    ccl =
                                                    arg1.ccl.Camlburg.action
                                                        ()
                                                in
                                                    
# 159 "arch/m68k/m68krec.mlb"
                                                    ( () )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        ((update_mem
                                            {Camlburg.cost =
                                                (arg1.meml.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        meml =
                                                        arg1.meml.Camlburg.action
                                                            ()
                                                    and w = arg2
                                                    in
                                                        
# 162 "arch/m68k/m68krec.mlb"
                                                        ( meml )
                                                        
# 000 "/dev/stdout"
)
                                            })
                                            ((update_pc
                                                {Camlburg.cost =
                                                    (arg1.pcl.Camlburg.cost
                                                    +
                                                    (Camlburg.matches 32)
                                                        arg2)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        let
                                                            pcl =
                                                            arg1.pcl.Camlburg.action
                                                                ()
                                                        in
                                                            
# 158 "arch/m68k/m68krec.mlb"
                                                            ( () )
                                                            
# 000 "/dev/stdout"
)
                                                })
                                                ((update_pcv
                                                    {Camlburg.cost =
                                                        (arg1.pcl.Camlburg.cost
                                                        +
                                                        (Camlburg.matches 32)
                                                            arg2)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                pcl =
                                                                arg1.pcl.Camlburg.action
                                                                    ()
                                                            in
                                                                
# 208 "arch/m68k/m68krec.mlb"
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
                                                                    arg1.regl.Camlburg.action
                                                                        ()
                                                                and w = arg2
                                                                in
                                                                    
# 148 "arch/m68k/m68krec.mlb"
                                                                    ( regl )
                                                                    
# 000 "/dev/stdout"
)
                                                        })
                                                        inf)))))))))))
and conFalse =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 277 "arch/m68k/m68krec.mlb"
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
                        
# 290 "arch/m68k/m68krec.mlb"
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
                            
# 236 "arch/m68k/m68krec.mlb"
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
                        
# 280 "arch/m68k/m68krec.mlb"
                        ( cat [ "Bits(b)" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost =
                    (let bits = arg1
                    in
                        
# 143 "arch/m68k/m68krec.mlb"
                        ( guard (Bits.width bits = 32)  )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let bits = arg1
                        in
                            
# 143 "arch/m68k/m68krec.mlb"
                            ( const32 bits )
                            
# 000 "/dev/stdout"
)
                })
                ((update_four
                    {Camlburg.cost =
                        (let bits = arg1
                        in
                            
# 211 "arch/m68k/m68krec.mlb"
                            ( guard (const_eq_int bits 4) )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let bits = arg1
                            in
                                
# 211 "arch/m68k/m68krec.mlb"
                                ( () )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_minus_four
                        {Camlburg.cost =
                            (let bits = arg1
                            in
                                
# 212 "arch/m68k/m68krec.mlb"
                                ( guard (const_eq_int bits (-4)) )
                                
# 000 "/dev/stdout"
)
                        ;Camlburg.action =
                            (fun () ->
                                let bits = arg1
                                in
                                    
# 212 "arch/m68k/m68krec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conAnd =
    fun arg1 arg2 ->
        (update__And23
            {Camlburg.cost =
                (arg1._Fetch6.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch6.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        let srcdst = _v1 in (srcdst ,y))
            })
            ((update__And24
                {Camlburg.cost =
                    (arg1._Fetch6.Camlburg.cost + arg2.imm.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch6.Camlburg.action ()
                        and y = arg2.imm.Camlburg.action ()
                        in
                            let srcdst = _v1 in (srcdst ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 286 "arch/m68k/m68krec.mlb"
                                ( cat [ "And(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAdd =
    fun arg1 arg2 ->
        (update__Add19
            {Camlburg.cost =
                (arg1._Fetch6.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch6.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        let srcdst = _v1 in (srcdst ,y))
            })
            ((update__Add20
                {Camlburg.cost =
                    (arg1._Fetch6.Camlburg.cost + arg2.imm.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch6.Camlburg.action ()
                        and y = arg2.imm.Camlburg.action ()
                        in
                            let srcdst = _v1 in (srcdst ,y))
                })
                ((update_addr
                    (Camlburg.choice
                        [{Camlburg.cost =
                            (arg1.imm.Camlburg.cost
                            +
                            arg2.areg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let imm = arg1.imm.Camlburg.action ()
                                and areg = arg2.areg.Camlburg.action ()
                                in
                                    
# 166 "arch/m68k/m68krec.mlb"
                                    ( cat [imm; "("; areg; ")"] )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.areg.Camlburg.cost
                            +
                            arg2.imm.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let areg = arg1.areg.Camlburg.action ()
                                and imm = arg2.imm.Camlburg.action ()
                                in
                                    
# 167 "arch/m68k/m68krec.mlb"
                                    ( cat [imm; "("; areg; ")"] )
                                    
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
                                    
# 284 "arch/m68k/m68krec.mlb"
                                    ( cat [ "Add(";x;", ";y;")" ] )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_spdec
                            {Camlburg.cost =
                                (arg1._Fetch7.Camlburg.cost
                                +
                                arg2.minus_four.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let _v1 = arg1._Fetch7.Camlburg.action ()
                                    and
                                        minus_four =
                                        arg2.minus_four.Camlburg.action ()
                                    in
                                        let abase = _v1
                                        in
                                            
# 213 "arch/m68k/m68krec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                            })
                            ((update_spinc
                                {Camlburg.cost =
                                    (arg1._Fetch7.Camlburg.cost
                                    +
                                    arg2.four.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            _v1 =
                                            arg1._Fetch7.Camlburg.action ()
                                        and
                                            four =
                                            arg2.four.Camlburg.action ()
                                        in
                                            let abase = _v1
                                            in
                                                
# 222 "arch/m68k/m68krec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                })
                                inf)))))



# 59 "arch/m68k/m68krec.mlb"

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
      | RP.App((("m68k_subcc"), [w]), [x; y]) -> conSubcc (exp x) (exp y)
      | RP.App((("m68k_eq"|"m68k_ne"|"m68k_lt"|"m68k_le"|"m68k_gt"|"m68k_ge"
                |"m68k_ls"|"m68k_hi") as op, [32]), [c]) ->
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
