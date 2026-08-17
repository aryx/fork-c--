
# 1 "riscv32rec.mlb"

  (* claude: no upstream riscv32rec.nw exists to port - qc-- predates
   * RISC-V (see riscv32.ml's header comment). Modeled on
   * arch/mips/mipsrec.mlb's shape (%head/%tail structure, exp/loc/effect/
   * guarded dispatch, direct register-vs-register branch guard - RISC-V's
   * beq/bne/blt/bge/bltu/bgeu, like MIPS's beq/bne/..., compare two
   * registers directly, no condition-code register) crossed with
   * arch/arm/armrec.mlb's immediate-materialization discipline (RISC-V's
   * addi/andi only take a 12-bit signed immediate, narrower than either
   * MIPS's 16-bit or a full 32-bit constant - out-of-range immediates fall
   * back to a guarded "li t0, imm" + register-register instruction, the
   * same [1]-cost fallback pattern arch/alpha/alpharec.mlb's imm8/lda pair
   * and armrec.mlb's "ldr ip,=y" rules use). Only the subset of RISC-V
   * actually needed by demos/hello_riscv32.c-- is covered: base+offset
   * addressing, add/sub/and/mul, the ten eq/ne/lt/le/gt/ge/ltu/leu/gtu/geu
   * comparisons, plain direct/indirect calls and branches. No or/xor/
   * shift/div, no compressed (C-extension) instructions ever emitted
   * (a.opt.norelax below pins the assembler to fixed-width encodings so
   * our own hand-computed ra_offset=4 stays valid).
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

  (* claude: signed decimal, not unsigned - riscv32-linux-gnu-as's addi/lw/
   * sw 12-bit signed-immediate field (and li/la, which do not care either
   * way) rejects a large unsigned decimal the same way mipsel-linux-gnu-
   * as's imm16 does (see mipsrec.mlb's own const32 comment) - Bits.S, not
   * Bits.U. *)
  let const32 b =
      assert (Bits.width b = 32);
      Int64.to_string (Bits.S.to_int64 b)

  (* claude: RISC-V addi/andi/slti/... only encode a 12-bit signed immediate
   * (-2048..2047) - out-of-range values need the "li t0, imm" fallback (see
   * the guarded imm12/imm rule pairs below). *)
  let fits_imm12 b =
    let n = Bits.S.to_int64 b in
    Int64.compare n (-2048L) >= 0 && Int64.compare n 2047L <= 0

  let cat     = String.concat ""
  let sprintf = Printf.sprintf

  let reg n   = "x" ^ string_of_int n

  (* claude: RISC-V load/store width suffixes - b/h/w, and (like MIPS,
   * unlike ARM) a zero-extending sub-word load just appends "u" to the
   * same suffix (lbu/lhu) rather than using a distinct mnemonic. RV32 has
   * no "d" (ld/sd) form - that is riscv64rec.mlb's own addition, see
   * riscv32.ml's header comment. *)
  let suffix = function
      | 8  -> "b"
      | 16 -> "h"
      | 32 -> "w"
      | w  -> error (sprintf "not a RISC-V width: %d" w)

  let zx    = "u"
  let sx    = ""
  let width = string_of_int


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
        _Zx3: ( 't40 ) Camlburg.nt;
        _Sx1: ( 't39 ) Camlburg.nt;
        _Sub13: ( 't38 ) Camlburg.nt;
        _Sub12: ( 't37 ) Camlburg.nt;
        _Store7: ( 't36 ) Camlburg.nt;
        _Store5: ( 't35 ) Camlburg.nt;
        _Quot20: ( 't34 ) Camlburg.nt;
        _Quot19: ( 't33 ) Camlburg.nt;
        _Mul18: ( 't32 ) Camlburg.nt;
        _Mul17: ( 't31 ) Camlburg.nt;
        _Goto8: ( 't30 ) Camlburg.nt;
        _Goto6: ( 't29 ) Camlburg.nt;
        _Goto4: ( 't28 ) Camlburg.nt;
        _Fetch2: ( 't27 ) Camlburg.nt;
        _And16: ( 't26 ) Camlburg.nt;
        _And15: ( 't25 ) Camlburg.nt;
        _And14: ( 't24 ) Camlburg.nt;
        _Add9: ( 't23 ) Camlburg.nt;
        _Add11: ( 't22 ) Camlburg.nt;
        _Add10: ( 't21 ) Camlburg.nt;
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
        imm12: ( 't6 ) Camlburg.nt;
        imm: ( 't5 ) Camlburg.nt;
        const: ( 't4 ) Camlburg.nt;
        cmpc: ( 't3 ) Camlburg.nt;
        cmp: ( 't2 ) Camlburg.nt;
        any: ( 't1 ) Camlburg.nt;
        addr: ( 't0 ) Camlburg.nt
    }

let rec
inf =
    {addr = (Camlburg.infinity)
    ;any = (Camlburg.infinity)
    ;cmp = (Camlburg.infinity)
    ;cmpc = (Camlburg.infinity)
    ;const = (Camlburg.infinity)
    ;imm = (Camlburg.infinity)
    ;imm12 = (Camlburg.infinity)
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
    ;_Add10 = (Camlburg.infinity)
    ;_Add11 = (Camlburg.infinity)
    ;_Add9 = (Camlburg.infinity)
    ;_And14 = (Camlburg.infinity)
    ;_And15 = (Camlburg.infinity)
    ;_And16 = (Camlburg.infinity)
    ;_Fetch2 = (Camlburg.infinity)
    ;_Goto4 = (Camlburg.infinity)
    ;_Goto6 = (Camlburg.infinity)
    ;_Goto8 = (Camlburg.infinity)
    ;_Mul17 = (Camlburg.infinity)
    ;_Mul18 = (Camlburg.infinity)
    ;_Quot19 = (Camlburg.infinity)
    ;_Quot20 = (Camlburg.infinity)
    ;_Store5 = (Camlburg.infinity)
    ;_Store7 = (Camlburg.infinity)
    ;_Sub12 = (Camlburg.infinity)
    ;_Sub13 = (Camlburg.infinity)
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
                                
# 280 "riscv32rec.mlb"
                                ( cat ["<";any;">"] )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with any = (nt) }
and update_cmp =
    fun nt x ->
        if nt.Camlburg.cost >= x.cmp.Camlburg.cost then
            x
        else
            { x with cmp = (nt) }
and update_cmpc =
    fun nt x ->
        if nt.Camlburg.cost >= x.cmpc.Camlburg.cost then
            x
        else
            { x with cmpc = (nt) }
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
                                
# 185 "riscv32rec.mlb"
                                ( const                   )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with const = (nt) }
and update_imm =
    fun nt x ->
        if nt.Camlburg.cost >= x.imm.Camlburg.cost then
            x
        else
            { x with imm = (nt) }
and update_imm12 =
    fun nt x ->
        if nt.Camlburg.cost >= x.imm12.Camlburg.cost then
            x
        else
            { x with imm12 = (nt) }
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
                                
# 165 "riscv32rec.mlb"
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
                                
# 180 "riscv32rec.mlb"
                                ( cat ["0("; reg; ")"] )
                                
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
                (update_addr
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let symbol = x.symbol.Camlburg.action ()
                            in
                                
# 183 "riscv32rec.mlb"
                                ( symbol                  )
                                
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
and update__Add9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add9.Camlburg.cost then
            x
        else
            { x with _Add9 = (nt) }
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
and update__And16 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And16.Camlburg.cost then
            x
        else
            { x with _And16 = (nt) }
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
and update__Goto8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto8.Camlburg.cost then
            x
        else
            { x with _Goto8 = (nt) }
and update__Mul17 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul17.Camlburg.cost then
            x
        else
            { x with _Mul17 = (nt) }
and update__Mul18 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul18.Camlburg.cost then
            x
        else
            { x with _Mul18 = (nt) }
and update__Quot19 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Quot19.Camlburg.cost then
            x
        else
            { x with _Quot19 = (nt) }
and update__Quot20 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Quot20.Camlburg.cost then
            x
        else
            { x with _Quot20 = (nt) }
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
                            
# 296 "riscv32rec.mlb"
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
                    
# 282 "riscv32rec.mlb"
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
                            
# 295 "riscv32rec.mlb"
                            ( cat [ "Sx(";any;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
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
                                
# 291 "riscv32rec.mlb"
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
                                
# 303 "riscv32rec.mlb"
                                ( cat [ "Store(";dst;",";src;",";width w;")" ] )
                                
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
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and
                                        symbol =
                                        arg2.symbol.Camlburg.action ()
                                    in
                                        
# 188 "riscv32rec.mlb"
                                        ( cat ["la"; " "; regl; ","; symbol] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.const.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and const = arg2.const.Camlburg.action ()
                                    in
                                        
# 191 "riscv32rec.mlb"
                                        ( cat ["li"; " "; regl; ","; const] )
                                        
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
                                        
# 194 "riscv32rec.mlb"
                                        ( cat ["l"; suffix 32; " "; regl; ","; mem] )
                                        
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
                                            
# 197 "riscv32rec.mlb"
                                            ( cat ["l"; suffix w; sx; " "; regl; ","; mem] )
                                            
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
                                            
# 200 "riscv32rec.mlb"
                                            ( cat ["l"; suffix w; zx; " "; regl; ","; mem] )
                                            
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
                                        
# 203 "riscv32rec.mlb"
                                        ( cat ["s"; suffix w; " "; reg; ","; meml] )
                                        
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
                                        
# 206 "riscv32rec.mlb"
                                        ( cat ["mv"; " "; regl; ","; reg] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add9.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add9.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 240 "riscv32rec.mlb"
                                            ( cat ["add"; " "; dst; ","; x; ","; y] )
                                            
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
                                            
# 243 "riscv32rec.mlb"
                                            ( cat ["addi"; " "; dst; ","; x; ","; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (1 + arg1.regl.Camlburg.cost
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
                                            
# 247 "riscv32rec.mlb"
                                            ( sprintf "li t0, %s\n\tadd %s, %s, t0" y dst x )
                                            
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
                                            
# 250 "riscv32rec.mlb"
                                            ( cat ["sub"; " "; dst; ","; x; ","; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (1 + arg1.regl.Camlburg.cost
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
                                            
# 253 "riscv32rec.mlb"
                                            ( sprintf "li t0, %s\n\tsub %s, %s, t0" y dst x )
                                            
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
                                            
# 256 "riscv32rec.mlb"
                                            ( cat ["and"; " "; dst; ","; x; ","; y] )
                                            
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
                                            
# 259 "riscv32rec.mlb"
                                            ( cat ["andi"; " "; dst; ","; x; ","; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (1 + arg1.regl.Camlburg.cost
                                +
                                arg2._And16.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._And16.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 262 "riscv32rec.mlb"
                                            ( sprintf "li t0, %s\n\tand %s, %s, t0" y dst x )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Mul17.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Mul17.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 266 "riscv32rec.mlb"
                                            ( cat ["mul"; " "; dst; ","; x; ","; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (1 + arg1.regl.Camlburg.cost
                                +
                                arg2._Mul18.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Mul18.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 269 "riscv32rec.mlb"
                                            ( sprintf "li t0, %s\n\tmul %s, %s, t0" y dst x )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Quot19.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Quot19.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 273 "riscv32rec.mlb"
                                            ( cat ["div"; " "; dst; ","; x; ","; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (1 + arg1.regl.Camlburg.cost
                                +
                                arg2._Quot20.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Quot20.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 276 "riscv32rec.mlb"
                                            ( sprintf "li t0, %s\n\tdiv %s, %s, t0" y dst x )
                                            
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
                        
# 300 "riscv32rec.mlb"
                        ( cat [ "Reg('";Char.escaped char;"',"; width n;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_pcl
                {Camlburg.cost =
                    ((Camlburg.matches 'c') arg1 + (Camlburg.matches 0) arg2)
                ;Camlburg.action =
                    (fun () ->
                        
# 168 "riscv32rec.mlb"
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
                                
# 164 "riscv32rec.mlb"
                                ( n )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_ral
                        {Camlburg.cost =
                            ((Camlburg.matches 'r') arg1
                            +
                            (Camlburg.matches 1) arg2)
                        ;Camlburg.action =
                            (fun () ->
                                
# 170 "riscv32rec.mlb"
                                ( () )
                                
# 000 "/dev/stdout"
)
                        })
                        ((update_spl
                            {Camlburg.cost =
                                ((Camlburg.matches 'r') arg1
                                +
                                (Camlburg.matches 2) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    
# 169 "riscv32rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                            })
                            inf))))
and conQuot =
    fun arg1 arg2 ->
        (update__Quot19
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Quot20
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
                                
# 294 "riscv32rec.mlb"
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
                        
# 310 "riscv32rec.mlb"
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
                                        
# 217 "riscv32rec.mlb"
                                        ( cat ["jal"; " "; "ra"; ","; symbol] )
                                        
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
                                        
# 220 "riscv32rec.mlb"
                                        ( cat ["jalr"; " "; "ra"; ","; target] )
                                        
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
                                        
# 224 "riscv32rec.mlb"
                                        ( sprintf "mv sp, %s\n\tjr %s" nsp target )
                                        
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
                    
# 278 "riscv32rec.mlb"
                    ( "nop" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update__Mul17
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Mul18
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
                                
# 293 "riscv32rec.mlb"
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
                        
# 299 "riscv32rec.mlb"
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
                            
# 176 "riscv32rec.mlb"
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
                        
# 297 "riscv32rec.mlb"
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
                        
# 284 "riscv32rec.mlb"
                        ( cat [ "Link(";x#mangled_text;",";width w;")" ] )
                        
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
                            
# 162 "riscv32rec.mlb"
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
                        
# 285 "riscv32rec.mlb"
                        ( cat [ "Late(";string;",";width w;")" ] )
                        
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
                        
# 304 "riscv32rec.mlb"
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
                        
# 307 "riscv32rec.mlb"
                        ( cat [ "Guarded(";guard;",";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1.cmp.Camlburg.cost + arg2._Goto8.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let cmp = arg1.cmp.Camlburg.action ()
                            and _v1 = arg2._Goto8.Camlburg.action ()
                            in
                                let addr = _v1
                                in
                                    
# 228 "riscv32rec.mlb"
                                    ( match cmp with
               | (op,x,y) -> cat ["b";op;" ";x;",";y;",";addr]
            )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (1 + arg1.cmpc.Camlburg.cost
                        +
                        arg2._Goto8.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let cmpc = arg1.cmpc.Camlburg.action ()
                            and _v1 = arg2._Goto8.Camlburg.action ()
                            in
                                let addr = _v1
                                in
                                    
# 235 "riscv32rec.mlb"
                                    ( match cmpc with
               | (op,x,y) -> sprintf "li t0, %s\n\tb%s %s, t0, %s" y op x addr
            )
                                    
# 000 "/dev/stdout"
)
                    }]))
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
                ((update__Goto8
                    {Camlburg.cost = (arg1.addr.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let addr = arg1.addr.Camlburg.action () in addr)
                    })
                    ((update_any
                        {Camlburg.cost = (arg1.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let any = arg1.any.Camlburg.action ()
                                in
                                    
# 311 "riscv32rec.mlb"
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
                                            
# 209 "riscv32rec.mlb"
                                            ( cat ["j"; " "; symbol] )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost = (arg1.reg.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let reg = arg1.reg.Camlburg.action ()
                                        in
                                            
# 212 "riscv32rec.mlb"
                                            ( cat ["jr"; " "; reg] )
                                            
# 000 "/dev/stdout"
)
                                }]))
                            inf))))
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
                            
# 288 "riscv32rec.mlb"
                            ( cat [ "Fetch(";any;",";width w;")" ] )
                            
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
                                
# 177 "riscv32rec.mlb"
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
                                    
# 172 "riscv32rec.mlb"
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
                                        
# 174 "riscv32rec.mlb"
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
                                            
# 166 "riscv32rec.mlb"
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
                                                arg1.spl.Camlburg.action ()
                                            in
                                                
# 173 "riscv32rec.mlb"
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
                    
# 283 "riscv32rec.mlb"
                    ( cat [ "False" ] )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conCmp =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost =
                (arg2.any.Camlburg.cost + arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let op = arg1
                    and x = arg2.any.Camlburg.action ()
                    and y = arg3.any.Camlburg.action ()
                    in
                        
# 309 "riscv32rec.mlb"
                        ( cat [ "Cmp(";op;",";x;",";y;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_cmp
                {Camlburg.cost =
                    (arg2.reg.Camlburg.cost + arg3.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let op = arg1
                        and x = arg2.reg.Camlburg.action ()
                        and y = arg3.reg.Camlburg.action ()
                        in
                            
# 226 "riscv32rec.mlb"
                            ( (op,x,y) )
                            
# 000 "/dev/stdout"
)
                })
                ((update_cmpc
                    {Camlburg.cost =
                        (arg2.reg.Camlburg.cost + arg3.const.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let op = arg1
                            and x = arg2.reg.Camlburg.action ()
                            and y = arg3.const.Camlburg.action ()
                            in
                                
# 233 "riscv32rec.mlb"
                                ( (op,x,y) )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conBits =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let bits = arg1
                    in
                        
# 286 "riscv32rec.mlb"
                        ( cat [ "Bits(b)" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost =
                    (let bits = arg1
                    in
                        
# 160 "riscv32rec.mlb"
                        ( guard (Bits.width bits = 32) )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let bits = arg1
                        in
                            
# 160 "riscv32rec.mlb"
                            ( const32 bits )
                            
# 000 "/dev/stdout"
)
                })
                ((update_imm12
                    {Camlburg.cost =
                        (let bits = arg1
                        in
                            
# 161 "riscv32rec.mlb"
                            ( guard (Bits.width bits = 32 && fits_imm12 bits) )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let bits = arg1
                            in
                                
# 161 "riscv32rec.mlb"
                                ( const32 bits )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
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
                    (arg1.reg.Camlburg.cost + arg2.imm12.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.imm12.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update__And16
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
                                    
# 292 "riscv32rec.mlb"
                                    ( cat [ "And(";x;", ";y;")" ] )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conAdd =
    fun arg1 arg2 ->
        (update__Add10
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.imm12.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.imm12.Camlburg.action ()
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
                ((update__Add9
                    {Camlburg.cost =
                        (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.reg.Camlburg.action ()
                            and y = arg2.reg.Camlburg.action ()
                            in
                                (x ,y))
                    })
                    ((update_addr
                        (Camlburg.choice
                            [{Camlburg.cost =
                                (arg1.imm12.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let imm12 = arg1.imm12.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    in
                                        
# 181 "riscv32rec.mlb"
                                        ( cat [imm12;"(";reg;")"] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.reg.Camlburg.cost
                                +
                                arg2.imm12.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let reg = arg1.reg.Camlburg.action ()
                                    and imm12 = arg2.imm12.Camlburg.action ()
                                    in
                                        
# 182 "riscv32rec.mlb"
                                        ( cat [imm12;"(";reg;")"] )
                                        
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
                                        
# 290 "riscv32rec.mlb"
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
                                        and
                                            const =
                                            arg2.const.Camlburg.action ()
                                        in
                                            
# 215 "riscv32rec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                })
                                inf)))))



# 70 "riscv32rec.mlb"

  let const = function
      | RP.Bool _                 -> error "boolean found"
      | RP.Link(s,_,w)            -> conLink s w
      | RP.Diff _                 -> error "PIC not supported"
      | RP.Bits(b)                -> conBits b
      | RP.Late(s,w)              -> error (sprintf "late constant %s found" s)

  let cmp =
      Strutil.from_list ["eq";"ge";"geu";"gt";"gtu";"le";"leu";"lt";"ltu";"ne"]

  let rec exp = function
      | RP.Const(k)               -> const k
      | RP.Fetch(l,w)             -> conFetch (loc l) w
      | RP.App(("add", [w]), [x; y])   -> conAdd (exp x) (exp y)
      | RP.App(("sub", [w]), [x; y])   -> conSub (exp x) (exp y)
      | RP.App(("and", [w]), [x; y])   -> conAnd (exp x) (exp y)
      | RP.App(("mul", [w]), [x; y])   -> conMul (exp x) (exp y)
      | RP.App(("quot", [w]), [x; y])  -> conQuot (exp x) (exp y)
      (* claude: %lobits only ever wraps a value immediately feeding a
       * narrow-width memory store (riscv32.ml's Post.lostore, its only
       * producer) - a RISC-V sb/sh/sw already truncates its source
       * register to the store's own width, so the narrowing is a free
       * no-op here, same reasoning as mipsrec.mlb's identical case. *)
      | RP.App(("lobits", [_;_]), [x]) -> exp x
      | RP.App((op, [w]), [x; y])
          when Strutil.Set.mem op cmp  -> conCmp op (exp x) (exp y)
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
