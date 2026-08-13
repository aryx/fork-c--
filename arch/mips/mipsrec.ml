
# 2 "mipsrec.mlb"
 
  (*s: head *)
  (* claude: =$= (string equality) lives in Nopoly, not the default Stdlib
   * polymorphic (=) - needed below by the "syscall" mangled-name guard. *)
  open Nopoly
  module RP = Rtl.Private
  module RU = Rtlutil
  module Up = Rtl.Up
  module Dn = Rtl.Dn
  module SS = Space.Standard32
  (*x: head *)
  exception Error of string
  let error msg = raise (Error msg)   
  let sprintf   = Printf.sprintf (* useful for formatting msg *)
  (*x: head *)
  let guard p = if p then 0 else Camlburg.inf_cost
  (*x: head *)
  (* claude: was Bits.U.to_native (unsigned) - mipsel-linux-gnu-as rejects
   * an unsigned 32-bit literal like "4294967264" as an addi/lw/sw 16-bit
   * signed-immediate operand ("operand out of range"), even though it is
   * bit-for-bit the same value as -32. GAS's imm16 range check looks at
   * the literal as written, it does not infer a negative interpretation
   * from a large unsigned decimal. Bits.S.to_native (signed) prints "-32"
   * instead, which addi/lw/sw's 16-bit field accepts directly, and li/la
   * (which the same const32 feeds too) don't care either way. *)
  let const32 b =
      assert (Bits.width b = 32);
      Nativeint.to_string (Bits.S.to_native b)

  let const64 b =
      assert (Bits.width b = 64);
      Int64.to_string (Bits.U.to_int64 b)     (* signed or unsigned? *)

  let cat     = String.concat ""
  let printf  = Printf.printf
  let sprintf = Printf.sprintf

  let reg n   = "$"  ^ string_of_int n
  let freg n  = "$f" ^ string_of_int n

  let suffix = function
      | 8  -> "b"      
      | 16 -> "h"
      | 32 -> "w"
      | w  -> error (sprintf "not a MIPS width: %d" w)

  let zx    = "u"  (* to construct op-code *) 
  let sx    = ""
  let width = string_of_int
  (*x: head *)
  let lo b =
      assert (Bits.width b = 64);
      Bits.Ops.lobits 32 b

  let b32 = Bits.U.of_int 32 64
  let hi b =
      assert (Bits.width b = 64);
      Bits.Ops.lobits 32 (Bits.Ops.shrl b b32)
  (*e: head *)


# 000 "/dev/stdout"


type
    (
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
        _Zx3: ( 't34 ) Camlburg.nt;
        _Sx1: ( 't33 ) Camlburg.nt;
        _Store7: ( 't32 ) Camlburg.nt;
        _Store5: ( 't31 ) Camlburg.nt;
        _S2D11: ( 't30 ) Camlburg.nt;
        _Goto6: ( 't29 ) Camlburg.nt;
        _Goto4: ( 't28 ) Camlburg.nt;
        _Fetch2: ( 't27 ) Camlburg.nt;
        _D2S10: ( 't26 ) Camlburg.nt;
        _Add9: ( 't25 ) Camlburg.nt;
        _Add8: ( 't24 ) Camlburg.nt;
        syscall: ( 't23 ) Camlburg.nt;
        symbol: ( 't22 ) Camlburg.nt;
        spl: ( 't21 ) Camlburg.nt;
        sp: ( 't20 ) Camlburg.nt;
        regl: ( 't19 ) Camlburg.nt;
        reg: ( 't18 ) Camlburg.nt;
        ral: ( 't17 ) Camlburg.nt;
        ra: ( 't16 ) Camlburg.nt;
        r: ( 't15 ) Camlburg.nt;
        pcl: ( 't14 ) Camlburg.nt;
        pc: ( 't13 ) Camlburg.nt;
        next: ( 't12 ) Camlburg.nt;
        meml: ( 't11 ) Camlburg.nt;
        mem: ( 't10 ) Camlburg.nt;
        inst: ( 't9 ) Camlburg.nt;
        imm: ( 't8 ) Camlburg.nt;
        fregl: ( 't7 ) Camlburg.nt;
        freg: ( 't6 ) Camlburg.nt;
        f: ( 't5 ) Camlburg.nt;
        const64: ( 't4 ) Camlburg.nt;
        const: ( 't3 ) Camlburg.nt;
        cmp: ( 't2 ) Camlburg.nt;
        any: ( 't1 ) Camlburg.nt;
        addr: ( 't0 ) Camlburg.nt
    }

let rec
inf =
    {addr = (Camlburg.infinity)
    ;any = (Camlburg.infinity)
    ;cmp = (Camlburg.infinity)
    ;const = (Camlburg.infinity)
    ;const64 = (Camlburg.infinity)
    ;f = (Camlburg.infinity)
    ;freg = (Camlburg.infinity)
    ;fregl = (Camlburg.infinity)
    ;imm = (Camlburg.infinity)
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
    ;syscall = (Camlburg.infinity)
    ;_Add8 = (Camlburg.infinity)
    ;_Add9 = (Camlburg.infinity)
    ;_D2S10 = (Camlburg.infinity)
    ;_Fetch2 = (Camlburg.infinity)
    ;_Goto4 = (Camlburg.infinity)
    ;_Goto6 = (Camlburg.infinity)
    ;_S2D11 = (Camlburg.infinity)
    ;_Store5 = (Camlburg.infinity)
    ;_Store7 = (Camlburg.infinity)
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
                                
# 274 "mipsrec.mlb"
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
                                
# 188 "mipsrec.mlb"
                                ( const                 )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with const = (nt) }
and update_const64 =
    fun nt x ->
        if nt.Camlburg.cost >= x.const64.Camlburg.cost then
            x
        else
            { x with const64 = (nt) }
and update_f =
    fun nt x ->
        if nt.Camlburg.cost >= x.f.Camlburg.cost then
            x
        else
            (fun x ->
                (update_fregl
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let f = x.f.Camlburg.action ()
                            in
                                
# 166 "mipsrec.mlb"
                                ( freg f )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with f = (nt) }
and update_freg =
    fun nt x ->
        if nt.Camlburg.cost >= x.freg.Camlburg.cost then
            x
        else
            { x with freg = (nt) }
and update_fregl =
    fun nt x ->
        if nt.Camlburg.cost >= x.fregl.Camlburg.cost then
            x
        else
            { x with fregl = (nt) }
and update_imm =
    fun nt x ->
        if nt.Camlburg.cost >= x.imm.Camlburg.cost then
            x
        else
            (fun x ->
                (update_addr
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let imm = x.imm.Camlburg.action ()
                            in
                                
# 183 "mipsrec.mlb"
                                ( imm                   )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with imm = (nt) }
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
                                
# 165 "mipsrec.mlb"
                                ( reg r  )
                                
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
                                
# 182 "mipsrec.mlb"
                                ( cat ["(";reg;")"]     )
                                
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
                                
# 186 "mipsrec.mlb"
                                ( symbol                )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                ((fun x ->
                    (update_imm
                        {Camlburg.cost = (nt.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let symbol = x.symbol.Camlburg.action ()
                                in
                                    
# 189 "mipsrec.mlb"
                                    ( symbol                )
                                    
# 000 "/dev/stdout"
)
                        })
                        x)
                    { x with symbol = (nt) })
and update_syscall =
    fun nt x ->
        if nt.Camlburg.cost >= x.syscall.Camlburg.cost then
            x
        else
            { x with syscall = (nt) }
and update__Add8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add8.Camlburg.cost then
            x
        else
            { x with _Add8 = (nt) }
and update__Add9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add9.Camlburg.cost then
            x
        else
            { x with _Add9 = (nt) }
and update__D2S10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._D2S10.Camlburg.cost then
            x
        else
            { x with _D2S10 = (nt) }
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
and update__S2D11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._S2D11.Camlburg.cost then
            x
        else
            { x with _S2D11 = (nt) }
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
                            
# 287 "mipsrec.mlb"
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
                    
# 276 "mipsrec.mlb"
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
                            
# 286 "mipsrec.mlb"
                            ( cat [ "Sx(";any;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conSub =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.any.Camlburg.action ()
                    and y = arg2.any.Camlburg.action ()
                    in
                        
# 285 "mipsrec.mlb"
                        ( cat [ "Sub(";x;", ";y;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            inf
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
                    (arg1.ral.Camlburg.cost + arg2.pc.Camlburg.cost
                    +
                    (Camlburg.matches 32) arg3)
                ;Camlburg.action =
                    (fun () ->
                        let ral = arg1.ral.Camlburg.action ()
                        and pc = arg2.pc.Camlburg.action ()
                        in
                            (ral ,pc))
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
                                
# 294 "mipsrec.mlb"
                                ( cat [ "Store(";dst;",";src;",";width w;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_inst
                        (Camlburg.choice
                            [{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.imm.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and imm = arg2.imm.Camlburg.action ()
                                    in
                                        
# 194 "mipsrec.mlb"
                                        ( cat ["la"; " "; regl; ","; imm] )
                                        
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
                                        
# 197 "mipsrec.mlb"
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
                                        
# 200 "mipsrec.mlb"
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
                                            
# 203 "mipsrec.mlb"
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
                                            
# 206 "mipsrec.mlb"
                                            ( cat ["l"; suffix w; zx; " "; regl; ","; mem] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.fregl.Camlburg.cost
                                +
                                arg2.const.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let fregl = arg1.fregl.Camlburg.action ()
                                    and const = arg2.const.Camlburg.action ()
                                    in
                                        
# 209 "mipsrec.mlb"
                                        ( sprintf "li $1, %s; mtc1 $1, %s" const fregl )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.f.Camlburg.cost
                                +
                                arg2.const64.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let f = arg1.f.Camlburg.action ()
                                    and b = arg2.const64.Camlburg.action ()
                                    in
                                        
# 212 "mipsrec.mlb"
                                        ( sprintf "li $1, %s; mtc1 $1, %s; li $1 %s; mtc1 $1, %s"
                       (const64 (lo b)) (freg f) (const64 (hi b)) (freg (f+1))
            )
                                        
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
                                        
# 217 "mipsrec.mlb"
                                        ( cat ["s"; suffix w; " "; reg; ","; meml] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.meml.Camlburg.cost
                                +
                                arg2.freg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let meml = arg1.meml.Camlburg.action ()
                                    and freg = arg2.freg.Camlburg.action ()
                                    and w = arg3
                                    in
                                        
# 220 "mipsrec.mlb"
                                        ( cat ["s.s "; freg; ","; meml] )
                                        
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
                                        
# 223 "mipsrec.mlb"
                                        ( cat ["move"; " "; regl; ","; reg] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.fregl.Camlburg.cost
                                +
                                arg2.freg.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let fregl = arg1.fregl.Camlburg.action ()
                                    and freg = arg2.freg.Camlburg.action ()
                                    in
                                        
# 226 "mipsrec.mlb"
                                        ( cat ["mov.s"; " "; fregl; ","; freg] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.fregl.Camlburg.cost
                                +
                                arg2.freg.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let fregl = arg1.fregl.Camlburg.action ()
                                    and freg = arg2.freg.Camlburg.action ()
                                    in
                                        
# 229 "mipsrec.mlb"
                                        ( cat ["mov.d"; " "; fregl; ","; freg] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.fregl.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let fregl = arg1.fregl.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    in
                                        
# 232 "mipsrec.mlb"
                                        ( cat ["nop; mtc1"; " "; reg; ","; fregl] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.freg.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and freg = arg2.freg.Camlburg.action ()
                                    in
                                        
# 235 "mipsrec.mlb"
                                        ( cat ["nop; mfc1"; " "; regl; ","; freg] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add8.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add8.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 261 "mipsrec.mlb"
                                            ( cat ["add"; " "; dst; ","; x; ","; y] )
                                            
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
                                            
# 264 "mipsrec.mlb"
                                            ( cat ["addi"; " "; dst; ","; x; ","; y] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.fregl.Camlburg.cost
                                +
                                arg2._D2S10.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.fregl.Camlburg.action ()
                                    and _v1 = arg2._D2S10.Camlburg.action ()
                                    in
                                        let x = _v1
                                        in
                                            
# 267 "mipsrec.mlb"
                                            ( cat ["cvt.s.d"; " "; dst; ","; x] )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.fregl.Camlburg.cost
                                +
                                arg2._S2D11.Camlburg.cost
                                +
                                (Camlburg.matches 64) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let dst = arg1.fregl.Camlburg.action ()
                                    and _v1 = arg2._S2D11.Camlburg.action ()
                                    in
                                        let x = _v1
                                        in
                                            
# 270 "mipsrec.mlb"
                                            ( cat ["cvt.d.s"; " "; dst; ","; x] )
                                            
# 000 "/dev/stdout"
)
                            }]))
                        inf)))
and conS2D =
    fun arg1 ->
        (update__S2D11
            {Camlburg.cost = (arg1.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let x = arg1.freg.Camlburg.action () in x)
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
                        
# 291 "mipsrec.mlb"
                        ( cat [ "Reg('";Char.escaped char;"',"; width n;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_f
                {Camlburg.cost = ((Camlburg.matches 'f') arg1)
                ;Camlburg.action =
                    (fun () ->
                        let n = arg2
                        in
                            
# 162 "mipsrec.mlb"
                            ( n )
                            
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
                            
# 168 "mipsrec.mlb"
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
                                    
# 163 "mipsrec.mlb"
                                    ( n )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_ral
                            {Camlburg.cost =
                                ((Camlburg.matches 'r') arg1
                                +
                                (Camlburg.matches 31) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    
# 170 "mipsrec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                            })
                            ((update_spl
                                {Camlburg.cost =
                                    ((Camlburg.matches 'r') arg1
                                    +
                                    (Camlburg.matches 29) arg2)
                                ;Camlburg.action =
                                    (fun () ->
                                        
# 169 "mipsrec.mlb"
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
                        
# 301 "mipsrec.mlb"
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
                                    let addr = _v1
                                    in
                                        
# 247 "mipsrec.mlb"
                                        ( cat ["jal"; " "; addr] )
                                        
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
                                let (ral, pc) = _v2
                                in
                                    let syscall = _v1
                                    in
                                        
# 251 "mipsrec.mlb"
                                        ( "syscall" )
                                        
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
                    
# 272 "mipsrec.mlb"
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
                        
# 290 "mipsrec.mlb"
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
                            
# 179 "mipsrec.mlb"
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
                        
# 288 "mipsrec.mlb"
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
                        
# 278 "mipsrec.mlb"
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
                            
# 160 "mipsrec.mlb"
                            ( x#mangled_text )
                            
# 000 "/dev/stdout"
)
                })
                ((update_syscall
                    {Camlburg.cost =
                        (let x = arg1
                        and w = arg2
                        in
                            
# 249 "mipsrec.mlb"
                            ( guard (x#mangled_text =$= "syscall") )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1
                            and w = arg2
                            in
                                
# 249 "mipsrec.mlb"
                                ( () )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conLate =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let string = arg1
                    and w = arg2
                    in
                        
# 279 "mipsrec.mlb"
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
                        
# 295 "mipsrec.mlb"
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
                        
# 298 "mipsrec.mlb"
                        ( cat [ "Guarded(";guard;",";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                {Camlburg.cost =
                    (arg1.cmp.Camlburg.cost + arg2._Goto4.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let cmp = arg1.cmp.Camlburg.action ()
                        and _v1 = arg2._Goto4.Camlburg.action ()
                        in
                            let addr = _v1
                            in
                                
# 256 "mipsrec.mlb"
                                ( match cmp with 
               | (op,x,y) -> cat ["b";op;" ";x;",";y;",";addr]
            )
                                
# 000 "/dev/stdout"
)
                })
                inf)
and conGoto =
    fun arg1 ->
        (update__Goto4
            {Camlburg.cost = (arg1.addr.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let addr = arg1.addr.Camlburg.action () in addr)
            })
            ((update__Goto6
                {Camlburg.cost = (arg1.syscall.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let syscall = arg1.syscall.Camlburg.action ()
                        in
                            syscall)
                })
                ((update_any
                    {Camlburg.cost = (arg1.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let any = arg1.any.Camlburg.action ()
                            in
                                
# 302 "mipsrec.mlb"
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
                                        
# 240 "mipsrec.mlb"
                                        ( cat ["j"; " "; symbol] )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost = (arg1.reg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let reg = arg1.reg.Camlburg.action ()
                                    in
                                        
# 243 "mipsrec.mlb"
                                        ( cat ["jr"; " "; reg] )
                                        
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
                            
# 282 "mipsrec.mlb"
                            ( cat [ "Fetch(";any;",";width w;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                ((update_freg
                    {Camlburg.cost = (arg1.fregl.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let fregl = arg1.fregl.Camlburg.action ()
                            and w = arg2
                            in
                                
# 173 "mipsrec.mlb"
                                ( fregl  )
                                
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
                                    
# 180 "mipsrec.mlb"
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
                                        
# 175 "mipsrec.mlb"
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
                                            
# 177 "mipsrec.mlb"
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
                                                
# 172 "mipsrec.mlb"
                                                ( regl   )
                                                
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
                                                    
# 176 "mipsrec.mlb"
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
                    
# 277 "mipsrec.mlb"
                    ( cat [ "False" ] )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conD2S =
    fun arg1 ->
        (update__D2S10
            {Camlburg.cost = (arg1.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let x = arg1.freg.Camlburg.action () in x)
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
                        
# 300 "mipsrec.mlb"
                        ( cat [ "Cmp(";op;",";x;",";y;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_cmp
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg2.reg.Camlburg.cost + arg3.reg.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let op = arg1
                            and x = arg2.reg.Camlburg.action ()
                            and y = arg3.reg.Camlburg.action ()
                            in
                                
# 253 "mipsrec.mlb"
                                ( (op,x,y) )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg2.reg.Camlburg.cost + arg3.const.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let op = arg1
                            and x = arg2.reg.Camlburg.action ()
                            and y = arg3.const.Camlburg.action ()
                            in
                                
# 254 "mipsrec.mlb"
                                ( (op,x,y) )
                                
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conBits =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let bits = arg1
                    in
                        
# 280 "mipsrec.mlb"
                        ( cat [ "Bits(b)" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost =
                    (let bits = arg1
                    in
                        
# 159 "mipsrec.mlb"
                        ( guard (Bits.width bits = 32)  )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let bits = arg1
                        in
                            
# 159 "mipsrec.mlb"
                            ( const32 bits )
                            
# 000 "/dev/stdout"
)
                })
                ((update_const64
                    {Camlburg.cost =
                        (let bits = arg1
                        in
                            
# 158 "mipsrec.mlb"
                            ( guard (Bits.width bits = 64)  )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let bits = arg1
                            in
                                
# 158 "mipsrec.mlb"
                                (         bits )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAdd =
    fun arg1 arg2 ->
        (update__Add8
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Add9
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
                                    
# 184 "mipsrec.mlb"
                                    ( cat [imm;"(";reg;")"] )
                                    
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
                                    
# 185 "mipsrec.mlb"
                                    ( cat [imm;"(";reg;")"] )
                                    
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
                                    
# 284 "mipsrec.mlb"
                                    ( cat [ "Add(";x;", ";y;")" ] )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_imm
                            (Camlburg.choice
                                [{Camlburg.cost =
                                    (arg1.symbol.Camlburg.cost
                                    +
                                    arg2.imm.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            symbol =
                                            arg1.symbol.Camlburg.action ()
                                        and imm = arg2.imm.Camlburg.action ()
                                        in
                                            
# 190 "mipsrec.mlb"
                                            ( cat [symbol;"+";imm]  )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost =
                                    (arg1.imm.Camlburg.cost
                                    +
                                    arg2.symbol.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let imm = arg1.imm.Camlburg.action ()
                                        and
                                            symbol =
                                            arg2.symbol.Camlburg.action ()
                                        in
                                            
# 191 "mipsrec.mlb"
                                            ( cat [symbol;"+";imm]  )
                                            
# 000 "/dev/stdout"
)
                                }]))
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
                                            
# 245 "mipsrec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                })
                                inf)))))



# 62 "mipsrec.mlb"
 
  (*s: tail(mipsrec.nw) *)
  let const = function
      | RP.Bool _                 -> error "boolean found"
      | RP.Link(s,_,w)            -> conLink s w
      | RP.Diff _                 -> error "PIC not supported"
      | RP.Bits(b)                -> conBits b
      | RP.Late(s,w)              -> error (sprintf "late constant %s found" s)
  (*x: tail(mipsrec.nw) *)
  (*s: helpers for [[exp]] and [[loc]] *)
  let cmp = 
      Strutil.from_list ["eq";"ge";"geu";"gt";"gtu";"le";"leu";"lt";"ltu";"ne"] 
  (*e: helpers for [[exp]] and [[loc]] *)
  let rec exp = function
      | RP.Const(k)               -> const k
      | RP.Fetch(l,w)             -> conFetch (loc l) w
      (*s: [[Mipsrec]] special cases for [[App]] *)
      | RP.App(("add", [w]), [x; y])            -> conAdd (exp x) (exp y)
      | RP.App(("sub", [w]), [x; y])            -> conSub (exp x) (exp y)
      | RP.App(("f2f_implicit_round", [32;64]), [x])     -> conS2D (exp x) 
      | RP.App(("f2f_implicit_round", [64;32]), [x])     -> conD2S (exp x) 

      | RP.App((op, [w]), [x; y]) 
          when Strutil.Set.mem op cmp           -> conCmp op (exp x) (exp y)
      (*e: [[Mipsrec]] special cases for [[App]] *)
      | RP.App((o,_),_)           -> error (sprintf "unknown operator %s" o)

  and loc = function
      | RP.Reg((sp,_,_),i,w)      -> conReg sp i
      | RP.Mem(('m',_,_),w,e,ass) -> conMem (exp e)
      | RP.Mem((sp,_,_),_,_,_)    -> error (sprintf "mem-space space %c" sp)
      | RP.Var   (s,i,w)          -> error (sprintf "var %s found" s)
      | RP.Global(s,i,w)          -> error (sprintf "var %s found" s)
      | RP.Slice _                -> error "cannot handle slice"
  (*x: tail(mipsrec.nw) *)
  let effect = function
      (*s: [[Mipsrec]] special cases for [[Store]] *)
      | RP.Store(RP.Reg(('c',_,_),i,w),r,_)
        when i = SS.indices.SS.pc   -> conGoto (exp r)
      (*e: [[Mipsrec]] special cases for [[Store]] *)
      | RP.Store(l,e,w)           -> conStore (loc l) (exp e) w
      | RP.Kill(l)                -> error "cannot handle kill"
  (*x: tail(mipsrec.nw) *)
  let guarded g stmt = match g with
      | RP.Const(RP.Bool b)       -> if b then effect stmt else conNop ()
      (*s: [[Mipsrec]] special cases for [[guarded]] *)
      (*e: [[Mipsrec]] special cases for [[guarded]] *)
      | _                         -> conGuarded (exp g) (effect stmt)

  let rec geffects = function
      | []                        -> conNop ()
      | [g, s]                    -> guarded g s
      | (g, s) :: t               -> conPar (guarded g s) (geffects t) 

  let rtl (RP.Rtl es) = geffects es
  (*x: tail(mipsrec.nw) *)
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
  (*e: tail(mipsrec.nw) *)


# 000 "/dev/stdout"
