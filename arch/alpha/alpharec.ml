
# 2 "alpharec.mlb"
 
   (*s: [[Alpharec]] head *)
   module RP = Rtl.Private
   module RU = Rtlutil
   module Up = Rtl.Up
   module Dn = Rtl.Dn
   module SS = Space.Standard64
   (*x: [[Alpharec]] head *)
   exception Error of string
   let error msg = raise (Error msg)   
   let sprintf   = Printf.sprintf (* useful for formatting msg *)
   (*x: [[Alpharec]] head *)
   let guard p = if p then 0 else Camlburg.inf_cost
   (* claude: Alpha's ALU instructions (addq/subq/...) only accept an
    * unsigned 8-bit literal operand (0..255) in their immediate form -
    * anything else needs a real register, or (for the common "adjust sp
    * by a small signed constant" case this guards) the "lda" pseudo-op,
    * which takes a full signed 16-bit displacement instead. Confirmed by
    * alpha-linux-gnu-as itself: "addq $30, -16, $30" (frame-size setup,
    * a negative literal) fails to assemble with "operand out of range
    * (-16 is not between 0 and 255)". See the "addq"/"lda" rule pair
    * below in %% that this guards between. *)
   let is_addq_lit b =
       let n = Bits.S.to_int64 b in
       Int64.compare n 0L >= 0 && Int64.compare n 255L <= 0
   (*x: [[Alpharec]] head *)
   let int64 b =
       assert (Bits.width b = 64);
       Int64.to_string (Bits.U.to_int64 b)

   let int32 b =
       assert (Bits.width b = 32);
       Nativeint.to_string (Bits.U.to_native b)

   let cat = String.concat ""

   let reg n   = "$"  ^ string_of_int n
   let freg n  = "$f" ^ string_of_int n

   let suffix = function
       | 8  -> "b"
       | 16 -> "w"
       | 32 -> "l"
       | 64 -> "q"
       | w  -> error (sprintf "not an Alpha width: %d" w)

   (* claude: Alpha's actual mnemonics for the unsigned cases are
    * "cmpult"/"cmpule" (unsigned-prefix, then lt/le), not "cmpltu"/
    * "cmpleu" (qc--'s own RTL operator names, ltu/leu, unsigned-suffix) -
    * a bare "cmp" ^ op concatenation produced an assembler-rejected
    * mnemonic for every unsigned compare until this remap (confirmed:
    * alpha-linux-gnu-as "unknown opcode cmpltu"). eq/lt/le pass through
    * unchanged - they already match. Defined here in %head, not %tail
    * (where "cmp" just below lives) - %% actions can only see %head
    * bindings, confirmed by is_addq_lit's own placement/use above. *)
   let alpha_cmp_mnemonic = function
     | "ltu" -> "ult" | "leu" -> "ule" | op -> op

   let width w = string_of_int w
   (*e: [[Alpharec]] head *)


# 000 "/dev/stdout"


type
    (
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
        _Zx3: ( 't44 ) Camlburg.nt;
        _Sx1: ( 't43 ) Camlburg.nt;
        _Sub14: ( 't42 ) Camlburg.nt;
        _Store5: ( 't41 ) Camlburg.nt;
        _Mul15: ( 't40 ) Camlburg.nt;
        _Goto8: ( 't39 ) Camlburg.nt;
        _Goto4: ( 't38 ) Camlburg.nt;
        _GP7: ( 't37 ) Camlburg.nt;
        _Fetch2: ( 't36 ) Camlburg.nt;
        _Com10: ( 't35 ) Camlburg.nt;
        _Bit9: ( 't34 ) Camlburg.nt;
        _And17: ( 't33 ) Camlburg.nt;
        _And16: ( 't32 ) Camlburg.nt;
        _Add6: ( 't31 ) Camlburg.nt;
        _Add13: ( 't30 ) Camlburg.nt;
        _Add12: ( 't29 ) Camlburg.nt;
        _Add11: ( 't28 ) Camlburg.nt;
        zero: ( 't27 ) Camlburg.nt;
        symbol: ( 't26 ) Camlburg.nt;
        spl: ( 't25 ) Camlburg.nt;
        sp: ( 't24 ) Camlburg.nt;
        regl: ( 't23 ) Camlburg.nt;
        reg: ( 't22 ) Camlburg.nt;
        ral: ( 't21 ) Camlburg.nt;
        ra: ( 't20 ) Camlburg.nt;
        pvl: ( 't19 ) Camlburg.nt;
        pv: ( 't18 ) Camlburg.nt;
        pcl: ( 't17 ) Camlburg.nt;
        pc: ( 't16 ) Camlburg.nt;
        next: ( 't15 ) Camlburg.nt;
        meml: ( 't14 ) Camlburg.nt;
        mem: ( 't13 ) Camlburg.nt;
        inst: ( 't12 ) Camlburg.nt;
        imm8: ( 't11 ) Camlburg.nt;
        imm: ( 't10 ) Camlburg.nt;
        gpl: ( 't9 ) Camlburg.nt;
        gp: ( 't8 ) Camlburg.nt;
        fregl: ( 't7 ) Camlburg.nt;
        freg: ( 't6 ) Camlburg.nt;
        four: ( 't5 ) Camlburg.nt;
        const: ( 't4 ) Camlburg.nt;
        cmp_zero: ( 't3 ) Camlburg.nt;
        cmp: ( 't2 ) Camlburg.nt;
        any: ( 't1 ) Camlburg.nt;
        addr: ( 't0 ) Camlburg.nt
    }

let rec
inf =
    {addr = (Camlburg.infinity)
    ;any = (Camlburg.infinity)
    ;cmp = (Camlburg.infinity)
    ;cmp_zero = (Camlburg.infinity)
    ;const = (Camlburg.infinity)
    ;four = (Camlburg.infinity)
    ;freg = (Camlburg.infinity)
    ;fregl = (Camlburg.infinity)
    ;gp = (Camlburg.infinity)
    ;gpl = (Camlburg.infinity)
    ;imm = (Camlburg.infinity)
    ;imm8 = (Camlburg.infinity)
    ;inst = (Camlburg.infinity)
    ;mem = (Camlburg.infinity)
    ;meml = (Camlburg.infinity)
    ;next = (Camlburg.infinity)
    ;pc = (Camlburg.infinity)
    ;pcl = (Camlburg.infinity)
    ;pv = (Camlburg.infinity)
    ;pvl = (Camlburg.infinity)
    ;ra = (Camlburg.infinity)
    ;ral = (Camlburg.infinity)
    ;reg = (Camlburg.infinity)
    ;regl = (Camlburg.infinity)
    ;sp = (Camlburg.infinity)
    ;spl = (Camlburg.infinity)
    ;symbol = (Camlburg.infinity)
    ;zero = (Camlburg.infinity)
    ;_Add11 = (Camlburg.infinity)
    ;_Add12 = (Camlburg.infinity)
    ;_Add13 = (Camlburg.infinity)
    ;_Add6 = (Camlburg.infinity)
    ;_And16 = (Camlburg.infinity)
    ;_And17 = (Camlburg.infinity)
    ;_Bit9 = (Camlburg.infinity)
    ;_Com10 = (Camlburg.infinity)
    ;_Fetch2 = (Camlburg.infinity)
    ;_GP7 = (Camlburg.infinity)
    ;_Goto4 = (Camlburg.infinity)
    ;_Goto8 = (Camlburg.infinity)
    ;_Mul15 = (Camlburg.infinity)
    ;_Store5 = (Camlburg.infinity)
    ;_Sub14 = (Camlburg.infinity)
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
                                
# 308 "alpharec.mlb"
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
and update_cmp_zero =
    fun nt x ->
        if nt.Camlburg.cost >= x.cmp_zero.Camlburg.cost then
            x
        else
            { x with cmp_zero = (nt) }
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
                                
# 196 "alpharec.mlb"
                                ( const  )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with const = (nt) }
and update_four =
    fun nt x ->
        if nt.Camlburg.cost >= x.four.Camlburg.cost then
            x
        else
            { x with four = (nt) }
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
and update_gp =
    fun nt x ->
        if nt.Camlburg.cost >= x.gp.Camlburg.cost then
            x
        else
            { x with gp = (nt) }
and update_gpl =
    fun nt x ->
        if nt.Camlburg.cost >= x.gpl.Camlburg.cost then
            x
        else
            { x with gpl = (nt) }
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
                                
# 191 "alpharec.mlb"
                                ( imm                      )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with imm = (nt) }
and update_imm8 =
    fun nt x ->
        if nt.Camlburg.cost >= x.imm8.Camlburg.cost then
            x
        else
            { x with imm8 = (nt) }
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
and update_pv =
    fun nt x ->
        if nt.Camlburg.cost >= x.pv.Camlburg.cost then
            x
        else
            { x with pv = (nt) }
and update_pvl =
    fun nt x ->
        if nt.Camlburg.cost >= x.pvl.Camlburg.cost then
            x
        else
            { x with pvl = (nt) }
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
                                
# 190 "alpharec.mlb"
                                ( sprintf "0(%s)" reg       )
                                
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
                                
# 194 "alpharec.mlb"
                                ( symbol                   )
                                
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
                                    
# 197 "alpharec.mlb"
                                    ( symbol )
                                    
# 000 "/dev/stdout"
)
                        })
                        x)
                    { x with symbol = (nt) })
and update_zero =
    fun nt x ->
        if nt.Camlburg.cost >= x.zero.Camlburg.cost then
            x
        else
            { x with zero = (nt) }
and update__Add11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add11.Camlburg.cost then
            x
        else
            { x with _Add11 = (nt) }
and update__Add12 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add12.Camlburg.cost then
            x
        else
            { x with _Add12 = (nt) }
and update__Add13 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add13.Camlburg.cost then
            x
        else
            { x with _Add13 = (nt) }
and update__Add6 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add6.Camlburg.cost then
            x
        else
            { x with _Add6 = (nt) }
and update__And16 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And16.Camlburg.cost then
            x
        else
            { x with _And16 = (nt) }
and update__And17 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And17.Camlburg.cost then
            x
        else
            { x with _And17 = (nt) }
and update__Bit9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Bit9.Camlburg.cost then
            x
        else
            { x with _Bit9 = (nt) }
and update__Com10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Com10.Camlburg.cost then
            x
        else
            { x with _Com10 = (nt) }
and update__Fetch2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch2.Camlburg.cost then
            x
        else
            { x with _Fetch2 = (nt) }
and update__GP7 =
    fun nt x ->
        if nt.Camlburg.cost >= x._GP7.Camlburg.cost then
            x
        else
            { x with _GP7 = (nt) }
and update__Goto4 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto4.Camlburg.cost then
            x
        else
            { x with _Goto4 = (nt) }
and update__Goto8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto8.Camlburg.cost then
            x
        else
            { x with _Goto8 = (nt) }
and update__Mul15 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul15.Camlburg.cost then
            x
        else
            { x with _Mul15 = (nt) }
and update__Store5 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store5.Camlburg.cost then
            x
        else
            { x with _Store5 = (nt) }
and update__Sub14 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub14.Camlburg.cost then
            x
        else
            { x with _Sub14 = (nt) }
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
                            
# 323 "alpharec.mlb"
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
                    
# 310 "alpharec.mlb"
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
                            
# 322 "alpharec.mlb"
                            ( cat [ "Sx(";any;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conSub =
    fun arg1 arg2 ->
        (update__Sub14
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
                            
# 319 "alpharec.mlb"
                            ( cat [ "Sub(";x;", ";y;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conStore =
    fun arg1 arg2 arg3 ->
        (update__Store5
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
                            
# 330 "alpharec.mlb"
                            ( cat [ "Store(";dst;",";src;",";width w;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                ((update_inst
                    (Camlburg.choice
                        [{Camlburg.cost =
                            (arg1.regl.Camlburg.cost + arg2.imm.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let regl = arg1.regl.Camlburg.action ()
                                and imm = arg2.imm.Camlburg.action ()
                                in
                                    
# 205 "alpharec.mlb"
                                    ( sprintf "lda %s, %s" regl imm )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2.const.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let regl = arg1.regl.Camlburg.action ()
                                and const = arg2.const.Camlburg.action ()
                                in
                                    
# 209 "alpharec.mlb"
                                    ( sprintf "ldiq %s, %s" regl const )
                                    
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
                                    
# 213 "alpharec.mlb"
                                    ( sprintf "ldq %s, %s" regl mem )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.fregl.Camlburg.cost
                            +
                            arg2.mem.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let fregl = arg1.fregl.Camlburg.action ()
                                and mem = arg2.mem.Camlburg.action ()
                                in
                                    
# 217 "alpharec.mlb"
                                    ( sprintf "lds %s, %s" fregl mem )
                                    
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
                                        
# 220 "alpharec.mlb"
                                        ( sprintf "ld%s %s, %s" (suffix w) regl mem )
                                        
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
                                        
# 223 "alpharec.mlb"
                                        ( sprintf "ld%su %s, %s" (suffix w) regl mem )
                                        
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
                                    
# 227 "alpharec.mlb"
                                    ( sprintf "st%s %s, %s" (suffix w) reg meml )
                                    
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
                                    
# 230 "alpharec.mlb"
                                    ( sprintf "sts %s, %s" freg meml )
                                    
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
                                    
# 233 "alpharec.mlb"
                                    ( sprintf "mov %s, %s" reg  regl )
                                    
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
                                    
# 236 "alpharec.mlb"
                                    ( sprintf "fmov %s, %s" freg  fregl )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.fregl.Camlburg.cost
                            +
                            arg2.reg.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let fregl = arg1.fregl.Camlburg.action ()
                                and reg = arg2.reg.Camlburg.action ()
                                in
                                    
# 239 "alpharec.mlb"
                                    ( sprintf "itoft %s, %s" reg  fregl )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2.freg.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let regl = arg1.regl.Camlburg.action ()
                                and freg = arg2.freg.Camlburg.action ()
                                in
                                    
# 242 "alpharec.mlb"
                                    ( sprintf "ftoit %s, %s" freg  regl )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.gpl.Camlburg.cost + arg2._GP7.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let gpl = arg1.gpl.Camlburg.action ()
                                and _v1 = arg2._GP7.Camlburg.action ()
                                in
                                    let reg = _v1
                                    in
                                        
# 261 "alpharec.mlb"
                                        ( sprintf "ldgp $gp, (%s)" reg )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2._Bit9.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._Bit9.Camlburg.action ()
                                in
                                    let cmp = _v1
                                    in
                                        
# 273 "alpharec.mlb"
                                        ( match cmp with (op, x, y) ->
                sprintf "cmp%s %s, %s, %s" (alpha_cmp_mnemonic op) x y dst
            )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2._Com10.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._Com10.Camlburg.action ()
                                in
                                    let reg = _v1
                                    in
                                        
# 278 "alpharec.mlb"
                                        ( sprintf "not %s, %s" dst reg )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2._Add11.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._Add11.Camlburg.action ()
                                in
                                    let (x, y) = _v1
                                    in
                                        
# 282 "alpharec.mlb"
                                        ( sprintf "addq %s, %s, %s" x y dst )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2._Add12.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._Add12.Camlburg.action ()
                                in
                                    let (x, y) = _v1
                                    in
                                        
# 285 "alpharec.mlb"
                                        ( sprintf "addq %s, %s, %s" x y dst )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (1 + arg1.regl.Camlburg.cost
                            +
                            arg2._Add13.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._Add13.Camlburg.action ()
                                in
                                    let (x, y) = _v1
                                    in
                                        
# 289 "alpharec.mlb"
                                        ( sprintf "lda %s, %s(%s)" dst y x )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2._Sub14.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._Sub14.Camlburg.action ()
                                in
                                    let (x, y) = _v1
                                    in
                                        
# 293 "alpharec.mlb"
                                        ( sprintf "subq %s, %s, %s" x y dst )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2._Mul15.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._Mul15.Camlburg.action ()
                                in
                                    let (x, y) = _v1
                                    in
                                        
# 297 "alpharec.mlb"
                                        ( sprintf "mulq %s, %s, %s" x y dst )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2._And16.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._And16.Camlburg.action ()
                                in
                                    let (x, y) = _v1
                                    in
                                        
# 301 "alpharec.mlb"
                                        ( sprintf "and %s, %s, %s" x y dst )
                                        
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (1 + arg1.regl.Camlburg.cost
                            +
                            arg2._And17.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._And17.Camlburg.action ()
                                in
                                    let (x, y) = _v1
                                    in
                                        
# 304 "alpharec.mlb"
                                        ( sprintf "ldiq $28, %s\n\tand %s, $28, %s" y x dst )
                                        
# 000 "/dev/stdout"
)
                        }]))
                    ((update_next
                        {Camlburg.cost =
                            (arg1.regl.Camlburg.cost
                            +
                            arg2._Add6.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg3)
                        ;Camlburg.action =
                            (fun () ->
                                let regl = arg1.regl.Camlburg.action ()
                                and _v1 = arg2._Add6.Camlburg.action ()
                                in
                                    let (pc, four) = _v1
                                    in
                                        
# 255 "alpharec.mlb"
                                        ( regl )
                                        
# 000 "/dev/stdout"
)
                        })
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
                        
# 327 "alpharec.mlb"
                        ( cat [ "Reg('";Char.escaped char;"',";width n;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_fregl
                {Camlburg.cost = ((Camlburg.matches 'f') arg1)
                ;Camlburg.action =
                    (fun () ->
                        let n = arg2
                        in
                            
# 167 "alpharec.mlb"
                            ( freg n )
                            
# 000 "/dev/stdout"
)
                })
                ((update_gpl
                    {Camlburg.cost =
                        ((Camlburg.matches 'r') arg1
                        +
                        (Camlburg.matches 29) arg2)
                    ;Camlburg.action =
                        (fun () ->
                            
# 173 "alpharec.mlb"
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
                                
# 169 "alpharec.mlb"
                                ( () )
                                
# 000 "/dev/stdout"
)
                        })
                        ((update_pvl
                            {Camlburg.cost =
                                ((Camlburg.matches 'r') arg1
                                +
                                (Camlburg.matches 27) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    
# 172 "alpharec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                            })
                            ((update_ral
                                {Camlburg.cost =
                                    ((Camlburg.matches 'r') arg1
                                    +
                                    (Camlburg.matches 26) arg2)
                                ;Camlburg.action =
                                    (fun () ->
                                        
# 171 "alpharec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                                })
                                ((update_regl
                                    {Camlburg.cost =
                                        ((Camlburg.matches 'r') arg1)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let n = arg2
                                            in
                                                
# 166 "alpharec.mlb"
                                                ( reg  n )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_spl
                                        {Camlburg.cost =
                                            ((Camlburg.matches 'r') arg1
                                            +
                                            (Camlburg.matches 30) arg2)
                                        ;Camlburg.action =
                                            (fun () ->
                                                
# 170 "alpharec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                        })
                                        inf)))))))
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
                        
# 340 "alpharec.mlb"
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
                                let (spl, nsp) = _v2
                                in
                                    let target = _v1
                                    in
                                        
# 253 "alpharec.mlb"
                                        ( sprintf "mov %s, $30\n\tjmp (%s)" nsp target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto4.Camlburg.cost + arg2.next.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto4.Camlburg.action ()
                            and next = arg2.next.Camlburg.action ()
                            in
                                let reg = _v1
                                in
                                    
# 258 "alpharec.mlb"
                                    ( sprintf "jsr %s,(%s)" next reg )
                                    
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
                    
# 306 "alpharec.mlb"
                    ( "nop" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update__Mul15
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
                            
# 320 "alpharec.mlb"
                            ( cat [ "Mul(";x;", ";y;")" ] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conMem =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 326 "alpharec.mlb"
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
                            
# 186 "alpharec.mlb"
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
                        
# 324 "alpharec.mlb"
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
                        
# 312 "alpharec.mlb"
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
                            
# 164 "alpharec.mlb"
                            ( x#mangled_text       )
                            
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
                        
# 313 "alpharec.mlb"
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
                        
# 331 "alpharec.mlb"
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
                        
# 334 "alpharec.mlb"
                        ( cat [ "Guarded(";guard;",";any;")" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                {Camlburg.cost =
                    (arg1.cmp_zero.Camlburg.cost + arg2._Goto8.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let cmp_zero = arg1.cmp_zero.Camlburg.action ()
                        and _v1 = arg2._Goto8.Camlburg.action ()
                        in
                            let addr = _v1
                            in
                                
# 264 "alpharec.mlb"
                                ( match cmp_zero with (op,reg) ->
                sprintf "b%s %s, %s" op reg addr
            )
                                
# 000 "/dev/stdout"
)
                })
                inf)
and conGoto =
    fun arg1 ->
        (update__Goto4
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
                                
# 341 "alpharec.mlb"
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
                                        
# 246 "alpharec.mlb"
                                        ( sprintf "br %s" symbol )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost = (arg1.reg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let reg = arg1.reg.Camlburg.action ()
                                    in
                                        
# 249 "alpharec.mlb"
                                        ( sprintf "jmp (%s)" reg  )
                                        
# 000 "/dev/stdout"
)
                            }]))
                        inf)))
and conGP =
    fun arg1 ->
        (update__GP7
            {Camlburg.cost = (arg1.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let reg = arg1.reg.Camlburg.action () in reg)
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        in
                            
# 337 "alpharec.mlb"
                            ( cat [ "GP(";any;")"] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
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
                            
# 316 "alpharec.mlb"
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
                                
# 177 "alpharec.mlb"
                                ( fregl   )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_gp
                        {Camlburg.cost =
                            (arg1.gpl.Camlburg.cost
                            +
                            (Camlburg.matches 64) arg2)
                        ;Camlburg.action =
                            (fun () ->
                                let gpl = arg1.gpl.Camlburg.action ()
                                in
                                    
# 183 "alpharec.mlb"
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
                                        
# 187 "alpharec.mlb"
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
                                            
# 179 "alpharec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_pv
                                    {Camlburg.cost =
                                        (arg1.pvl.Camlburg.cost
                                        +
                                        (Camlburg.matches 64) arg2)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                pvl =
                                                arg1.pvl.Camlburg.action ()
                                            in
                                                
# 182 "alpharec.mlb"
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
                                                let
                                                    ral =
                                                    arg1.ral.Camlburg.action
                                                        ()
                                                in
                                                    
# 181 "alpharec.mlb"
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
                                                        
# 176 "alpharec.mlb"
                                                        ( regl    )
                                                        
# 000 "/dev/stdout"
)
                                            })
                                            ((update_sp
                                                {Camlburg.cost =
                                                    (arg1.spl.Camlburg.cost
                                                    +
                                                    (Camlburg.matches 64)
                                                        arg2)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        let
                                                            spl =
                                                            arg1.spl.Camlburg.action
                                                                ()
                                                        in
                                                            
# 180 "alpharec.mlb"
                                                            ( () )
                                                            
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
                    
# 311 "alpharec.mlb"
                    ( cat [ "False" ] )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conCom =
    fun arg1 ->
        (update__Com10
            {Camlburg.cost = (arg1.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let reg = arg1.reg.Camlburg.action () in reg)
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        in
                            
# 338 "alpharec.mlb"
                            ( cat [ "Com(";any;")"] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
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
                        
# 336 "alpharec.mlb"
                        ( cat [ "Cmp(";op;",";x;",";y;")"] )
                        
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
                            
# 268 "alpharec.mlb"
                            ( (op, x, y) )
                            
# 000 "/dev/stdout"
)
                })
                ((update_cmp_zero
                    {Camlburg.cost =
                        (arg2.reg.Camlburg.cost + arg3.zero.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let op = arg1
                            and reg = arg2.reg.Camlburg.action ()
                            and zero = arg3.zero.Camlburg.action ()
                            in
                                
# 269 "alpharec.mlb"
                                ( (op, reg)  )
                                
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
                        
# 314 "alpharec.mlb"
                        ( cat [ "Bits(b)" ] )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost = (0)
                ;Camlburg.action =
                    (fun () ->
                        let bits = arg1
                        in
                            
# 161 "alpharec.mlb"
                            ( int64 bits           )
                            
# 000 "/dev/stdout"
)
                })
                ((update_four
                    {Camlburg.cost =
                        (let bits = arg1
                        in
                            
# 163 "alpharec.mlb"
                            ( guard (Bits.eq bits (Bits.S.of_int 4 64)) )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let bits = arg1
                            in
                                
# 163 "alpharec.mlb"
                                (())
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_imm8
                        {Camlburg.cost =
                            (let bits = arg1
                            in
                                
# 202 "alpharec.mlb"
                                ( guard (is_addq_lit bits) )
                                
# 000 "/dev/stdout"
)
                        ;Camlburg.action =
                            (fun () ->
                                let bits = arg1
                                in
                                    
# 202 "alpharec.mlb"
                                    ( int64 bits )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_zero
                            {Camlburg.cost =
                                (let bits = arg1
                                in
                                    
# 162 "alpharec.mlb"
                                    ( guard (Bits.eq bits (Bits.zero 64)) )
                                    
# 000 "/dev/stdout"
)
                            ;Camlburg.action =
                                (fun () ->
                                    let bits = arg1
                                    in
                                        
# 162 "alpharec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            inf))))
and conBit =
    fun arg1 ->
        (update__Bit9
            {Camlburg.cost = (arg1.cmp.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cmp = arg1.cmp.Camlburg.action () in cmp)
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        in
                            
# 339 "alpharec.mlb"
                            ( cat [ "Bit(";any;")"] )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conAnd =
    fun arg1 arg2 ->
        (update__And16
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__And17
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
                                
# 321 "alpharec.mlb"
                                ( cat [ "And(";x;", ";y;")" ] )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAdd =
    fun arg1 arg2 ->
        (update__Add11
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Add12
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.imm8.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.imm8.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update__Add13
                    {Camlburg.cost =
                        (arg1.reg.Camlburg.cost + arg2.imm.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.reg.Camlburg.action ()
                            and y = arg2.imm.Camlburg.action ()
                            in
                                (x ,y))
                    })
                    ((update__Add6
                        {Camlburg.cost =
                            (arg1.pc.Camlburg.cost + arg2.four.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let pc = arg1.pc.Camlburg.action ()
                                and four = arg2.four.Camlburg.action ()
                                in
                                    (pc ,four))
                        })
                        ((update_addr
                            (Camlburg.choice
                                [{Camlburg.cost =
                                    (arg1.imm.Camlburg.cost
                                    +
                                    arg2.reg.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let imm = arg1.imm.Camlburg.action ()
                                        and reg = arg2.reg.Camlburg.action ()
                                        in
                                            
# 192 "alpharec.mlb"
                                            ( sprintf "%s(%s)" imm reg )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost =
                                    (arg1.reg.Camlburg.cost
                                    +
                                    arg2.imm.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let reg = arg1.reg.Camlburg.action ()
                                        and imm = arg2.imm.Camlburg.action ()
                                        in
                                            
# 193 "alpharec.mlb"
                                            ( sprintf "%s(%s)" imm reg )
                                            
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
                                            
# 318 "alpharec.mlb"
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
                                                    arg1.symbol.Camlburg.action
                                                        ()
                                                and
                                                    imm =
                                                    arg2.imm.Camlburg.action
                                                        ()
                                                in
                                                    
# 198 "alpharec.mlb"
                                                    ( sprintf "%s+%s" symbol imm )
                                                    
# 000 "/dev/stdout"
)
                                        }
                                        ;{Camlburg.cost =
                                            (arg1.imm.Camlburg.cost
                                            +
                                            arg2.symbol.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    imm =
                                                    arg1.imm.Camlburg.action
                                                        ()
                                                and
                                                    symbol =
                                                    arg2.symbol.Camlburg.action
                                                        ()
                                                in
                                                    
# 199 "alpharec.mlb"
                                                    ( sprintf "%s+%s" symbol imm )
                                                    
# 000 "/dev/stdout"
)
                                        }]))
                                    inf))))))



# 63 "alpharec.mlb"
 
   (*s: [[Alpharec]] tail *)
   let const = function
       | RP.Bool _                 -> error "boolean found"
       | RP.Link(s,_,w)            -> conLink s w
       | RP.Diff _                 -> error "PIC not supported"
       | RP.Bits(b)                -> conBits b
       | RP.Late(s,w)              -> error (sprintf "late constant %s found" s)
   (*x: [[Alpharec]] tail *)
   (*s: [[Alpharec]] helpers for [[exp]] and [[loc]] *)
   let cmp = Strutil.from_list ["eq";"ge";"geu";"gt";"gtu";"le";"leu";"lt";"ltu";"ne"]
   (*e: [[Alpharec]] helpers for [[exp]] and [[loc]] *)
   let rec exp = function
       | RP.Const(k)               -> const k
       | RP.Fetch(l,w)             -> conFetch (loc l) w
       (*s: [[Alpharec]] special cases for [[App]] *)
       | RP.App(("add", [w]), [x; y])            -> conAdd (exp x) (exp y)
       | RP.App(("sub", [w]), [x; y])            -> conSub (exp x) (exp y)
       | RP.App(("mul", [w]), [x; y])            -> conMul (exp x) (exp y)
       | RP.App(("and", [w]), [x; y])            -> conAnd (exp x) (exp y)
       | RP.App((op, [w]), [x; y])  
           when Strutil.Set.mem op cmp           -> conCmp op (exp x) (exp y)
       | RP.App(("bit", []), [x])                -> conBit (exp x)
       | RP.App(("alpha_gp", []), [x])           -> conGP  (exp x)  
       | RP.App(("com", [64]), [x])              -> conCom  (exp x)
       (* claude: %lobits only ever wraps a value immediately feeding a narrow-width memory store (alpha.ml's Post.lostore, its only producer) - a real stb/stw/stl already truncates its source register to the store's own width, so the narrowing is a free no-op here, same reasoning riscv32rec.mlb's/riscv64rec.mlb's/mipsrec.mlb's identical case gives. *)
       | RP.App(("lobits", [_;_]), [x])          -> exp x
       (*e: [[Alpharec]] special cases for [[App]] *)
       | RP.App((o,_),_)           -> error (sprintf "unknown operator %s" o)

   and loc = function
       | RP.Reg((sp,_,_),i,_)      -> conReg sp i
       | RP.Mem(('m',_,_),w,e,ass) -> conMem (exp e) 
       | RP.Mem((sp,_,_),_,_,_)    -> error (sprintf "mem-space space %c" sp)
       | RP.Var   (s,i,w)          -> error (sprintf "var %s found" s)
       | RP.Global(s,i,w)          -> error (sprintf "var %s found" s)
       | RP.Slice _                -> error "cannot handle slice"
   (*x: [[Alpharec]] tail *)
   let effect = function
       (*s: [[Alpharec]] special cases for [[Store]] *)
       | RP.Store(RP.Reg(('c',_,_),i,_),r,_) when i = SS.indices.SS.pc   -> conGoto (exp r)
       (*e: [[Alpharec]] special cases for [[Store]] *)
       | RP.Store(l,e,w)           -> conStore (loc l) (exp e) w
       | RP.Kill(l)                -> error "cannot handle kill"
   (*x: [[Alpharec]] tail *)
   let guarded g stmt = match g with
       | RP.Const(RP.Bool b)       -> if b then effect stmt else conNop ()
       (*s: [[Alpharec]] special cases for [[guarded]] *)
       (*e: [[Alpharec]] special cases for [[guarded]] *)
       | _                         -> conGuarded (exp g) (effect stmt)

   let rec geffects = function
       | []                        -> conNop ()
       | [g, s]                    -> guarded g s
       | (g, s) :: t               -> conPar (guarded g s) (geffects t) 

   let rtl (RP.Rtl es) = geffects es
   (*x: [[Alpharec]] tail *)
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
           sprintf "\t%s" (plan.inst.Camlburg.action ())
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
   (*e: [[Alpharec]] tail *)
 

# 000 "/dev/stdout"
