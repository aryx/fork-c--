
# 2 "sparcrec.mlb"
 
   (*s: [[Sparcrec]] modules *)
   module R  = Rtl
   module RU = Rtlutil
   module RP = Rtl.Private
   module SS = Space.Standard32
   module Down = Rtl.Dn
   module Up   = Rtl.Up

   (*e: [[Sparcrec]] modules *)
   (*s: [[Sparcrec]] code to precede the labeler *)
   let infinity = Camlburg.inf_cost
   let guard b = if b then 0 else infinity

   let const32 b =
     assert (Bits.width b = 32);
     Nativeint.to_string (Bits.U.to_native b)

   let const64 b =
     assert (Bits.width b = 64);
     let hi32 = Bits.Ops.lobits 32 (Bits.Ops.shrl b (Bits.U.of_int 32 64)) in
     let lo32 = Bits.Ops.lobits 32 b in
     (Nativeint.to_string (Bits.U.to_native hi32),
      Nativeint.to_string (Bits.U.to_native lo32))

   exception Error of string
   let sprintf   = Printf.sprintf
   let s         = Printf.sprintf
   let error msg = raise (Error msg)

   let rspace = ('r', R.BigEndian, Cell.of_size 32)
   let spl = RP.Reg(rspace, 14, R.C 1)
   let sp  = RP.Fetch(spl, 32)
   let ral = RP.Reg(rspace, 31, R.C 1)
   let ra  = RP.Fetch(ral, 32)
   let yregl = RP.Reg Sparcregs.y
   let yreg = RP.Fetch(yregl, 32)

   let idiomatic_reg_name n =
     if n = 14 then "%sp"
     else if n = 30 then "%fp"
     else if n >= 0 && n < 8 then sprintf "%%g%i" n
     else if n < 16 then sprintf "%%o%i" (n - 8)
     else if n < 24 then sprintf "%%l%i" (n - 16)
     else if n < 32 then sprintf "%%i%i" (n - 24)
     else Impossible.impossible (sprintf "Register %%r%i doesn't exist" n)

   let positive b = Bits.Ops.gt b (Bits.zero 32)
   let negative b = Bits.Ops.lt b (Bits.zero 32)

   let in_proc = Reinit.ref false
   (*e: [[Sparcrec]] code to precede the labeler *)
      

# 000 "/dev/stdout"


type
    (
        't129,
        't128,
        't127,
        't126,
        't125,
        't124,
        't123,
        't122,
        't121,
        't120,
        't119,
        't118,
        't117,
        't116,
        't115,
        't114,
        't113,
        't112,
        't111,
        't110,
        't109,
        't108,
        't107,
        't106,
        't105,
        't104,
        't103,
        't102,
        't101,
        't100,
        't99,
        't98,
        't97,
        't96,
        't95,
        't94,
        't93,
        't92,
        't91,
        't90,
        't89,
        't88,
        't87,
        't86,
        't85,
        't84,
        't83,
        't82,
        't81,
        't80,
        't79,
        't78,
        't77,
        't76,
        't75,
        't74,
        't73,
        't72,
        't71,
        't70,
        't69,
        't68,
        't67,
        't66,
        't65,
        't64,
        't63,
        't62,
        't61,
        't60,
        't59,
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
        _Zxhalf9: ( 't129 ) Camlburg.nt;
        _Zxbyte8: ( 't128 ) Camlburg.nt;
        _Xor35: ( 't127 ) Camlburg.nt;
        _Xor34: ( 't126 ) Camlburg.nt;
        _Sxhalf11: ( 't125 ) Camlburg.nt;
        _Sxbyte10: ( 't124 ) Camlburg.nt;
        _Subb59: ( 't123 ) Camlburg.nt;
        _Sub19: ( 't122 ) Camlburg.nt;
        _Store64: ( 't121 ) Camlburg.nt;
        _Store63: ( 't120 ) Camlburg.nt;
        _Store61: ( 't119 ) Camlburg.nt;
        _Store60: ( 't118 ) Camlburg.nt;
        _Store55: ( 't117 ) Camlburg.nt;
        _Store25: ( 't116 ) Camlburg.nt;
        _Store23: ( 't115 ) Camlburg.nt;
        _Store22: ( 't114 ) Camlburg.nt;
        _Sparcsubcc68: ( 't113 ) Camlburg.nt;
        _Sparcsubcc67: ( 't112 ) Camlburg.nt;
        _Sparcsubcc66: ( 't111 ) Camlburg.nt;
        _Sparcsbbflags65: ( 't110 ) Camlburg.nt;
        _Sparcne70: ( 't109 ) Camlburg.nt;
        _Sparcmulxhi24: ( 't108 ) Camlburg.nt;
        _Sparcmuluxhi26: ( 't107 ) Camlburg.nt;
        _Sparcltu78: ( 't106 ) Camlburg.nt;
        _Sparclt77: ( 't105 ) Camlburg.nt;
        _Sparcleu76: ( 't104 ) Camlburg.nt;
        _Sparcle75: ( 't103 ) Camlburg.nt;
        _Sparcgtu74: ( 't102 ) Camlburg.nt;
        _Sparcgt73: ( 't101 ) Camlburg.nt;
        _Sparcgeu72: ( 't100 ) Camlburg.nt;
        _Sparcge71: ( 't99 ) Camlburg.nt;
        _Sparcfne80: ( 't98 ) Camlburg.nt;
        _Sparcflt84: ( 't97 ) Camlburg.nt;
        _Sparcfle83: ( 't96 ) Camlburg.nt;
        _Sparcfgt82: ( 't95 ) Camlburg.nt;
        _Sparcfge81: ( 't94 ) Camlburg.nt;
        _Sparcfeq79: ( 't93 ) Camlburg.nt;
        _Sparceq69: ( 't92 ) Camlburg.nt;
        _Sparccarrybit58: ( 't91 ) Camlburg.nt;
        _Sparcaddcc56: ( 't90 ) Camlburg.nt;
        _Sparcadcflags62: ( 't89 ) Camlburg.nt;
        _Shrl38: ( 't88 ) Camlburg.nt;
        _Shra39: ( 't87 ) Camlburg.nt;
        _Shl37: ( 't86 ) Camlburg.nt;
        _Reg2: ( 't85 ) Camlburg.nt;
        _Reg1: ( 't84 ) Camlburg.nt;
        _Quot27: ( 't83 ) Camlburg.nt;
        _Or33: ( 't82 ) Camlburg.nt;
        _Or32: ( 't81 ) Camlburg.nt;
        _Neg29: ( 't80 ) Camlburg.nt;
        _Mul21: ( 't79 ) Camlburg.nt;
        _Mul20: ( 't78 ) Camlburg.nt;
        _Mem14: ( 't77 ) Camlburg.nt;
        _Lohalf13: ( 't76 ) Camlburg.nt;
        _Lobyte12: ( 't75 ) Camlburg.nt;
        _Itof5: ( 't74 ) Camlburg.nt;
        _Goto54: ( 't73 ) Camlburg.nt;
        _Goto52: ( 't72 ) Camlburg.nt;
        _Goto51: ( 't71 ) Camlburg.nt;
        _Ftoi4: ( 't70 ) Camlburg.nt;
        _Ftoi3: ( 't69 ) Camlburg.nt;
        _Ftof7: ( 't68 ) Camlburg.nt;
        _Ftof6: ( 't67 ) Camlburg.nt;
        _Fsub47: ( 't66 ) Camlburg.nt;
        _Fsub46: ( 't65 ) Camlburg.nt;
        _Fneg49: ( 't64 ) Camlburg.nt;
        _Fneg48: ( 't63 ) Camlburg.nt;
        _Fmul43: ( 't62 ) Camlburg.nt;
        _Fmul42: ( 't61 ) Camlburg.nt;
        _Fetch16: ( 't60 ) Camlburg.nt;
        _Fdiv41: ( 't59 ) Camlburg.nt;
        _Fdiv40: ( 't58 ) Camlburg.nt;
        _Fadd45: ( 't57 ) Camlburg.nt;
        _Fadd44: ( 't56 ) Camlburg.nt;
        _Divu28: ( 't55 ) Camlburg.nt;
        _Com36: ( 't54 ) Camlburg.nt;
        _And31: ( 't53 ) Camlburg.nt;
        _And30: ( 't52 ) Camlburg.nt;
        _Addc57: ( 't51 ) Camlburg.nt;
        _Add53: ( 't50 ) Camlburg.nt;
        _Add50: ( 't49 ) Camlburg.nt;
        _Add18: ( 't48 ) Camlburg.nt;
        _Add17: ( 't47 ) Camlburg.nt;
        _Add15: ( 't46 ) Camlburg.nt;
        zero: ( 't45 ) Camlburg.nt;
        yregl: ( 't44 ) Camlburg.nt;
        yreg: ( 't43 ) Camlburg.nt;
        target: ( 't42 ) Camlburg.nt;
        spl: ( 't41 ) Camlburg.nt;
        sp: ( 't40 ) Camlburg.nt;
        rregl: ( 't39 ) Camlburg.nt;
        rreg: ( 't38 ) Camlburg.nt;
        result_regl: ( 't37 ) Camlburg.nt;
        result_reg: ( 't36 ) Camlburg.nt;
        restore: ( 't35 ) Camlburg.nt;
        regl: ( 't34 ) Camlburg.nt;
        reg_or_const: ( 't33 ) Camlburg.nt;
        reg: ( 't32 ) Camlburg.nt;
        ral: ( 't31 ) Camlburg.nt;
        ra: ( 't30 ) Camlburg.nt;
        pos: ( 't29 ) Camlburg.nt;
        pcl: ( 't28 ) Camlburg.nt;
        pc: ( 't27 ) Camlburg.nt;
        one: ( 't26 ) Camlburg.nt;
        npcl: ( 't25 ) Camlburg.nt;
        next: ( 't24 ) Camlburg.nt;
        neg: ( 't23 ) Camlburg.nt;
        meml: ( 't22 ) Camlburg.nt;
        mem: ( 't21 ) Camlburg.nt;
        lconst: ( 't20 ) Camlburg.nt;
        inst: ( 't19 ) Camlburg.nt;
        fsrl: ( 't18 ) Camlburg.nt;
        fsr: ( 't17 ) Camlburg.nt;
        fregl: ( 't16 ) Camlburg.nt;
        freg: ( 't15 ) Camlburg.nt;
        fpl: ( 't14 ) Camlburg.nt;
        four: ( 't13 ) Camlburg.nt;
        dregnuml: ( 't12 ) Camlburg.nt;
        dregnum: ( 't11 ) Camlburg.nt;
        dregl: ( 't10 ) Camlburg.nt;
        dreg: ( 't9 ) Camlburg.nt;
        cwpl: ( 't8 ) Camlburg.nt;
        cwp: ( 't7 ) Camlburg.nt;
        constx: ( 't6 ) Camlburg.nt;
        const: ( 't5 ) Camlburg.nt;
        ccl: ( 't4 ) Camlburg.nt;
        cc: ( 't3 ) Camlburg.nt;
        arg_regl: ( 't2 ) Camlburg.nt;
        arg_reg: ( 't1 ) Camlburg.nt;
        any: ( 't0 ) Camlburg.nt
    }

let rec
inf =
    {any = (Camlburg.infinity)
    ;arg_reg = (Camlburg.infinity)
    ;arg_regl = (Camlburg.infinity)
    ;cc = (Camlburg.infinity)
    ;ccl = (Camlburg.infinity)
    ;const = (Camlburg.infinity)
    ;constx = (Camlburg.infinity)
    ;cwp = (Camlburg.infinity)
    ;cwpl = (Camlburg.infinity)
    ;dreg = (Camlburg.infinity)
    ;dregl = (Camlburg.infinity)
    ;dregnum = (Camlburg.infinity)
    ;dregnuml = (Camlburg.infinity)
    ;four = (Camlburg.infinity)
    ;fpl = (Camlburg.infinity)
    ;freg = (Camlburg.infinity)
    ;fregl = (Camlburg.infinity)
    ;fsr = (Camlburg.infinity)
    ;fsrl = (Camlburg.infinity)
    ;inst = (Camlburg.infinity)
    ;lconst = (Camlburg.infinity)
    ;mem = (Camlburg.infinity)
    ;meml = (Camlburg.infinity)
    ;neg = (Camlburg.infinity)
    ;next = (Camlburg.infinity)
    ;npcl = (Camlburg.infinity)
    ;one = (Camlburg.infinity)
    ;pc = (Camlburg.infinity)
    ;pcl = (Camlburg.infinity)
    ;pos = (Camlburg.infinity)
    ;ra = (Camlburg.infinity)
    ;ral = (Camlburg.infinity)
    ;reg = (Camlburg.infinity)
    ;reg_or_const = (Camlburg.infinity)
    ;regl = (Camlburg.infinity)
    ;restore = (Camlburg.infinity)
    ;result_reg = (Camlburg.infinity)
    ;result_regl = (Camlburg.infinity)
    ;rreg = (Camlburg.infinity)
    ;rregl = (Camlburg.infinity)
    ;sp = (Camlburg.infinity)
    ;spl = (Camlburg.infinity)
    ;target = (Camlburg.infinity)
    ;yreg = (Camlburg.infinity)
    ;yregl = (Camlburg.infinity)
    ;zero = (Camlburg.infinity)
    ;_Add15 = (Camlburg.infinity)
    ;_Add17 = (Camlburg.infinity)
    ;_Add18 = (Camlburg.infinity)
    ;_Add50 = (Camlburg.infinity)
    ;_Add53 = (Camlburg.infinity)
    ;_Addc57 = (Camlburg.infinity)
    ;_And30 = (Camlburg.infinity)
    ;_And31 = (Camlburg.infinity)
    ;_Com36 = (Camlburg.infinity)
    ;_Divu28 = (Camlburg.infinity)
    ;_Fadd44 = (Camlburg.infinity)
    ;_Fadd45 = (Camlburg.infinity)
    ;_Fdiv40 = (Camlburg.infinity)
    ;_Fdiv41 = (Camlburg.infinity)
    ;_Fetch16 = (Camlburg.infinity)
    ;_Fmul42 = (Camlburg.infinity)
    ;_Fmul43 = (Camlburg.infinity)
    ;_Fneg48 = (Camlburg.infinity)
    ;_Fneg49 = (Camlburg.infinity)
    ;_Fsub46 = (Camlburg.infinity)
    ;_Fsub47 = (Camlburg.infinity)
    ;_Ftof6 = (Camlburg.infinity)
    ;_Ftof7 = (Camlburg.infinity)
    ;_Ftoi3 = (Camlburg.infinity)
    ;_Ftoi4 = (Camlburg.infinity)
    ;_Goto51 = (Camlburg.infinity)
    ;_Goto52 = (Camlburg.infinity)
    ;_Goto54 = (Camlburg.infinity)
    ;_Itof5 = (Camlburg.infinity)
    ;_Lobyte12 = (Camlburg.infinity)
    ;_Lohalf13 = (Camlburg.infinity)
    ;_Mem14 = (Camlburg.infinity)
    ;_Mul20 = (Camlburg.infinity)
    ;_Mul21 = (Camlburg.infinity)
    ;_Neg29 = (Camlburg.infinity)
    ;_Or32 = (Camlburg.infinity)
    ;_Or33 = (Camlburg.infinity)
    ;_Quot27 = (Camlburg.infinity)
    ;_Reg1 = (Camlburg.infinity)
    ;_Reg2 = (Camlburg.infinity)
    ;_Shl37 = (Camlburg.infinity)
    ;_Shra39 = (Camlburg.infinity)
    ;_Shrl38 = (Camlburg.infinity)
    ;_Sparcadcflags62 = (Camlburg.infinity)
    ;_Sparcaddcc56 = (Camlburg.infinity)
    ;_Sparccarrybit58 = (Camlburg.infinity)
    ;_Sparceq69 = (Camlburg.infinity)
    ;_Sparcfeq79 = (Camlburg.infinity)
    ;_Sparcfge81 = (Camlburg.infinity)
    ;_Sparcfgt82 = (Camlburg.infinity)
    ;_Sparcfle83 = (Camlburg.infinity)
    ;_Sparcflt84 = (Camlburg.infinity)
    ;_Sparcfne80 = (Camlburg.infinity)
    ;_Sparcge71 = (Camlburg.infinity)
    ;_Sparcgeu72 = (Camlburg.infinity)
    ;_Sparcgt73 = (Camlburg.infinity)
    ;_Sparcgtu74 = (Camlburg.infinity)
    ;_Sparcle75 = (Camlburg.infinity)
    ;_Sparcleu76 = (Camlburg.infinity)
    ;_Sparclt77 = (Camlburg.infinity)
    ;_Sparcltu78 = (Camlburg.infinity)
    ;_Sparcmuluxhi26 = (Camlburg.infinity)
    ;_Sparcmulxhi24 = (Camlburg.infinity)
    ;_Sparcne70 = (Camlburg.infinity)
    ;_Sparcsbbflags65 = (Camlburg.infinity)
    ;_Sparcsubcc66 = (Camlburg.infinity)
    ;_Sparcsubcc67 = (Camlburg.infinity)
    ;_Sparcsubcc68 = (Camlburg.infinity)
    ;_Store22 = (Camlburg.infinity)
    ;_Store23 = (Camlburg.infinity)
    ;_Store25 = (Camlburg.infinity)
    ;_Store55 = (Camlburg.infinity)
    ;_Store60 = (Camlburg.infinity)
    ;_Store61 = (Camlburg.infinity)
    ;_Store63 = (Camlburg.infinity)
    ;_Store64 = (Camlburg.infinity)
    ;_Sub19 = (Camlburg.infinity)
    ;_Subb59 = (Camlburg.infinity)
    ;_Sxbyte10 = (Camlburg.infinity)
    ;_Sxhalf11 = (Camlburg.infinity)
    ;_Xor34 = (Camlburg.infinity)
    ;_Xor35 = (Camlburg.infinity)
    ;_Zxbyte8 = (Camlburg.infinity)
    ;_Zxhalf9 = (Camlburg.infinity)
    }


let rec
update_any =
    fun nt x ->
        if nt.Camlburg.cost >= x.any.Camlburg.cost then
            x
        else
            (fun x ->
                (update_inst
                    {Camlburg.cost = (nt.Camlburg.cost + 1000)
                    ;Camlburg.action =
                        (fun () ->
                            let any = x.any.Camlburg.action ()
                            in
                                
# 595 "sparcrec.mlb"
                                ( s "<%s>" any )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with any = (nt) }
and update_arg_reg =
    fun nt x ->
        if nt.Camlburg.cost >= x.arg_reg.Camlburg.cost then
            x
        else
            (fun x ->
                (update_reg
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let arg_reg = x.arg_reg.Camlburg.action ()
                            in
                                
# 251 "sparcrec.mlb"
                                ( arg_reg )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with arg_reg = (nt) }
and update_arg_regl =
    fun nt x ->
        if nt.Camlburg.cost >= x.arg_regl.Camlburg.cost then
            x
        else
            (fun x ->
                (update_regl
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let arg_regl = x.arg_regl.Camlburg.action ()
                            in
                                
# 247 "sparcrec.mlb"
                                ( arg_regl )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with arg_regl = (nt) }
and update_cc =
    fun nt x ->
        if nt.Camlburg.cost >= x.cc.Camlburg.cost then
            x
        else
            { x with cc = (nt) }
and update_ccl =
    fun nt x ->
        if nt.Camlburg.cost >= x.ccl.Camlburg.cost then
            x
        else
            { x with ccl = (nt) }
and update_const =
    fun nt x ->
        if nt.Camlburg.cost >= x.const.Camlburg.cost then
            x
        else
            (fun x ->
                (update_reg_or_const
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let const = x.const.Camlburg.action ()
                            in
                                
# 257 "sparcrec.mlb"
                                ( const )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with const = (nt) }
and update_constx =
    fun nt x ->
        if nt.Camlburg.cost >= x.constx.Camlburg.cost then
            x
        else
            { x with constx = (nt) }
and update_cwp =
    fun nt x ->
        if nt.Camlburg.cost >= x.cwp.Camlburg.cost then
            x
        else
            { x with cwp = (nt) }
and update_cwpl =
    fun nt x ->
        if nt.Camlburg.cost >= x.cwpl.Camlburg.cost then
            x
        else
            { x with cwpl = (nt) }
and update_dreg =
    fun nt x ->
        if nt.Camlburg.cost >= x.dreg.Camlburg.cost then
            x
        else
            { x with dreg = (nt) }
and update_dregl =
    fun nt x ->
        if nt.Camlburg.cost >= x.dregl.Camlburg.cost then
            x
        else
            { x with dregl = (nt) }
and update_dregnum =
    fun nt x ->
        if nt.Camlburg.cost >= x.dregnum.Camlburg.cost then
            x
        else
            { x with dregnum = (nt) }
and update_dregnuml =
    fun nt x ->
        if nt.Camlburg.cost >= x.dregnuml.Camlburg.cost then
            x
        else
            { x with dregnuml = (nt) }
and update_four =
    fun nt x ->
        if nt.Camlburg.cost >= x.four.Camlburg.cost then
            x
        else
            { x with four = (nt) }
and update_fpl =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpl.Camlburg.cost then
            x
        else
            { x with fpl = (nt) }
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
and update_fsr =
    fun nt x ->
        if nt.Camlburg.cost >= x.fsr.Camlburg.cost then
            x
        else
            { x with fsr = (nt) }
and update_fsrl =
    fun nt x ->
        if nt.Camlburg.cost >= x.fsrl.Camlburg.cost then
            x
        else
            { x with fsrl = (nt) }
and update_inst =
    fun nt x ->
        if nt.Camlburg.cost >= x.inst.Camlburg.cost then
            x
        else
            { x with inst = (nt) }
and update_lconst =
    fun nt x ->
        if nt.Camlburg.cost >= x.lconst.Camlburg.cost then
            x
        else
            (fun x ->
                (update_target
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let lconst = x.lconst.Camlburg.action ()
                            in
                                
# 259 "sparcrec.mlb"
                                ( lconst )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with lconst = (nt) }
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
and update_neg =
    fun nt x ->
        if nt.Camlburg.cost >= x.neg.Camlburg.cost then
            x
        else
            { x with neg = (nt) }
and update_next =
    fun nt x ->
        if nt.Camlburg.cost >= x.next.Camlburg.cost then
            x
        else
            { x with next = (nt) }
and update_npcl =
    fun nt x ->
        if nt.Camlburg.cost >= x.npcl.Camlburg.cost then
            x
        else
            { x with npcl = (nt) }
and update_one =
    fun nt x ->
        if nt.Camlburg.cost >= x.one.Camlburg.cost then
            x
        else
            { x with one = (nt) }
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
and update_pos =
    fun nt x ->
        if nt.Camlburg.cost >= x.pos.Camlburg.cost then
            x
        else
            { x with pos = (nt) }
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
                (update_reg_or_const
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let reg = x.reg.Camlburg.action ()
                            in
                                
# 256 "sparcrec.mlb"
                                ( reg )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with reg = (nt) }
and update_reg_or_const =
    fun nt x ->
        if nt.Camlburg.cost >= x.reg_or_const.Camlburg.cost then
            x
        else
            { x with reg_or_const = (nt) }
and update_regl =
    fun nt x ->
        if nt.Camlburg.cost >= x.regl.Camlburg.cost then
            x
        else
            { x with regl = (nt) }
and update_restore =
    fun nt x ->
        if nt.Camlburg.cost >= x.restore.Camlburg.cost then
            x
        else
            { x with restore = (nt) }
and update_result_reg =
    fun nt x ->
        if nt.Camlburg.cost >= x.result_reg.Camlburg.cost then
            x
        else
            (fun x ->
                (update_reg
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let result_reg = x.result_reg.Camlburg.action ()
                            in
                                
# 250 "sparcrec.mlb"
                                ( result_reg )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with result_reg = (nt) }
and update_result_regl =
    fun nt x ->
        if nt.Camlburg.cost >= x.result_regl.Camlburg.cost then
            x
        else
            (fun x ->
                (update_regl
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let
                                result_regl =
                                x.result_regl.Camlburg.action ()
                            in
                                
# 246 "sparcrec.mlb"
                                ( result_regl )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with result_regl = (nt) }
and update_rreg =
    fun nt x ->
        if nt.Camlburg.cost >= x.rreg.Camlburg.cost then
            x
        else
            (fun x ->
                (update_reg
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let rreg = x.rreg.Camlburg.action ()
                            in
                                
# 249 "sparcrec.mlb"
                                ( rreg )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with rreg = (nt) }
and update_rregl =
    fun nt x ->
        if nt.Camlburg.cost >= x.rregl.Camlburg.cost then
            x
        else
            (fun x ->
                (update_regl
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let rregl = x.rregl.Camlburg.action ()
                            in
                                
# 245 "sparcrec.mlb"
                                ( rregl )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with rregl = (nt) }
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
and update_target =
    fun nt x ->
        if nt.Camlburg.cost >= x.target.Camlburg.cost then
            x
        else
            { x with target = (nt) }
and update_yreg =
    fun nt x ->
        if nt.Camlburg.cost >= x.yreg.Camlburg.cost then
            x
        else
            { x with yreg = (nt) }
and update_yregl =
    fun nt x ->
        if nt.Camlburg.cost >= x.yregl.Camlburg.cost then
            x
        else
            { x with yregl = (nt) }
and update_zero =
    fun nt x ->
        if nt.Camlburg.cost >= x.zero.Camlburg.cost then
            x
        else
            { x with zero = (nt) }
and update__Add15 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add15.Camlburg.cost then
            x
        else
            { x with _Add15 = (nt) }
and update__Add17 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add17.Camlburg.cost then
            x
        else
            { x with _Add17 = (nt) }
and update__Add18 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add18.Camlburg.cost then
            x
        else
            { x with _Add18 = (nt) }
and update__Add50 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add50.Camlburg.cost then
            x
        else
            { x with _Add50 = (nt) }
and update__Add53 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add53.Camlburg.cost then
            x
        else
            { x with _Add53 = (nt) }
and update__Addc57 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Addc57.Camlburg.cost then
            x
        else
            { x with _Addc57 = (nt) }
and update__And30 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And30.Camlburg.cost then
            x
        else
            { x with _And30 = (nt) }
and update__And31 =
    fun nt x ->
        if nt.Camlburg.cost >= x._And31.Camlburg.cost then
            x
        else
            { x with _And31 = (nt) }
and update__Com36 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Com36.Camlburg.cost then
            x
        else
            { x with _Com36 = (nt) }
and update__Divu28 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Divu28.Camlburg.cost then
            x
        else
            { x with _Divu28 = (nt) }
and update__Fadd44 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fadd44.Camlburg.cost then
            x
        else
            { x with _Fadd44 = (nt) }
and update__Fadd45 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fadd45.Camlburg.cost then
            x
        else
            { x with _Fadd45 = (nt) }
and update__Fdiv40 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fdiv40.Camlburg.cost then
            x
        else
            { x with _Fdiv40 = (nt) }
and update__Fdiv41 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fdiv41.Camlburg.cost then
            x
        else
            { x with _Fdiv41 = (nt) }
and update__Fetch16 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch16.Camlburg.cost then
            x
        else
            { x with _Fetch16 = (nt) }
and update__Fmul42 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fmul42.Camlburg.cost then
            x
        else
            { x with _Fmul42 = (nt) }
and update__Fmul43 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fmul43.Camlburg.cost then
            x
        else
            { x with _Fmul43 = (nt) }
and update__Fneg48 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fneg48.Camlburg.cost then
            x
        else
            { x with _Fneg48 = (nt) }
and update__Fneg49 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fneg49.Camlburg.cost then
            x
        else
            { x with _Fneg49 = (nt) }
and update__Fsub46 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fsub46.Camlburg.cost then
            x
        else
            { x with _Fsub46 = (nt) }
and update__Fsub47 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fsub47.Camlburg.cost then
            x
        else
            { x with _Fsub47 = (nt) }
and update__Ftof6 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Ftof6.Camlburg.cost then
            x
        else
            { x with _Ftof6 = (nt) }
and update__Ftof7 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Ftof7.Camlburg.cost then
            x
        else
            { x with _Ftof7 = (nt) }
and update__Ftoi3 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Ftoi3.Camlburg.cost then
            x
        else
            { x with _Ftoi3 = (nt) }
and update__Ftoi4 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Ftoi4.Camlburg.cost then
            x
        else
            { x with _Ftoi4 = (nt) }
and update__Goto51 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto51.Camlburg.cost then
            x
        else
            { x with _Goto51 = (nt) }
and update__Goto52 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto52.Camlburg.cost then
            x
        else
            { x with _Goto52 = (nt) }
and update__Goto54 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto54.Camlburg.cost then
            x
        else
            { x with _Goto54 = (nt) }
and update__Itof5 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Itof5.Camlburg.cost then
            x
        else
            { x with _Itof5 = (nt) }
and update__Lobyte12 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Lobyte12.Camlburg.cost then
            x
        else
            { x with _Lobyte12 = (nt) }
and update__Lohalf13 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Lohalf13.Camlburg.cost then
            x
        else
            { x with _Lohalf13 = (nt) }
and update__Mem14 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mem14.Camlburg.cost then
            x
        else
            { x with _Mem14 = (nt) }
and update__Mul20 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul20.Camlburg.cost then
            x
        else
            { x with _Mul20 = (nt) }
and update__Mul21 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mul21.Camlburg.cost then
            x
        else
            { x with _Mul21 = (nt) }
and update__Neg29 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Neg29.Camlburg.cost then
            x
        else
            { x with _Neg29 = (nt) }
and update__Or32 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Or32.Camlburg.cost then
            x
        else
            { x with _Or32 = (nt) }
and update__Or33 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Or33.Camlburg.cost then
            x
        else
            { x with _Or33 = (nt) }
and update__Quot27 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Quot27.Camlburg.cost then
            x
        else
            { x with _Quot27 = (nt) }
and update__Reg1 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Reg1.Camlburg.cost then
            x
        else
            { x with _Reg1 = (nt) }
and update__Reg2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Reg2.Camlburg.cost then
            x
        else
            { x with _Reg2 = (nt) }
and update__Shl37 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Shl37.Camlburg.cost then
            x
        else
            { x with _Shl37 = (nt) }
and update__Shra39 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Shra39.Camlburg.cost then
            x
        else
            { x with _Shra39 = (nt) }
and update__Shrl38 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Shrl38.Camlburg.cost then
            x
        else
            { x with _Shrl38 = (nt) }
and update__Sparcadcflags62 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcadcflags62.Camlburg.cost then
            x
        else
            { x with _Sparcadcflags62 = (nt) }
and update__Sparcaddcc56 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcaddcc56.Camlburg.cost then
            x
        else
            { x with _Sparcaddcc56 = (nt) }
and update__Sparccarrybit58 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparccarrybit58.Camlburg.cost then
            x
        else
            { x with _Sparccarrybit58 = (nt) }
and update__Sparceq69 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparceq69.Camlburg.cost then
            x
        else
            { x with _Sparceq69 = (nt) }
and update__Sparcfeq79 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcfeq79.Camlburg.cost then
            x
        else
            { x with _Sparcfeq79 = (nt) }
and update__Sparcfge81 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcfge81.Camlburg.cost then
            x
        else
            { x with _Sparcfge81 = (nt) }
and update__Sparcfgt82 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcfgt82.Camlburg.cost then
            x
        else
            { x with _Sparcfgt82 = (nt) }
and update__Sparcfle83 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcfle83.Camlburg.cost then
            x
        else
            { x with _Sparcfle83 = (nt) }
and update__Sparcflt84 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcflt84.Camlburg.cost then
            x
        else
            { x with _Sparcflt84 = (nt) }
and update__Sparcfne80 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcfne80.Camlburg.cost then
            x
        else
            { x with _Sparcfne80 = (nt) }
and update__Sparcge71 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcge71.Camlburg.cost then
            x
        else
            { x with _Sparcge71 = (nt) }
and update__Sparcgeu72 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcgeu72.Camlburg.cost then
            x
        else
            { x with _Sparcgeu72 = (nt) }
and update__Sparcgt73 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcgt73.Camlburg.cost then
            x
        else
            { x with _Sparcgt73 = (nt) }
and update__Sparcgtu74 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcgtu74.Camlburg.cost then
            x
        else
            { x with _Sparcgtu74 = (nt) }
and update__Sparcle75 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcle75.Camlburg.cost then
            x
        else
            { x with _Sparcle75 = (nt) }
and update__Sparcleu76 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcleu76.Camlburg.cost then
            x
        else
            { x with _Sparcleu76 = (nt) }
and update__Sparclt77 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparclt77.Camlburg.cost then
            x
        else
            { x with _Sparclt77 = (nt) }
and update__Sparcltu78 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcltu78.Camlburg.cost then
            x
        else
            { x with _Sparcltu78 = (nt) }
and update__Sparcmuluxhi26 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcmuluxhi26.Camlburg.cost then
            x
        else
            { x with _Sparcmuluxhi26 = (nt) }
and update__Sparcmulxhi24 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcmulxhi24.Camlburg.cost then
            x
        else
            { x with _Sparcmulxhi24 = (nt) }
and update__Sparcne70 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcne70.Camlburg.cost then
            x
        else
            { x with _Sparcne70 = (nt) }
and update__Sparcsbbflags65 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcsbbflags65.Camlburg.cost then
            x
        else
            { x with _Sparcsbbflags65 = (nt) }
and update__Sparcsubcc66 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcsubcc66.Camlburg.cost then
            x
        else
            { x with _Sparcsubcc66 = (nt) }
and update__Sparcsubcc67 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcsubcc67.Camlburg.cost then
            x
        else
            { x with _Sparcsubcc67 = (nt) }
and update__Sparcsubcc68 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sparcsubcc68.Camlburg.cost then
            x
        else
            { x with _Sparcsubcc68 = (nt) }
and update__Store22 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store22.Camlburg.cost then
            x
        else
            { x with _Store22 = (nt) }
and update__Store23 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store23.Camlburg.cost then
            x
        else
            { x with _Store23 = (nt) }
and update__Store25 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store25.Camlburg.cost then
            x
        else
            { x with _Store25 = (nt) }
and update__Store55 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store55.Camlburg.cost then
            x
        else
            { x with _Store55 = (nt) }
and update__Store60 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store60.Camlburg.cost then
            x
        else
            { x with _Store60 = (nt) }
and update__Store61 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store61.Camlburg.cost then
            x
        else
            { x with _Store61 = (nt) }
and update__Store63 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store63.Camlburg.cost then
            x
        else
            { x with _Store63 = (nt) }
and update__Store64 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store64.Camlburg.cost then
            x
        else
            { x with _Store64 = (nt) }
and update__Sub19 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub19.Camlburg.cost then
            x
        else
            { x with _Sub19 = (nt) }
and update__Subb59 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Subb59.Camlburg.cost then
            x
        else
            { x with _Subb59 = (nt) }
and update__Sxbyte10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sxbyte10.Camlburg.cost then
            x
        else
            { x with _Sxbyte10 = (nt) }
and update__Sxhalf11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sxhalf11.Camlburg.cost then
            x
        else
            { x with _Sxhalf11 = (nt) }
and update__Xor34 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Xor34.Camlburg.cost then
            x
        else
            { x with _Xor34 = (nt) }
and update__Xor35 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Xor35.Camlburg.cost then
            x
        else
            { x with _Xor35 = (nt) }
and update__Zxbyte8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zxbyte8.Camlburg.cost then
            x
        else
            { x with _Zxbyte8 = (nt) }
and update__Zxhalf9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zxhalf9.Camlburg.cost then
            x
        else
            { x with _Zxhalf9 = (nt) }


let rec
conZxhalf =
    fun arg1 ->
        (update__Zxhalf9
            {Camlburg.cost = (arg1.mem.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let src = arg1.mem.Camlburg.action () in src)
            })
            inf
and conZxbyte =
    fun arg1 ->
        (update__Zxbyte8
            {Camlburg.cost = (arg1.mem.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let src = arg1.mem.Camlburg.action () in src)
            })
            inf
and conZx =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 629 "sparcrec.mlb"
                        ( s "Zx(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conXor =
    fun arg1 arg2 ->
        (update__Xor34
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Xor35
                {Camlburg.cost =
                    (arg1.reg_or_const.Camlburg.cost
                    +
                    arg2.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg_or_const.Camlburg.action ()
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
                                
# 608 "sparcrec.mlb"
                                ( s "Xor(%s, %s)" x y )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conTrue =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 597 "sparcrec.mlb"
                    ( "True"  )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conSxhalf =
    fun arg1 ->
        (update__Sxhalf11
            {Camlburg.cost = (arg1.mem.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let src = arg1.mem.Camlburg.action () in src)
            })
            inf
and conSxbyte =
    fun arg1 ->
        (update__Sxbyte10
            {Camlburg.cost = (arg1.mem.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let src = arg1.mem.Camlburg.action () in src)
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
                        
# 628 "sparcrec.mlb"
                        ( s "Sx(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conSubb =
    fun arg1 arg2 arg3 ->
        (update__Subb59
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost
                +
                arg3._Sparccarrybit58.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    and _v1 = arg3._Sparccarrybit58.Camlburg.action ()
                    in
                        let cc = _v1 in (x ,y ,cc))
            })
            inf
and conSub =
    fun arg1 arg2 ->
        (update__Sub19
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            inf
and conStore =
    fun arg1 arg2 arg3 ->
        (update__Store22
            {Camlburg.cost =
                (arg1.regl.Camlburg.cost + arg2._Mul20.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let dst = arg1.regl.Camlburg.action ()
                    and _v1 = arg2._Mul20.Camlburg.action ()
                    and w = arg3
                    in
                        let (x, y) = _v1 in (dst ,x ,y ,w))
            })
            ((update__Store23
                {Camlburg.cost =
                    (arg1.yregl.Camlburg.cost
                    +
                    arg2._Sparcmulxhi24.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let yregl = arg1.yregl.Camlburg.action ()
                        and _v1 = arg2._Sparcmulxhi24.Camlburg.action ()
                        and w = arg3
                        in
                            let (x2, y2) = _v1 in (yregl ,x2 ,y2 ,w))
                })
                ((update__Store25
                    {Camlburg.cost =
                        (arg1.yregl.Camlburg.cost
                        +
                        arg2._Sparcmuluxhi26.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let yregl = arg1.yregl.Camlburg.action ()
                            and _v1 = arg2._Sparcmuluxhi26.Camlburg.action ()
                            and w = arg3
                            in
                                let (x2, y2) = _v1 in (yregl ,x2 ,y2 ,w))
                    })
                    ((update__Store55
                        {Camlburg.cost =
                            (arg1.spl.Camlburg.cost + arg2.reg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let spl = arg1.spl.Camlburg.action ()
                                and reg = arg2.reg.Camlburg.action ()
                                and w = arg3
                                in
                                    (spl ,reg ,w))
                        })
                        ((update__Store60
                            {Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Addc57.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let d = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Addc57.Camlburg.action ()
                                    and w = arg3
                                    in
                                        let (x, y, cc) = _v1
                                        in
                                            (d ,x ,y ,cc ,w))
                            })
                            ((update__Store61
                                {Camlburg.cost =
                                    (arg1.ccl.Camlburg.cost
                                    +
                                    arg2._Sparcadcflags62.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let ccl = arg1.ccl.Camlburg.action ()
                                        and
                                            _v1 =
                                            arg2._Sparcadcflags62.Camlburg.action
                                                ()
                                        and w = arg3
                                        in
                                            let (x, y, cc) = _v1
                                            in
                                                (ccl ,x ,y ,cc ,w))
                                })
                                ((update__Store63
                                    {Camlburg.cost =
                                        (arg1.regl.Camlburg.cost
                                        +
                                        arg2._Subb59.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                d =
                                                arg1.regl.Camlburg.action ()
                                            and
                                                _v1 =
                                                arg2._Subb59.Camlburg.action
                                                    ()
                                            and w = arg3
                                            in
                                                let (x, y, cc) = _v1
                                                in
                                                    (d ,x ,y ,cc ,w))
                                    })
                                    ((update__Store64
                                        {Camlburg.cost =
                                            (arg1.ccl.Camlburg.cost
                                            +
                                            arg2._Sparcsbbflags65.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    ccl =
                                                    arg1.ccl.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._Sparcsbbflags65.Camlburg.action
                                                        ()
                                                and w = arg3
                                                in
                                                    let (x, y, cc) = _v1
                                                    in
                                                        (ccl ,x ,y ,cc ,w))
                                        })
                                        ((update_any
                                            {Camlburg.cost =
                                                (arg1.any.Camlburg.cost
                                                +
                                                arg2.any.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        dst =
                                                        arg1.any.Camlburg.action
                                                            ()
                                                    and
                                                        src =
                                                        arg2.any.Camlburg.action
                                                            ()
                                                    and w = arg3
                                                    in
                                                        
# 638 "sparcrec.mlb"
                                                        ( s "Store(%s,%s,%d)" dst src w )
                                                        
# 000 "/dev/stdout"
)
                                            })
                                            ((update_inst
                                                (Camlburg.choice
                                                    [{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Ftoi3.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Ftoi3.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 276 "sparcrec.mlb"
                                                                    ( sprintf "fstoi %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Ftoi4.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Ftoi4.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 279 "sparcrec.mlb"
                                                                    ( sprintf "fdtoi %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Itof5.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Itof5.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 282 "sparcrec.mlb"
                                                                    ( sprintf "fitos %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregl.Camlburg.cost
                                                        +
                                                        arg2._Itof5.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Itof5.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 284 "sparcrec.mlb"
                                                                    ( sprintf "fitod %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Ftof6.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Ftof6.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 287 "sparcrec.mlb"
                                                                    ( sprintf "fmovs %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregl.Camlburg.cost
                                                        +
                                                        arg2._Ftof6.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Ftof6.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 289 "sparcrec.mlb"
                                                                    ( sprintf "fstod %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Ftof7.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Ftof7.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 291 "sparcrec.mlb"
                                                                    ( sprintf "fdtos %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2.lconst.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.lconst.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 295 "sparcrec.mlb"
                                                                ( sprintf "set %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2.const.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.const.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 299 "sparcrec.mlb"
                                                                ( sprintf "set %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2.reg.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.reg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 303 "sparcrec.mlb"
                                                                ( sprintf "mov %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2.freg.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.freg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 306 "sparcrec.mlb"
                                                                ( sprintf "fmovs %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2.yreg.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.yreg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 309 "sparcrec.mlb"
                                                                ( sprintf "rd %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregnuml.Camlburg.cost
                                                        +
                                                        arg2.dregnum.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregnuml.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.dregnum.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 313 "sparcrec.mlb"
                                                                ( sprintf "fmovs %%f%d, %%f%d\nfmovs %%f%d, %%f%d" src dst (src+1) (dst+1))
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2.mem.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.mem.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 317 "sparcrec.mlb"
                                                                ( if w = 64 then sprintf "ldx %s, %s" src dst
     else sprintf "ld %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Zxbyte8.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Zxbyte8.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 320 "sparcrec.mlb"
                                                                    ( sprintf "ldub %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Zxhalf9.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Zxhalf9.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 322 "sparcrec.mlb"
                                                                    ( sprintf "lduh %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Sxbyte10.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Sxbyte10.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 325 "sparcrec.mlb"
                                                                    ( sprintf "ldsb %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Sxhalf11.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Sxhalf11.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 327 "sparcrec.mlb"
                                                                    ( sprintf "ldsh %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2.mem.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.mem.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 330 "sparcrec.mlb"
                                                                ( sprintf "ld %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregl.Camlburg.cost
                                                        +
                                                        arg2.mem.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregl.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.mem.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 332 "sparcrec.mlb"
                                                                ( sprintf "ldd %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fsrl.Camlburg.cost
                                                        +
                                                        arg2.mem.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fsrl.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.mem.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 335 "sparcrec.mlb"
                                                                ( sprintf "ld %s, %s" src dst )
                                                                
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
                                                                dst =
                                                                arg1.meml.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.reg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 339 "sparcrec.mlb"
                                                                ( sprintf "st %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.meml.Camlburg.cost
                                                        +
                                                        arg2._Lobyte12.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.meml.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Lobyte12.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 341 "sparcrec.mlb"
                                                                    ( sprintf "stb %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.meml.Camlburg.cost
                                                        +
                                                        arg2._Lohalf13.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.meml.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Lohalf13.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let src = _v1
                                                                in
                                                                    
# 343 "sparcrec.mlb"
                                                                    ( sprintf "sth %s, %s" src dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.meml.Camlburg.cost
                                                        +
                                                        arg2.freg.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.meml.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.freg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 347 "sparcrec.mlb"
                                                                ( sprintf "st %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.meml.Camlburg.cost
                                                        +
                                                        arg2.dreg.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.meml.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.dreg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 349 "sparcrec.mlb"
                                                                ( sprintf "std %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.meml.Camlburg.cost
                                                        +
                                                        arg2.fsr.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.meml.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.fsr.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 352 "sparcrec.mlb"
                                                                ( sprintf "st %s, %s" src dst )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1._Mem14.Camlburg.cost
                                                        +
                                                        arg2.reg.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                _v1 =
                                                                arg1._Mem14.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.reg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    mw) =
                                                                    _v1
                                                                in
                                                                    
# 358 "sparcrec.mlb"
                                                                    ( sprintf "st %s, [%s+%s]" src x y )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1._Mem14.Camlburg.cost
                                                        +
                                                        arg2.freg.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                _v1 =
                                                                arg1._Mem14.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.freg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    mw) =
                                                                    _v1
                                                                in
                                                                    
# 360 "sparcrec.mlb"
                                                                    ( sprintf "st %s, [%s+%s]" src x y )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1._Mem14.Camlburg.cost
                                                        +
                                                        arg2.dreg.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                _v1 =
                                                                arg1._Mem14.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.dreg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    mw) =
                                                                    _v1
                                                                in
                                                                    
# 362 "sparcrec.mlb"
                                                                    ( sprintf "std %s, [%s+%s]" src x y )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1._Mem14.Camlburg.cost
                                                        +
                                                        arg2.fsr.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                _v1 =
                                                                arg1._Mem14.Camlburg.action
                                                                    ()
                                                            and
                                                                src =
                                                                arg2.fsr.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    mw) =
                                                                    _v1
                                                                in
                                                                    
# 365 "sparcrec.mlb"
                                                                    ( sprintf "st %s, [%s+%s]" src x y )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Fetch16.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fetch16.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    mw,
                                                                    w) =
                                                                    _v1
                                                                in
                                                                    
# 369 "sparcrec.mlb"
                                                                    ( sprintf "ld [%s+%s], %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Fetch16.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fetch16.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    mw,
                                                                    w) =
                                                                    _v1
                                                                in
                                                                    
# 371 "sparcrec.mlb"
                                                                    ( sprintf "ld [%s+%s], %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregl.Camlburg.cost
                                                        +
                                                        arg2._Fetch16.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fetch16.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    mw,
                                                                    w) =
                                                                    _v1
                                                                in
                                                                    
# 374 "sparcrec.mlb"
                                                                    ( sprintf "ldd [%s+%s], %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fsrl.Camlburg.cost
                                                        +
                                                        arg2._Fetch16.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fsrl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fetch16.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    mw,
                                                                    w) =
                                                                    _v1
                                                                in
                                                                    
# 377 "sparcrec.mlb"
                                                                    ( sprintf "ld [%s+%s], %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Add17.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Add17.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 381 "sparcrec.mlb"
                                                                    ( sprintf "add %s, %s, %s" y x dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Add18.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Add18.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 383 "sparcrec.mlb"
                                                                    ( sprintf "add %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Sub19.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Sub19.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 387 "sparcrec.mlb"
                                                                    ( sprintf "sub %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Mul20.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Mul20.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 391 "sparcrec.mlb"
                                                                    ( sprintf "smul %s, %s, %s" y x dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Mul21.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Mul21.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 393 "sparcrec.mlb"
                                                                    ( sprintf "smul %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Quot27.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Quot27.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 411 "sparcrec.mlb"
                                                                    ( sprintf "sdiv %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Divu28.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Divu28.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 415 "sparcrec.mlb"
                                                                    ( sprintf "udiv %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Neg29.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Neg29.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let x = _v1
                                                                in
                                                                    
# 419 "sparcrec.mlb"
                                                                    ( sprintf "neg %s, %s" x dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._And30.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._And30.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 423 "sparcrec.mlb"
                                                                    ( sprintf "and %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._And31.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._And31.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 425 "sparcrec.mlb"
                                                                    ( sprintf "and %s, %s, %s" y x dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Or32.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Or32.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 429 "sparcrec.mlb"
                                                                    ( sprintf "or %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Or33.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Or33.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 431 "sparcrec.mlb"
                                                                    ( sprintf "or %s, %s, %s" y x dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Xor34.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Xor34.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 435 "sparcrec.mlb"
                                                                    ( sprintf "xor %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Xor35.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Xor35.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 437 "sparcrec.mlb"
                                                                    ( sprintf "xor %s, %s, %s" y x dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Com36.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Com36.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let x = _v1
                                                                in
                                                                    
# 441 "sparcrec.mlb"
                                                                    ( sprintf "not %s, %s" x dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Shl37.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Shl37.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 445 "sparcrec.mlb"
                                                                    ( sprintf "sll %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Shrl38.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Shrl38.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 449 "sparcrec.mlb"
                                                                    ( sprintf "srl %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Shra39.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Shra39.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 453 "sparcrec.mlb"
                                                                    ( sprintf "sra %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Fdiv40.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fdiv40.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 457 "sparcrec.mlb"
                                                                    ( sprintf "fdivs %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregl.Camlburg.cost
                                                        +
                                                        arg2._Fdiv41.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fdiv41.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 459 "sparcrec.mlb"
                                                                    ( sprintf "fdivd %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Fmul42.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fmul42.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 463 "sparcrec.mlb"
                                                                    ( sprintf "fmuls %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregl.Camlburg.cost
                                                        +
                                                        arg2._Fmul43.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fmul43.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 465 "sparcrec.mlb"
                                                                    ( sprintf "fmuld %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Fadd44.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fadd44.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 469 "sparcrec.mlb"
                                                                    ( sprintf "fadds %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregl.Camlburg.cost
                                                        +
                                                        arg2._Fadd45.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fadd45.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 471 "sparcrec.mlb"
                                                                    ( sprintf "faddd %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Fsub46.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fsub46.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 475 "sparcrec.mlb"
                                                                    ( sprintf "fsubs %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregl.Camlburg.cost
                                                        +
                                                        arg2._Fsub47.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fsub47.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 477 "sparcrec.mlb"
                                                                    ( sprintf "fsubd %s, %s, %s" x y dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.fregl.Camlburg.cost
                                                        +
                                                        arg2._Fneg48.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.fregl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fneg48.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let x = _v1
                                                                in
                                                                    
# 481 "sparcrec.mlb"
                                                                    ( sprintf "fnegs %s, %s" x dst )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.dregnuml.Camlburg.cost
                                                        +
                                                        arg2._Fneg49.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                dst =
                                                                arg1.dregnuml.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fneg49.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let x = _v1
                                                                in
                                                                    
# 484 "sparcrec.mlb"
                                                                    ( sprintf "fnegs %%f%d, %%f%d\n\tfmovs %%f%d, %%f%d" x dst (x+1) (dst+1) )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.cwpl.Camlburg.cost
                                                        +
                                                        arg2.zero.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                cwpl =
                                                                arg1.cwpl.Camlburg.action
                                                                    ()
                                                            and
                                                                zero =
                                                                arg2.zero.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 514 "sparcrec.mlb"
                                                                ( "ta 3" )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.ccl.Camlburg.cost
                                                        +
                                                        arg2._Sparcaddcc56.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                ccl =
                                                                arg1.ccl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Sparcaddcc56.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 528 "sparcrec.mlb"
                                                                    ( sprintf "addcc %s, %s, %%g0" x y )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Addc57.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                d =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Addc57.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    cc) =
                                                                    _v1
                                                                in
                                                                    
# 531 "sparcrec.mlb"
                                                                    ( sprintf "addx %s, %s, %s" x y d )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Subb59.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                d =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Subb59.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x,
                                                                    y,
                                                                    cc) =
                                                                    _v1
                                                                in
                                                                    
# 534 "sparcrec.mlb"
                                                                    ( sprintf "subx %s, %s, %s" x y d )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.ccl.Camlburg.cost
                                                        +
                                                        arg2._Sparcsubcc66.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                ccl =
                                                                arg1.ccl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Sparcsubcc66.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 547 "sparcrec.mlb"
                                                                    ( sprintf "subcc %s, %s, %%g0" x y )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.ccl.Camlburg.cost
                                                        +
                                                        arg2._Sparcsubcc67.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                ccl =
                                                                arg1.ccl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Sparcsubcc67.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 549 "sparcrec.mlb"
                                                                    ( sprintf "fcmps %s, %s\n\tnop" x y )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.ccl.Camlburg.cost
                                                        +
                                                        arg2._Sparcsubcc68.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                ccl =
                                                                arg1.ccl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Sparcsubcc68.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (x, y) =
                                                                    _v1
                                                                in
                                                                    
# 551 "sparcrec.mlb"
                                                                    ( sprintf "fcmpd %s, %s\n\tnop" x y )
                                                                    
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.yregl.Camlburg.cost
                                                        +
                                                        arg2.yreg.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                yregl =
                                                                arg1.yregl.Camlburg.action
                                                                    ()
                                                            and
                                                                yreg =
                                                                arg2.yreg.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                
# 594 "sparcrec.mlb"
                                                                ( "! y register self-store (no-op)" )
                                                                
# 000 "/dev/stdout"
)
                                                    }]))
                                                ((update_next
                                                    {Camlburg.cost =
                                                        (arg1.regl.Camlburg.cost
                                                        +
                                                        arg2._Add50.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                regl =
                                                                arg1.regl.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Add50.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (pc,
                                                                    four) =
                                                                    _v1
                                                                in
                                                                    
# 487 "sparcrec.mlb"
                                                                    ( regl )
                                                                    
# 000 "/dev/stdout"
)
                                                    })
                                                    ((update_restore
                                                        {Camlburg.cost =
                                                            (arg1.cwpl.Camlburg.cost
                                                            +
                                                            arg2._Add53.Camlburg.cost)
                                                        ;Camlburg.action =
                                                            (fun () ->
                                                                let
                                                                    cwpl =
                                                                    arg1.cwpl.Camlburg.action
                                                                        ()
                                                                and
                                                                    _v1 =
                                                                    arg2._Add53.Camlburg.action
                                                                        ()
                                                                and w = arg3
                                                                in
                                                                    let
                                                                        (cwp,
                                                                        one) =
                                                                        _v1
                                                                    in
                                                                        
# 509 "sparcrec.mlb"
                                                                        ( () )
                                                                        
# 000 "/dev/stdout"
)
                                                        })
                                                        inf)))))))))))
and conSparcsubcc =
    fun arg1 arg2 ->
        (update__Sparcsubcc66
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Sparcsubcc67
                {Camlburg.cost =
                    (arg1.freg.Camlburg.cost + arg2.freg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.freg.Camlburg.action ()
                        and y = arg2.freg.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update__Sparcsubcc68
                    {Camlburg.cost =
                        (arg1.dreg.Camlburg.cost + arg2.dreg.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.dreg.Camlburg.action ()
                            and y = arg2.dreg.Camlburg.action ()
                            in
                                (x ,y))
                    })
                    inf))
and conSparcsbbflags =
    fun arg1 arg2 arg3 ->
        (update__Sparcsbbflags65
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost
                +
                arg3._Sparccarrybit58.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    and _v1 = arg3._Sparccarrybit58.Camlburg.action ()
                    in
                        let cc = _v1 in (x ,y ,cc))
            })
            inf
and conSparcne =
    fun arg1 ->
        (update__Sparcne70
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcmulxhi =
    fun arg1 arg2 ->
        (update__Sparcmulxhi24
            {Camlburg.cost =
                (arg1.reg_or_const.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x2 = arg1.reg_or_const.Camlburg.action ()
                    and y2 = arg2.reg.Camlburg.action ()
                    in
                        (x2 ,y2))
            })
            inf
and conSparcmuluxhi =
    fun arg1 arg2 ->
        (update__Sparcmuluxhi26
            {Camlburg.cost =
                (arg1.reg_or_const.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x2 = arg1.reg_or_const.Camlburg.action ()
                    and y2 = arg2.reg.Camlburg.action ()
                    in
                        (x2 ,y2))
            })
            inf
and conSparcltu =
    fun arg1 ->
        (update__Sparcltu78
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparclt =
    fun arg1 ->
        (update__Sparclt77
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcleu =
    fun arg1 ->
        (update__Sparcleu76
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcle =
    fun arg1 ->
        (update__Sparcle75
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcgtu =
    fun arg1 ->
        (update__Sparcgtu74
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcgt =
    fun arg1 ->
        (update__Sparcgt73
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcgeu =
    fun arg1 ->
        (update__Sparcgeu72
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcge =
    fun arg1 ->
        (update__Sparcge71
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcfne =
    fun arg1 ->
        (update__Sparcfne80
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcflt =
    fun arg1 ->
        (update__Sparcflt84
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcfle =
    fun arg1 ->
        (update__Sparcfle83
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcfgt =
    fun arg1 ->
        (update__Sparcfgt82
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcfge =
    fun arg1 ->
        (update__Sparcfge81
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcfeq =
    fun arg1 ->
        (update__Sparcfeq79
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparceq =
    fun arg1 ->
        (update__Sparceq69
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparccarrybit =
    fun arg1 ->
        (update__Sparccarrybit58
            {Camlburg.cost = (arg1.cc.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let cc = arg1.cc.Camlburg.action () in cc)
            })
            inf
and conSparcaddcc =
    fun arg1 arg2 ->
        (update__Sparcaddcc56
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            inf
and conSparcadcflags =
    fun arg1 arg2 arg3 ->
        (update__Sparcadcflags62
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost
                +
                arg3._Sparccarrybit58.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    and _v1 = arg3._Sparccarrybit58.Camlburg.action ()
                    in
                        let cc = _v1 in (x ,y ,cc))
            })
            inf
and conSlice =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost = (arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1
                    and lsb = arg2
                    and y = arg3.any.Camlburg.action ()
                    in
                        
# 626 "sparcrec.mlb"
                        ( sprintf "Slice(%d, %d, %s)" n lsb y )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conShrl =
    fun arg1 arg2 ->
        (update__Shrl38
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            inf
and conShra =
    fun arg1 arg2 ->
        (update__Shra39
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            inf
and conShl =
    fun arg1 arg2 ->
        (update__Shl37
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            inf
and conSave =
    fun arg1 arg2 ->
        (update_inst
            (Camlburg.choice
                [{Camlburg.cost =
                    (arg1.sp.Camlburg.cost + arg2.neg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.sp.Camlburg.action ()
                        and y = arg2.neg.Camlburg.action ()
                        in
                            
# 495 "sparcrec.mlb"
                            ( if not !in_proc then (in_proc := true; sprintf "save %%sp, %s, %%sp" y)
     else sprintf "! Evil recognizer deleted add %%sp, %s, %%sp" y )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.neg.Camlburg.cost + arg2.sp.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.neg.Camlburg.action ()
                        and y = arg2.sp.Camlburg.action ()
                        in
                            
# 499 "sparcrec.mlb"
                            ( if not !in_proc then (in_proc := true; sprintf "save %%sp, %s, %%sp" x)
     else sprintf "! Evil recognizer deleted add %%sp, %s, %%sp" x )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.sp.Camlburg.cost + arg2.pos.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.sp.Camlburg.action ()
                        and y = arg2.pos.Camlburg.action ()
                        in
                            
# 503 "sparcrec.mlb"
                            ( sprintf "! Evil recognizer deleted add %%sp, %s, %%sp" y )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.pos.Camlburg.cost + arg2.sp.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.pos.Camlburg.action ()
                        and y = arg2.sp.Camlburg.action ()
                        in
                            
# 506 "sparcrec.mlb"
                            ( sprintf "! Evil recognizer deleted add %%sp, %s, %%sp" x )
                            
# 000 "/dev/stdout"
)
                }]))
            inf
and conRegPair =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let char = arg1
                    and n = arg2
                    in
                        
# 636 "sparcrec.mlb"
                        ( sprintf "RegPair(%s, %d)" (Char.escaped char) n )
                        
# 000 "/dev/stdout"
)
            })
            ((update_dregl
                {Camlburg.cost = ((Camlburg.matches 'f') arg1)
                ;Camlburg.action =
                    (fun () ->
                        let n = arg2
                        in
                            
# 225 "sparcrec.mlb"
                            ( sprintf "%%f%i" n )
                            
# 000 "/dev/stdout"
)
                })
                ((update_dregnuml
                    {Camlburg.cost = ((Camlburg.matches 'f') arg1)
                    ;Camlburg.action =
                        (fun () ->
                            let n = arg2
                            in
                                
# 228 "sparcrec.mlb"
                                ( n )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conReg =
    fun arg1 arg2 ->
        (update__Reg1
            {Camlburg.cost = ((Camlburg.matches 'i') arg1)
            ;Camlburg.action = (fun () -> let n = arg2 in n)
            })
            ((update__Reg2
                {Camlburg.cost = ((Camlburg.matches 'o') arg1)
                ;Camlburg.action = (fun () -> let n = arg2 in n)
                })
                ((update_any
                    {Camlburg.cost = (0)
                    ;Camlburg.action =
                        (fun () ->
                            let char = arg1
                            and n = arg2
                            in
                                
# 635 "sparcrec.mlb"
                                ( sprintf "Reg(%s, %d)" (Char.escaped char) n )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_arg_regl
                        {Camlburg.cost = ((Camlburg.matches 'o') arg1)
                        ;Camlburg.action =
                            (fun () ->
                                let n = arg2
                                in
                                    
# 243 "sparcrec.mlb"
                                    ( sprintf "%%o%i" n )
                                    
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
                                    
# 263 "sparcrec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                            })
                            ((update_cwpl
                                {Camlburg.cost =
                                    ((Camlburg.matches 'k') arg1
                                    +
                                    (Camlburg.matches 0) arg2)
                                ;Camlburg.action =
                                    (fun () ->
                                        
# 267 "sparcrec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                                })
                                ((update_fpl
                                    {Camlburg.cost =
                                        ((Camlburg.matches 'r') arg1
                                        +
                                        (Camlburg.matches 30) arg2)
                                    ;Camlburg.action =
                                        (fun () ->
                                            
# 265 "sparcrec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                    })
                                    ((update_fregl
                                        {Camlburg.cost =
                                            ((Camlburg.matches 'f') arg1)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let n = arg2
                                                in
                                                    
# 222 "sparcrec.mlb"
                                                    ( sprintf "%%f%i" n )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        ((update_fsrl
                                            {Camlburg.cost =
                                                (let n = arg2
                                                in
                                                    
# 234 "sparcrec.mlb"
                                                    ( guard (n = 4) )
                                                    
# 000 "/dev/stdout"

                                                +
                                                (Camlburg.matches 'c') arg1)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let n = arg2
                                                    in
                                                        
# 234 "sparcrec.mlb"
                                                        ( "%fsr" )
                                                        
# 000 "/dev/stdout"
)
                                            })
                                            ((update_npcl
                                                {Camlburg.cost =
                                                    ((Camlburg.matches 'c')
                                                        arg1
                                                    +
                                                    (Camlburg.matches 1)
                                                        arg2)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        
# 262 "sparcrec.mlb"
                                                        ( () )
                                                        
# 000 "/dev/stdout"
)
                                                })
                                                ((update_pcl
                                                    {Camlburg.cost =
                                                        ((Camlburg.matches
                                                            'c')
                                                            arg1
                                                        +
                                                        (Camlburg.matches 0)
                                                            arg2)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            
# 261 "sparcrec.mlb"
                                                            ( () )
                                                            
# 000 "/dev/stdout"
)
                                                    })
                                                    ((update_ral
                                                        {Camlburg.cost =
                                                            ((Camlburg.matches
                                                                'r')
                                                                arg1
                                                            +
                                                            (Camlburg.matches
                                                                31)
                                                                arg2)
                                                        ;Camlburg.action =
                                                            (fun () ->
                                                                
# 266 "sparcrec.mlb"
                                                                ( () )
                                                                
# 000 "/dev/stdout"
)
                                                        })
                                                        ((update_result_regl
                                                            {Camlburg.cost =
                                                                ((Camlburg.matches
                                                                    'i')
                                                                    arg1)
                                                            ;Camlburg.action =
                                                                (fun () ->
                                                                    let
                                                                        n =
                                                                        arg2
                                                                    in
                                                                        
# 240 "sparcrec.mlb"
                                                                        ( sprintf "%%i%i" n )
                                                                        
# 000 "/dev/stdout"
)
                                                            })
                                                            ((update_rregl
                                                                {Camlburg.cost =
                                                                    ((Camlburg.matches
                                                                        'r')
                                                                        arg1)
                                                                ;Camlburg.action =
                                                                    (fun
                                                                    ()
                                                                    ->
                                                                        let
                                                                            n =
                                                                            arg2
                                                                        in
                                                                            
# 219 "sparcrec.mlb"
                                                                            ( idiomatic_reg_name n )
                                                                            
# 000 "/dev/stdout"
)
                                                                })
                                                                ((update_spl
                                                                    {Camlburg.cost =
                                                                        ((Camlburg.matches
                                                                            'r')
                                                                            arg1
                                                                        +
                                                                        (Camlburg.matches
                                                                            14)
                                                                            arg2)
                                                                    ;Camlburg.action =
                                                                        (fun
                                                                        ()
                                                                        ->
                                                                            
# 264 "sparcrec.mlb"
                                                                            ( () )
                                                                            
# 000 "/dev/stdout"
)
                                                                    })
                                                                    ((update_yregl
                                                                        {Camlburg.cost =
                                                                            ((Camlburg.matches
                                                                                'y')
                                                                                arg1)
                                                                        ;Camlburg.action =
                                                                            (fun
                                                                            ()
                                                                            ->
                                                                                let
                                                                                    n =
                                                                                    arg2
                                                                                in
                                                                                    
# 231 "sparcrec.mlb"
                                                                                    ( "%y" )
                                                                                    
# 000 "/dev/stdout"
)
                                                                        })
                                                                        inf)))))))))))))))
and conQuot =
    fun arg1 arg2 ->
        (update__Quot27
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            inf
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
                        
# 642 "sparcrec.mlb"
                        ( s "Par(%s,%s)" l r )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1._Store22.Camlburg.cost
                        +
                        arg2._Store23.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Store22.Camlburg.action ()
                            and _v2 = arg2._Store23.Camlburg.action ()
                            in
                                let (yregl, x2, y2, w) = _v2
                                in
                                    let (dst, x, y, w) = _v1
                                    in
                                        
# 400 "sparcrec.mlb"
                                        ( sprintf "smul %s, %s, %s" x y dst )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Store22.Camlburg.cost
                        +
                        arg2._Store25.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Store22.Camlburg.action ()
                            and _v2 = arg2._Store25.Camlburg.action ()
                            in
                                let (yregl, x2, y2, w) = _v2
                                in
                                    let (dst, x, y, w) = _v1
                                    in
                                        
# 407 "sparcrec.mlb"
                                        ( sprintf "umul %s, %s, %s" x y dst )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto51.Camlburg.cost
                        +
                        arg2.next.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto51.Camlburg.action ()
                            and next = arg2.next.Camlburg.action ()
                            in
                                let target = _v1
                                in
                                    
# 489 "sparcrec.mlb"
                                    ( sprintf "call %s, 0\n\tnop" target )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto52.Camlburg.cost
                        +
                        arg2.next.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto52.Camlburg.action ()
                            and next = arg2.next.Camlburg.action ()
                            in
                                let reg = _v1
                                in
                                    
# 491 "sparcrec.mlb"
                                    ( sprintf "call %s, 0\n\tnop" reg )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto54.Camlburg.cost
                        +
                        arg2.restore.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto54.Camlburg.action ()
                            and restore = arg2.restore.Camlburg.action ()
                            in
                                let ra = _v1
                                in
                                    
# 511 "sparcrec.mlb"
                                    ( (in_proc := false; "ret\n\trestore") )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto52.Camlburg.cost
                        +
                        arg2._Store55.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto52.Camlburg.action ()
                            and _v2 = arg2._Store55.Camlburg.action ()
                            in
                                let (spl, reg, w) = _v2
                                in
                                    let r = _v1
                                    in
                                        
# 518 "sparcrec.mlb"
                                        ( sprintf "jmp %s\n\tmov %s, %%sp" r reg )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Store60.Camlburg.cost
                        +
                        arg2._Store61.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Store60.Camlburg.action ()
                            and _v2 = arg2._Store61.Camlburg.action ()
                            in
                                let (ccl, x, y, cc, w) = _v2
                                in
                                    let (d, x, y, cc, w) = _v1
                                    in
                                        
# 539 "sparcrec.mlb"
                                        ( sprintf "addxcc %s, %s, %s" x y d )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Store63.Camlburg.cost
                        +
                        arg2._Store64.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Store63.Camlburg.action ()
                            and _v2 = arg2._Store64.Camlburg.action ()
                            in
                                let (ccl, x, y, cc, w) = _v2
                                in
                                    let (d, x, y, cc, w) = _v1
                                    in
                                        
# 543 "sparcrec.mlb"
                                        ( sprintf "subxcc %s, %s, %s" x y d )
                                        
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conOr =
    fun arg1 arg2 ->
        (update__Or32
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Or33
                {Camlburg.cost =
                    (arg1.reg_or_const.Camlburg.cost
                    +
                    arg2.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg_or_const.Camlburg.action ()
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
                                
# 607 "sparcrec.mlb"
                                ( s "Or(%s, %s)" x y )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conNop =
    fun () ->
        (update_inst
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 591 "sparcrec.mlb"
                    ( "! Why do you think there should be a nop? " )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conNeg =
    fun arg1 ->
        (update__Neg29
            {Camlburg.cost = (arg1.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let x = arg1.reg_or_const.Camlburg.action () in x)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update__Mul20
            {Camlburg.cost =
                (arg1.reg_or_const.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg_or_const.Camlburg.action ()
                    and y = arg2.reg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Mul21
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost
                    +
                    arg2.reg_or_const.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg.Camlburg.action ()
                        and y = arg2.reg_or_const.Camlburg.action ()
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
                                
# 613 "sparcrec.mlb"
                                ( s "Mul(%s, %s)" x y )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conMem =
    fun arg1 arg2 ->
        (update__Mem14
            {Camlburg.cost = (arg1._Add15.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Add15.Camlburg.action ()
                    and mw = arg2
                    in
                        let (x, y) = _v1 in (x ,y ,mw))
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        and w = arg2
                        in
                            
# 634 "sparcrec.mlb"
                            ( s "Mem(%s)" any )
                            
# 000 "/dev/stdout"
)
                })
                ((update_meml
                    {Camlburg.cost = (arg1.reg.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let reg = arg1.reg.Camlburg.action ()
                            and w = arg2
                            in
                                
# 254 "sparcrec.mlb"
                                ( s "[%s]" reg )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conLoword =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 622 "sparcrec.mlb"
                        ( sprintf "Loword(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conLohalf =
    fun arg1 ->
        (update__Lohalf13
            {Camlburg.cost = (arg1.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let src = arg1.reg.Camlburg.action () in src)
            })
            inf
and conLobyte =
    fun arg1 ->
        (update__Lobyte12
            {Camlburg.cost = (arg1.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let src = arg1.reg.Camlburg.action () in src)
            })
            inf
and conLobits =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    and w = arg2
                    in
                        
# 633 "sparcrec.mlb"
                        ( s "Lobits(%s, %d)" any w )
                        
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
                    let symbol = arg1
                    and w = arg2
                    in
                        
# 599 "sparcrec.mlb"
                        ( s "Link(%s,%d)" (symbol#mangled_text) w )
                        
# 000 "/dev/stdout"
)
            })
            ((update_lconst
                {Camlburg.cost = (0)
                ;Camlburg.action =
                    (fun () ->
                        let symbol = arg1
                        and w = arg2
                        in
                            
# 207 "sparcrec.mlb"
                            ( symbol#mangled_text )
                            
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
                        
# 600 "sparcrec.mlb"
                        ( s "Late(%s,%d)" string w )
                        
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
                        
# 639 "sparcrec.mlb"
                        ( s "Kill(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conItof =
    fun arg1 ->
        (update__Itof5
            {Camlburg.cost = (arg1.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let src = arg1.freg.Camlburg.action () in src)
            })
            inf
and conI2f =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost = (arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1
                    and w = arg2
                    and any = arg3.any.Camlburg.action ()
                    in
                        
# 632 "sparcrec.mlb"
                        ( sprintf "I2f(%d, %d, %s)" n w any )
                        
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
                        
# 641 "sparcrec.mlb"
                        ( s "Guarded(%s,%s)" guard any )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1._Sparceq69.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparceq69.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 554 "sparcrec.mlb"
                                        ( sprintf "be %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcne70.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcne70.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 556 "sparcrec.mlb"
                                        ( sprintf "bne %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcge71.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcge71.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 559 "sparcrec.mlb"
                                        ( sprintf "bge %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcgeu72.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcgeu72.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 561 "sparcrec.mlb"
                                        ( sprintf "bgeu %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcgt73.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcgt73.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 563 "sparcrec.mlb"
                                        ( sprintf "bg %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcgtu74.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcgtu74.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 565 "sparcrec.mlb"
                                        ( sprintf "bgu %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcle75.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcle75.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 568 "sparcrec.mlb"
                                        ( sprintf "ble %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcleu76.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcleu76.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 570 "sparcrec.mlb"
                                        ( sprintf "bleu %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparclt77.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparclt77.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 572 "sparcrec.mlb"
                                        ( sprintf "bl %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcltu78.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcltu78.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 574 "sparcrec.mlb"
                                        ( sprintf "blu %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcfeq79.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcfeq79.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 577 "sparcrec.mlb"
                                        ( sprintf "fbe %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcfne80.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcfne80.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 579 "sparcrec.mlb"
                                        ( sprintf "fbne %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcfge81.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcfge81.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 582 "sparcrec.mlb"
                                        ( sprintf "fbge %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcfgt82.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcfgt82.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 584 "sparcrec.mlb"
                                        ( sprintf "fbg %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcfle83.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcfle83.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 587 "sparcrec.mlb"
                                        ( sprintf "fble %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Sparcflt84.Camlburg.cost
                        +
                        arg2._Goto51.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sparcflt84.Camlburg.action ()
                            and _v2 = arg2._Goto51.Camlburg.action ()
                            in
                                let target = _v2
                                in
                                    let cc = _v1
                                    in
                                        
# 589 "sparcrec.mlb"
                                        ( sprintf "fbl %s\n\tnop" target )
                                        
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conGoto =
    fun arg1 ->
        (update__Goto51
            {Camlburg.cost = (arg1.target.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let target = arg1.target.Camlburg.action () in target)
            })
            ((update__Goto52
                {Camlburg.cost = (arg1.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () -> let reg = arg1.reg.Camlburg.action () in reg)
                })
                ((update__Goto54
                    {Camlburg.cost = (arg1.ra.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () -> let ra = arg1.ra.Camlburg.action () in ra)
                    })
                    ((update_any
                        {Camlburg.cost = (arg1.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let any = arg1.any.Camlburg.action ()
                                in
                                    
# 643 "sparcrec.mlb"
                                    ( s "Goto(%s)" any )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_inst
                            (Camlburg.choice
                                [{Camlburg.cost = (arg1.target.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            target =
                                            arg1.target.Camlburg.action ()
                                        in
                                            
# 521 "sparcrec.mlb"
                                            ( sprintf "ba %s\n\tnop" target )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost = (arg1.reg.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let reg = arg1.reg.Camlburg.action ()
                                        in
                                            
# 524 "sparcrec.mlb"
                                            ( sprintf "jmp %s\n\tnop" reg )
                                            
# 000 "/dev/stdout"
)
                                }]))
                            inf))))
and conFtoi =
    fun arg1 ->
        (update__Ftoi3
            {Camlburg.cost = (arg1.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let src = arg1.freg.Camlburg.action () in src)
            })
            ((update__Ftoi4
                {Camlburg.cost = (arg1.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () -> let src = arg1.dreg.Camlburg.action () in src)
                })
                inf)
and conFtof =
    fun arg1 ->
        (update__Ftof6
            {Camlburg.cost = (arg1.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let src = arg1.freg.Camlburg.action () in src)
            })
            ((update__Ftof7
                {Camlburg.cost = (arg1.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () -> let src = arg1.dreg.Camlburg.action () in src)
                })
                inf)
and conFsub =
    fun arg1 arg2 ->
        (update__Fsub46
            {Camlburg.cost =
                (arg1.freg.Camlburg.cost + arg2.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.freg.Camlburg.action ()
                    and y = arg2.freg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Fsub47
                {Camlburg.cost =
                    (arg1.dreg.Camlburg.cost + arg2.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.dreg.Camlburg.action ()
                        and y = arg2.dreg.Camlburg.action ()
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
                                
# 615 "sparcrec.mlb"
                                ( sprintf "Fsub(%s, %s)" x y )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conFneg =
    fun arg1 ->
        (update__Fneg48
            {Camlburg.cost = (arg1.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let x = arg1.freg.Camlburg.action () in x)
            })
            ((update__Fneg49
                {Camlburg.cost = (arg1.dregnum.Camlburg.cost)
                ;Camlburg.action =
                    (fun () -> let x = arg1.dregnum.Camlburg.action () in x)
                })
                ((update_any
                    {Camlburg.cost = (arg1.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let any = arg1.any.Camlburg.action ()
                            in
                                
# 619 "sparcrec.mlb"
                                ( sprintf "Fneg(%s)" any )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conFmul =
    fun arg1 arg2 ->
        (update__Fmul42
            {Camlburg.cost =
                (arg1.freg.Camlburg.cost + arg2.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.freg.Camlburg.action ()
                    and y = arg2.freg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Fmul43
                {Camlburg.cost =
                    (arg1.dreg.Camlburg.cost + arg2.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.dreg.Camlburg.action ()
                        and y = arg2.dreg.Camlburg.action ()
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
                                
# 616 "sparcrec.mlb"
                                ( sprintf "Fmul(%s, %s)" x y )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conFetch =
    fun arg1 arg2 ->
        (update__Fetch16
            {Camlburg.cost = (arg1._Mem14.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Mem14.Camlburg.action ()
                    and w = arg2
                    in
                        let (x, y, mw) = _v1 in (x ,y ,mw ,w))
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        and w = arg2
                        in
                            
# 604 "sparcrec.mlb"
                            ( s "Fetch(%s,%d)" any w )
                            
# 000 "/dev/stdout"
)
                })
                ((update_arg_reg
                    {Camlburg.cost = (arg1._Reg2.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Reg2.Camlburg.action ()
                            and w = arg2
                            in
                                let n = _v1
                                in
                                    
# 242 "sparcrec.mlb"
                                    ( sprintf "%%i%i" n )
                                    
# 000 "/dev/stdout"
)
                    })
                    ((update_cc
                        {Camlburg.cost = (arg1.ccl.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let ccl = arg1.ccl.Camlburg.action ()
                                and w = arg2
                                in
                                    
# 270 "sparcrec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_cwp
                            {Camlburg.cost = (arg1.cwpl.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let cwpl = arg1.cwpl.Camlburg.action ()
                                    and w = arg2
                                    in
                                        
# 273 "sparcrec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_dreg
                                {Camlburg.cost = (arg1.dregl.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            dregl =
                                            arg1.dregl.Camlburg.action ()
                                        and w = arg2
                                        in
                                            
# 224 "sparcrec.mlb"
                                            ( dregl )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_dregnum
                                    {Camlburg.cost =
                                        (arg1.dregnuml.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                dregnuml =
                                                arg1.dregnuml.Camlburg.action
                                                    ()
                                            and w = arg2
                                            in
                                                
# 227 "sparcrec.mlb"
                                                ( dregnuml )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_freg
                                        {Camlburg.cost =
                                            (arg1.fregl.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    fregl =
                                                    arg1.fregl.Camlburg.action
                                                        ()
                                                and w = arg2
                                                in
                                                    
# 221 "sparcrec.mlb"
                                                    ( fregl )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        ((update_fsr
                                            {Camlburg.cost =
                                                (arg1.fsrl.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        fsrl =
                                                        arg1.fsrl.Camlburg.action
                                                            ()
                                                    and w = arg2
                                                    in
                                                        
# 233 "sparcrec.mlb"
                                                        ( fsrl )
                                                        
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
                                                            
# 253 "sparcrec.mlb"
                                                            ( meml )
                                                            
# 000 "/dev/stdout"
)
                                                })
                                                ((update_pc
                                                    {Camlburg.cost =
                                                        (arg1.pcl.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                pcl =
                                                                arg1.pcl.Camlburg.action
                                                                    ()
                                                            and w = arg2
                                                            in
                                                                
# 269 "sparcrec.mlb"
                                                                ( () )
                                                                
# 000 "/dev/stdout"
)
                                                    })
                                                    ((update_ra
                                                        {Camlburg.cost =
                                                            (arg1.ral.Camlburg.cost)
                                                        ;Camlburg.action =
                                                            (fun () ->
                                                                let
                                                                    ral =
                                                                    arg1.ral.Camlburg.action
                                                                        ()
                                                                and w = arg2
                                                                in
                                                                    
# 272 "sparcrec.mlb"
                                                                    ( () )
                                                                    
# 000 "/dev/stdout"
)
                                                        })
                                                        ((update_result_reg
                                                            {Camlburg.cost =
                                                                (arg1._Reg1.Camlburg.cost)
                                                            ;Camlburg.action =
                                                                (fun () ->
                                                                    let
                                                                        _v1 =
                                                                        arg1._Reg1.Camlburg.action
                                                                            ()
                                                                    and
                                                                        w =
                                                                        arg2
                                                                    in
                                                                        let
                                                                            n =
                                                                            _v1
                                                                        in
                                                                            
# 239 "sparcrec.mlb"
                                                                            ( sprintf "%%o%i" n )
                                                                            
# 000 "/dev/stdout"
)
                                                            })
                                                            ((update_rreg
                                                                {Camlburg.cost =
                                                                    (arg1.rregl.Camlburg.cost)
                                                                ;Camlburg.action =
                                                                    (fun
                                                                    ()
                                                                    ->
                                                                        let
                                                                            rregl =
                                                                            arg1.rregl.Camlburg.action
                                                                                ()
                                                                        and
                                                                            w =
                                                                            arg2
                                                                        in
                                                                            
# 218 "sparcrec.mlb"
                                                                            ( rregl )
                                                                            
# 000 "/dev/stdout"
)
                                                                })
                                                                ((update_sp
                                                                    {Camlburg.cost =
                                                                        (arg1.spl.Camlburg.cost)
                                                                    ;Camlburg.action =
                                                                        (fun
                                                                        ()
                                                                        ->
                                                                            let
                                                                                spl =
                                                                                arg1.spl.Camlburg.action
                                                                                    ()
                                                                            and
                                                                                w =
                                                                                arg2
                                                                            in
                                                                                
# 271 "sparcrec.mlb"
                                                                                ( () )
                                                                                
# 000 "/dev/stdout"
)
                                                                    })
                                                                    ((update_yreg
                                                                        {Camlburg.cost =
                                                                            (arg1.yregl.Camlburg.cost)
                                                                        ;Camlburg.action =
                                                                            (fun
                                                                            ()
                                                                            ->
                                                                                let
                                                                                    yregl =
                                                                                    arg1.yregl.Camlburg.action
                                                                                        ()
                                                                                and
                                                                                    w =
                                                                                    arg2
                                                                                in
                                                                                    
# 230 "sparcrec.mlb"
                                                                                    ( yregl )
                                                                                    
# 000 "/dev/stdout"
)
                                                                        })
                                                                        inf)))))))))))))))
and conFdiv =
    fun arg1 arg2 ->
        (update__Fdiv40
            {Camlburg.cost =
                (arg1.freg.Camlburg.cost + arg2.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.freg.Camlburg.action ()
                    and y = arg2.freg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Fdiv41
                {Camlburg.cost =
                    (arg1.dreg.Camlburg.cost + arg2.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.dreg.Camlburg.action ()
                        and y = arg2.dreg.Camlburg.action ()
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
                                
# 617 "sparcrec.mlb"
                                ( sprintf "Fdiv(%s, %s)" x y )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conFalse =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 598 "sparcrec.mlb"
                    ( "False" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conFadd =
    fun arg1 arg2 ->
        (update__Fadd44
            {Camlburg.cost =
                (arg1.freg.Camlburg.cost + arg2.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.freg.Camlburg.action ()
                    and y = arg2.freg.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Fadd45
                {Camlburg.cost =
                    (arg1.dreg.Camlburg.cost + arg2.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.dreg.Camlburg.action ()
                        and y = arg2.dreg.Camlburg.action ()
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
                                
# 614 "sparcrec.mlb"
                                ( sprintf "Fadd(%s, %s)" x y )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conFabs =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 620 "sparcrec.mlb"
                        ( sprintf "Fabs(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conF2i =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost = (arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1
                    and w = arg2
                    and any = arg3.any.Camlburg.action ()
                    in
                        
# 631 "sparcrec.mlb"
                        ( sprintf "F2i(%d, %d, %s)" n w any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conF2f =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost = (arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1
                    and w = arg2
                    and any = arg3.any.Camlburg.action ()
                    in
                        
# 630 "sparcrec.mlb"
                        ( sprintf "F2f(%d, %d, %s)" n w any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conDivu =
    fun arg1 arg2 ->
        (update__Divu28
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            inf
and conDiff =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let c1 = arg1.any.Camlburg.action ()
                    and c2 = arg2.any.Camlburg.action ()
                    in
                        
# 601 "sparcrec.mlb"
                        ( s "Diff(%s,%s)" c1 c2 )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conCom =
    fun arg1 ->
        (update__Com36
            {Camlburg.cost = (arg1.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let x = arg1.reg_or_const.Camlburg.action () in x)
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.any.Camlburg.action ()
                        in
                            
# 609 "sparcrec.mlb"
                            ( s "Com(%s)" x )
                            
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
                        
# 602 "sparcrec.mlb"
                        ( sprintf "Bits(%s)" (Bits.to_string bits) )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost =
                    (let b = arg1
                    in
                        
# 208 "sparcrec.mlb"
                        ( guard (Bits.width b = 32) )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let b = arg1
                        in
                            
# 208 "sparcrec.mlb"
                            ( const32 b )
                            
# 000 "/dev/stdout"
)
                })
                ((update_constx
                    {Camlburg.cost =
                        (let b = arg1
                        in
                            
# 212 "sparcrec.mlb"
                            ( guard (Bits.width b = 64) )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let b = arg1
                            in
                                
# 212 "sparcrec.mlb"
                                ( const64 b )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_four
                        {Camlburg.cost =
                            (let bits = arg1
                            in
                                
# 214 "sparcrec.mlb"
                                ( guard (Bits.eq bits (Bits.S.of_int 4 32)) )
                                
# 000 "/dev/stdout"
)
                        ;Camlburg.action =
                            (fun () ->
                                let bits = arg1
                                in
                                    
# 214 "sparcrec.mlb"
                                    (())
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_neg
                            {Camlburg.cost =
                                (let b = arg1
                                in
                                    
# 210 "sparcrec.mlb"
                                    ( guard (Bits.width b = 32 && negative b) )
                                    
# 000 "/dev/stdout"
)
                            ;Camlburg.action =
                                (fun () ->
                                    let b = arg1
                                    in
                                        
# 210 "sparcrec.mlb"
                                        ( const32 b )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_one
                                {Camlburg.cost =
                                    (let bits = arg1
                                    in
                                        
# 215 "sparcrec.mlb"
                                        ( guard (Bits.eq bits (Bits.S.of_int 1 32)) )
                                        
# 000 "/dev/stdout"
)
                                ;Camlburg.action =
                                    (fun () ->
                                        let bits = arg1
                                        in
                                            
# 215 "sparcrec.mlb"
                                            (())
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_pos
                                    {Camlburg.cost =
                                        (let b = arg1
                                        in
                                            
# 209 "sparcrec.mlb"
                                            ( guard (Bits.width b = 32 && positive b) )
                                            
# 000 "/dev/stdout"
)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let b = arg1
                                            in
                                                
# 209 "sparcrec.mlb"
                                                ( const32 b )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_zero
                                        {Camlburg.cost =
                                            (let bits = arg1
                                            in
                                                
# 216 "sparcrec.mlb"
                                                ( guard (Bits.eq bits (Bits.S.of_int 0 32)) )
                                                
# 000 "/dev/stdout"
)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let bits = arg1
                                                in
                                                    
# 216 "sparcrec.mlb"
                                                    (())
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        inf)))))))
and conBitInsert =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost
                +
                arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.any.Camlburg.action ()
                    and y = arg2.any.Camlburg.action ()
                    and z = arg3.any.Camlburg.action ()
                    in
                        
# 624 "sparcrec.mlb"
                        ( sprintf "BitInsert(%s, %s, %s)" x y z )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conBitExtract =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let lsb = arg1.any.Camlburg.action ()
                    and y = arg2.any.Camlburg.action ()
                    and n = arg3
                    in
                        
# 625 "sparcrec.mlb"
                        ( sprintf "BitExtract(%s, %s, %d)" lsb y n )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conAnd =
    fun arg1 arg2 ->
        (update__And30
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__And31
                {Camlburg.cost =
                    (arg1.reg_or_const.Camlburg.cost
                    +
                    arg2.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg_or_const.Camlburg.action ()
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
                                
# 606 "sparcrec.mlb"
                                ( s "And(%s, %s)" x y )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAddc =
    fun arg1 arg2 arg3 ->
        (update__Addc57
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.reg_or_const.Camlburg.cost
                +
                arg3._Sparccarrybit58.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.reg_or_const.Camlburg.action ()
                    and _v1 = arg3._Sparccarrybit58.Camlburg.action ()
                    in
                        let cc = _v1 in (x ,y ,cc))
            })
            inf
and conAdd =
    fun arg1 arg2 ->
        (update__Add15
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.reg.Camlburg.action ()
                    and y = arg2.const.Camlburg.action ()
                    in
                        (x ,y))
            })
            ((update__Add17
                {Camlburg.cost =
                    (arg1.reg_or_const.Camlburg.cost
                    +
                    arg2.reg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.reg_or_const.Camlburg.action ()
                        and y = arg2.reg.Camlburg.action ()
                        in
                            (x ,y))
                })
                ((update__Add18
                    {Camlburg.cost =
                        (arg1.reg.Camlburg.cost
                        +
                        arg2.reg_or_const.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.reg.Camlburg.action ()
                            and y = arg2.reg_or_const.Camlburg.action ()
                            in
                                (x ,y))
                    })
                    ((update__Add50
                        {Camlburg.cost =
                            (arg1.pc.Camlburg.cost + arg2.four.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let pc = arg1.pc.Camlburg.action ()
                                and four = arg2.four.Camlburg.action ()
                                in
                                    (pc ,four))
                        })
                        ((update__Add53
                            {Camlburg.cost =
                                (arg1.cwp.Camlburg.cost
                                +
                                arg2.one.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let cwp = arg1.cwp.Camlburg.action ()
                                    and one = arg2.one.Camlburg.action ()
                                    in
                                        (cwp ,one))
                            })
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
                                            
# 612 "sparcrec.mlb"
                                            ( s "Add(%s, %s)" x y )
                                            
# 000 "/dev/stdout"
)
                                })
                                inf)))))



# 55 "sparcrec.mlb"
 
   (*s: [[Sparcrec]] code to follow the labeler *)
   let unimp = Impossible.unimp
   let const = function
     | RP.Late(s,w)              -> unimp "sparc: late constants"
     | RP.Bool(b)                -> unimp "sparc: bool"
     | RP.Link(s,_,w)            -> conLink s w
     | RP.Diff _                 -> error "PIC not supported"
     | RP.Bits(b)                -> conBits(b)
   (*x: [[Sparcrec]] code to follow the labeler *)
   let rec exp = function
     | RP.Const(k)               -> const (k)
     | RP.Fetch(l,w)             -> conFetch (loc l) w
     (*s: [[Sparcrec]] special cases for particular operators *)
     | RP.App(("sub",   [w]), [x; y])  -> conSub    (exp x) (exp y)
     | RP.App(("subb",  [w]), [x; y; z]) -> conSubb (exp x) (exp y) (exp z)
     | RP.App(("add",   [w]), [x; y])  -> conAdd    (exp x) (exp y)
     | RP.App(("addc",  [w]), [x; y; z]) -> conAddc (exp x) (exp y) (exp z)
     | RP.App(("mul",   [32]), [x; y]) -> conMul    (exp x) (exp y)
     | RP.App(("neg",   [w]), [x])     -> conNeg    (exp x)
     | RP.App(("quot",  [w]), [x; y])  -> conQuot   (exp x) (exp y)
     | RP.App(("divu",  [w]), [x; y])  -> conDivu   (exp x) (exp y)
     (* maybe can be implemented with mulx instruction for SPARC 9
     | RP.App(("mulx",  [32;64]), [x; y]) -> conMulx (exp x) (exp y)
     *)

     | RP.App(("and",   [w]), [x; y]) -> conAnd    (exp x) (exp y)
     | RP.App(("or",    [w]), [x; y]) -> conOr     (exp x) (exp y)
     | RP.App(("xor",   [w]), [x; y]) -> conXor    (exp x) (exp y)
     | RP.App(("com",   [w]), [x])    -> conCom    (exp x)

     | RP.App(("shl",   [w]), [x; y]) -> conShl    (exp x) (exp y)
     | RP.App(("shrl",  [w]), [x; y]) -> conShrl   (exp x) (exp y)
     | RP.App(("shra",  [w]), [x; y]) -> conShra   (exp x) (exp y)

     | RP.App(("lobits",[32;8]),  [x]) -> conLobyte (exp x)
     | RP.App(("lobits",[32;16]), [x]) -> conLohalf (exp x)
     | RP.App(("lobits",[64;32]), [x]) -> conLoword (exp x)
     | RP.App(("zx", [8; 32]),    [x]) -> conZxbyte (exp x)
     | RP.App(("zx", [16;32]),    [x]) -> conZxhalf (exp x)
     | RP.App(("sx", [8; 32]),    [x]) -> conSxbyte (exp x)
     | RP.App(("sx", [16;32]),    [x]) -> conSxhalf (exp x)

     | RP.App(("sparc_subcc", [w]), [x; y]) -> conSparcsubcc (exp x) (exp y)
     | RP.App(("sparc_addcc", [w]), [x; y]) -> conSparcaddcc (exp x) (exp y)
     | RP.App(("sparc_mulx_hi", [w]), [x; y]) -> conSparcmulxhi (exp x) (exp y)
     | RP.App(("sparc_mulux_hi", [w]), [x; y]) -> conSparcmuluxhi (exp x) (exp y)
     | RP.App(("sparc_adcflags", [w]), [x; y; z]) -> conSparcadcflags (exp x) (exp y) (exp z)
     | RP.App(("sparc_sbbflags", [w]), [x; y; z]) -> conSparcsbbflags (exp x) (exp y) (exp z)
     | RP.App(("sparc_carrybit", _), [x]) -> conSparccarrybit (exp x)

     | RP.App(("sparc_eq",    [w]), [x]) -> conSparceq  (exp x)
     | RP.App(("sparc_ne",    [w]), [x]) -> conSparcne  (exp x)
     | RP.App(("sparc_ge",    [w]), [x]) -> conSparcge  (exp x)
     | RP.App(("sparc_geu",   [w]), [x]) -> conSparcgeu (exp x)
     | RP.App(("sparc_gt",    [w]), [x]) -> conSparcgt  (exp x)
     | RP.App(("sparc_gtu",   [w]), [x]) -> conSparcgtu (exp x)
     | RP.App(("sparc_le",    [w]), [x]) -> conSparcle  (exp x)
     | RP.App(("sparc_leu",   [w]), [x]) -> conSparcleu (exp x)
     | RP.App(("sparc_lt",    [w]), [x]) -> conSparclt  (exp x)
     | RP.App(("sparc_ltu",   [w]), [x]) -> conSparcltu (exp x)

     | RP.App(("sparc_feq",    [w]), [x]) -> conSparcfeq  (exp x)
     | RP.App(("sparc_fne",    [w]), [x]) -> conSparcfne  (exp x)
     | RP.App(("sparc_fge",    [w]), [x]) -> conSparcfge  (exp x)
     | RP.App(("sparc_fgt",    [w]), [x]) -> conSparcfgt  (exp x)
     | RP.App(("sparc_fle",    [w]), [x]) -> conSparcfle  (exp x)
     | RP.App(("sparc_flt",    [w]), [x]) -> conSparcflt  (exp x)

     | RP.App(("fdiv", [w]), [x; y; rm]) -> conFdiv (exp x) (exp y)
     | RP.App(("fmul", [w]), [x; y; rm]) -> conFmul (exp x) (exp y)
     | RP.App(("fadd", [w]), [x; y; rm]) -> conFadd (exp x) (exp y)
     | RP.App(("fsub", [w]), [x; y; rm]) -> conFsub (exp x) (exp y)
     | RP.App(("fneg", [w]), [x])        -> conFneg (exp x)

     | RP.App(("i2f", [w;w']), [x; rm]) -> conItof (exp x)
     | RP.App(("f2i", [w;w']), [x; rm]) -> conFtoi (exp x)
          (* MISSING ASSERTION: %f2i ALWAYS ROUNDS TO ZERO *)
     | RP.App(("f2f", [w;w']), [x; rm]) -> conFtof (exp x)

     | RP.App((("add"|"sub"|"mul"|"sx"|"zx"|"lobits"|"bitInsert"|
                "bitExtract"|"fabs"|"fneg"|"fdiv"|"fmul"|"fsub"|"fadd"|"f2f"|"f2i"|
                "i2f"|"and"|"or"|"xor"|"com") as op, ws), xs)->
         Impossible.impossible
           (Printf.sprintf
              "operator %%%s specialized to [%s] & applied to %d arguments"
              op (String.concat "; " (List.map string_of_int ws)) (List.length xs))
     (*e: [[Sparcrec]] special cases for particular operators *)
     | RP.App((o,_),_)           -> error ("unknown operator " ^ o)
   (*x: [[Sparcrec]] code to follow the labeler *)
   and loc l = match l with
     | RP.Mem(('m', aff, _), w, e, ass) -> conMem (exp e) w
     | RP.Reg((sp, _, _), i, R.C 1)     -> conReg     sp i
     | RP.Reg((sp, _, _), i, R.C 2)     -> conRegPair sp i
     | RP.Reg _                  -> unimp "quad registers and other large beasts"
     | RP.Mem(_, _, _, _)        -> error "non-mem, non-reg cell"
     | RP.Var _ | RP.Global _    -> error "var found"
     | RP.Slice(w,i,l)           -> unimp "sparc: slice locations"
   (*x: [[Sparcrec]] code to follow the labeler *)
   and effect = function
     | RP.Store(RP.Reg(('c',_,_), i, c), r, _)
       when (i = 1 (* i = npc *)) -> conGoto (exp r)
   (*
     | RP.Store(RP.Reg('c',i, _), r, w)  -> error (sprintf "set $c[%d]" i)
   *)
     | RP.Store(maybe_spl, RP.App(("add",_), [x;y]), _)
       when (RU.Eq.loc maybe_spl spl && (RU.Eq.exp x sp || RU.Eq.exp y sp)) ->
         conSave (exp x) (exp y)
     | RP.Store(l,e,w)                   -> conStore (loc l) (exp e) w
     | RP.Kill(l)                        -> unimp "sparc: kill effect"
   (*x: [[Sparcrec]] code to follow the labeler *)
   and regpair = function
     | _ -> Impossible.impossible "Argument is not a register pair"
   (*x: [[Sparcrec]] code to follow the labeler *)
   and rtl (RP.Rtl es) = geffects es
   and geffects = function
       | []          -> conNop ()
       | [g, s]      -> guarded g s
       | (g, s) :: t -> conPar (guarded g s) (geffects t)
   and guarded g eff = match g with
     | RP.Const(RP.Bool b) -> if b then effect eff else conNop()
     | _ -> conGuarded (exp g) (effect eff)
   (*x: [[Sparcrec]] code to follow the labeler *)
   let errmsg r msg =
     List.iter prerr_string
       [ "recognizer error: "; msg; " on "; RU.ToString.rtl r; "\n" ]

   let to_asm r =
     try
       let plan = rtl (Down.rtl (Simplify.rtl r)) in
       plan.inst.Camlburg.action ()
     with 
     | Camlburg.Uncovered -> " not an instruction: " ^ RU.ToString.rtl r
     | Error msg -> (errmsg r msg; " error in recognizer: " ^ msg)

   let is_instruction r =
     try
       let plan = rtl (Down.rtl (Simplify.rtl r)) in
       plan.inst.Camlburg.cost < 100  (* should be true, but shade this... *)
     with 
     | Camlburg.Uncovered -> false
     | Error msg -> (errmsg r msg; false)
   (*e: [[Sparcrec]] code to follow the labeler *)
   

# 000 "/dev/stdout"
