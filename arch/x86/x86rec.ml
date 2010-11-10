
# 2 "x86rec.mlb"
 
  (*s: modules *)
  open Nopoly

  module Rg = X86regs
  module RP = Rtl.Private
  module RU = Rtlutil
  module SS = Space.Standard32
  module Dn = Rtl.Dn      (* Convert Dn  to private repr. *)
  module Up = Rtl.Up      (* Convert Up    to abstract repr. *)

  let ( =/  ) = RU.Eq.loc
  let ( =// ) = RU.Eq.exp
  (*e: modules *)
  module M = struct
    (*s: code to precede the labeler *)
    exception Error of string
    let error msg = raise (Error msg)
    (*x: code to precede the labeler *)
    let sprintf = Printf.sprintf
    let s       = Printf.sprintf
    (*x: code to precede the labeler *)
    let weirdb = Bits.S.of_int (-4) 32
    let weird = Nativeint.to_string (Bits.S.to_native weirdb)
     (* let _ = List.iter prerr_string ["Weird integer is "; weird; "\n"] *)
    let native' w b = 
      assert (Bits.width b = w);
      Nativeint.to_string (Bits.U.to_native b)
    let native = native' 32
    let cat = String.concat ""
    let is_shamt b =
      let w = Bits.width b in
      w >= 5 && Bits.Ops.lt (Bits.zero w) b && Bits.Ops.lt b (Bits.U.of_int 32 w)
    (*x: code to precede the labeler *)
    let infinity = Camlburg.inf_cost
    let guard b = if b then 0 else infinity
    (*x: code to precede the labeler *)
    let suffix w = match w with
    | 8  -> "b"
    | 16 -> "w"
    | 32 -> "l"
    | _ -> Impossible.impossible "width in x86 not 8/16/32"
    let fsuffix w = match w with
    | 32 -> "s"
    | 64 -> "l"
    | 80 | 96 -> "t"
    | _ -> Impossible.impossible "floating-point width in x86 not 32/64/80"
    let fisuffix w = match w with
    | 16 -> "w"
    | 32 -> "l"
    | 64 -> "q"
    | _ -> Impossible.impossible "int to/from fpregs width in x86 not 16/32/64"
    (*x: code to precede the labeler *)
    let r8_names  = [| "%al"; "%cl"; "%dl"; "%bl"; "%ah"; "%ch"; "%dh"; "%bh" |]
    let r16_names = [| "%ax"; "%cx"; "%dx"; "%bx"; "%sp"; "%bp"; "%si"; "%di" |]
    let r32_names = [| "%eax"; "%ecx"; "%edx"; "%ebx"; "%esp"; "%ebp"; "%esi"; "%edi" |]
    let reg_names w = match w with
    | 8  -> r8_names
    | 16 -> r16_names
    | 32 -> r32_names
    | _  -> Impossible.impossible "x86 register width not 8/16/32"
    let regname w = Array.get (reg_names w)
    let hregname r = sprintf "%%%ch" r.[2]  (* pass %eax, get back %ah *)
    (*x: code to precede the labeler *)
    let fpcc      = Dn.loc Rg.fpcc
    let fpustatus = Dn.loc Rg.fpustatus
    let fpuctl    = RP.Reg Rg.fpuctl
    (*e: code to precede the labeler *)


# 000 "/dev/stdout"


type
    (
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
        _Zx33: ( 't84 ) Camlburg.nt;
        _Zx11: ( 't83 ) Camlburg.nt;
        _Zx10: ( 't82 ) Camlburg.nt;
        _X86_subflags35: ( 't81 ) Camlburg.nt;
        _X86_subflags34: ( 't80 ) Camlburg.nt;
        _Sx8: ( 't79 ) Camlburg.nt;
        _Sx6: ( 't78 ) Camlburg.nt;
        _Sub50: ( 't77 ) Camlburg.nt;
        _Sub39: ( 't76 ) Camlburg.nt;
        _Store71: ( 't75 ) Camlburg.nt;
        _Store67: ( 't74 ) Camlburg.nt;
        _Store65: ( 't73 ) Camlburg.nt;
        _Store63: ( 't72 ) Camlburg.nt;
        _Store61: ( 't71 ) Camlburg.nt;
        _Store59: ( 't70 ) Camlburg.nt;
        _Store57: ( 't69 ) Camlburg.nt;
        _Store55: ( 't68 ) Camlburg.nt;
        _Store53: ( 't67 ) Camlburg.nt;
        _Store51: ( 't66 ) Camlburg.nt;
        _Store44: ( 't65 ) Camlburg.nt;
        _Store4: ( 't64 ) Camlburg.nt;
        _Store37: ( 't63 ) Camlburg.nt;
        _Store22: ( 't62 ) Camlburg.nt;
        _Reg14: ( 't61 ) Camlburg.nt;
        _Par43: ( 't60 ) Camlburg.nt;
        _Par21: ( 't59 ) Camlburg.nt;
        _Par19: ( 't58 ) Camlburg.nt;
        _Par17: ( 't57 ) Camlburg.nt;
        _Mem29: ( 't56 ) Camlburg.nt;
        _Mem27: ( 't55 ) Camlburg.nt;
        _Lobits5: ( 't54 ) Camlburg.nt;
        _Lobits15: ( 't53 ) Camlburg.nt;
        _Lobits12: ( 't52 ) Camlburg.nt;
        _Llr20: ( 't51 ) Camlburg.nt;
        _Llr18: ( 't50 ) Camlburg.nt;
        _Llr16: ( 't49 ) Camlburg.nt;
        _I2f56: ( 't48 ) Camlburg.nt;
        _Guarded23: ( 't47 ) Camlburg.nt;
        _Goto46: ( 't46 ) Camlburg.nt;
        _Goto45: ( 't45 ) Camlburg.nt;
        _Goto42: ( 't44 ) Camlburg.nt;
        _Goto40: ( 't43 ) Camlburg.nt;
        _Goto36: ( 't42 ) Camlburg.nt;
        _Goto25: ( 't41 ) Camlburg.nt;
        _Fsub62: ( 't40 ) Camlburg.nt;
        _Frnd69: ( 't39 ) Camlburg.nt;
        _Fmul68: ( 't38 ) Camlburg.nt;
        _Flags2ah74: ( 't37 ) Camlburg.nt;
        _Fetch9: ( 't36 ) Camlburg.nt;
        _Fetch75: ( 't35 ) Camlburg.nt;
        _Fetch7: ( 't34 ) Camlburg.nt;
        _Fetch47: ( 't33 ) Camlburg.nt;
        _Fetch41: ( 't32 ) Camlburg.nt;
        _Fetch32: ( 't31 ) Camlburg.nt;
        _Fetch31: ( 't30 ) Camlburg.nt;
        _Fetch30: ( 't29 ) Camlburg.nt;
        _Fetch28: ( 't28 ) Camlburg.nt;
        _Fetch26: ( 't27 ) Camlburg.nt;
        _Fetch24: ( 't26 ) Camlburg.nt;
        _Fetch2: ( 't25 ) Camlburg.nt;
        _Fetch13: ( 't24 ) Camlburg.nt;
        _Fetch1: ( 't23 ) Camlburg.nt;
        _Fdiv66: ( 't22 ) Camlburg.nt;
        _Fdiv64: ( 't21 ) Camlburg.nt;
        _Fcmp70: ( 't20 ) Camlburg.nt;
        _Fadd60: ( 't19 ) Camlburg.nt;
        _F2i58: ( 't18 ) Camlburg.nt;
        _F2f54: ( 't17 ) Camlburg.nt;
        _F2f52: ( 't16 ) Camlburg.nt;
        _Cmp3: ( 't15 ) Camlburg.nt;
        _BitInsert72: ( 't14 ) Camlburg.nt;
        _Ah2flags73: ( 't13 ) Camlburg.nt;
        _Add49: ( 't12 ) Camlburg.nt;
        _Add48: ( 't11 ) Camlburg.nt;
        _Add38: ( 't10 ) Camlburg.nt;
        zero: ( ( unit ) ) Camlburg.nt;
        vfpl: ( ( unit ) ) Camlburg.nt;
        vfp: ( ( unit ) ) Camlburg.nt;
        two3: ( ( unit ) ) Camlburg.nt;
        target: ( ( string ) ) Camlburg.nt;
        stacktop: ( ( unit ) ) Camlburg.nt;
        stacknext: ( ( unit ) ) Camlburg.nt;
        slotaddr: ( ( string ) ) Camlburg.nt;
        slot: ( ( string ) ) Camlburg.nt;
        shamt: ( ( int ) ) Camlburg.nt;
        regpairl: ( ( string ) ) Camlburg.nt;
        regpair: ( ( string ) ) Camlburg.nt;
        regl_ecx: ( 't9 ) Camlburg.nt;
        regl8H: ( 't8 ) Camlburg.nt;
        regl8: ( ( string ) ) Camlburg.nt;
        regl: ( ( string ) ) Camlburg.nt;
        regabcdl: ( ( string ) ) Camlburg.nt;
        regabcd: ( ( string ) ) Camlburg.nt;
        reg_cll: ( 't7 ) Camlburg.nt;
        reg_cl: ( 't6 ) Camlburg.nt;
        reg8H: ( 't5 ) Camlburg.nt;
        reg8: ( ( string ) ) Camlburg.nt;
        reg: ( ( string ) ) Camlburg.nt;
        push: ( ( unit ) ) Camlburg.nt;
        pop: ( ( unit ) ) Camlburg.nt;
        pic: ( 't4 ) Camlburg.nt;
        one3: ( ( unit ) ) Camlburg.nt;
        minusfour: ( ( unit ) ) Camlburg.nt;
        meml: ( ( string ) ) Camlburg.nt;
        mem: ( ( string ) ) Camlburg.nt;
        lconst: ( ( string ) ) Camlburg.nt;
        inthandler: ( ( string ) ) Camlburg.nt;
        inst: ( ( string ) ) Camlburg.nt;
        immed8: ( ( string ) ) Camlburg.nt;
        immed: ( ( string ) ) Camlburg.nt;
        i2f_mem: ( ( int * string ) ) Camlburg.nt;
        fpustatusl: ( ( unit ) ) Camlburg.nt;
        fpustatus: ( ( unit ) ) Camlburg.nt;
        fpuctll: ( ( unit ) ) Camlburg.nt;
        fpuctl: ( ( unit ) ) Camlburg.nt;
        fpstacktopl: ( ( unit ) ) Camlburg.nt;
        fpstacktop: ( ( unit ) ) Camlburg.nt;
        fpstacknextl: ( ( unit ) ) Camlburg.nt;
        fpstacknext: ( ( unit ) ) Camlburg.nt;
        fpstack1l: ( ( unit ) ) Camlburg.nt;
        fpstack1: ( ( unit ) ) Camlburg.nt;
        fpsp: ( ( unit ) ) Camlburg.nt;
        fppush: ( ( unit ) ) Camlburg.nt;
        fppop2: ( ( unit ) ) Camlburg.nt;
        fppop: ( ( unit ) ) Camlburg.nt;
        fpccl: ( ( unit ) ) Camlburg.nt;
        four: ( ( unit ) ) Camlburg.nt;
        fiadd: ( ( int * string ) ) Camlburg.nt;
        espl: ( ( unit ) ) Camlburg.nt;
        esp: ( ( unit ) ) Camlburg.nt;
        esi: ( ( unit ) ) Camlburg.nt;
        eip: ( ( unit ) ) Camlburg.nt;
        eight: ( ( unit ) ) Camlburg.nt;
        eflags: ( ( unit ) ) Camlburg.nt;
        edx_eax: ( ( unit ) ) Camlburg.nt;
        edx: ( ( unit ) ) Camlburg.nt;
        edi: ( ( unit ) ) Camlburg.nt;
        ecx: ( ( unit ) ) Camlburg.nt;
        eax: ( ( unit ) ) Camlburg.nt;
        eaddrl: ( ( string ) ) Camlburg.nt;
        eaddri: ( ( string ) ) Camlburg.nt;
        eaddrbitk: ( 't3 ) Camlburg.nt;
        eaddrbit0: ( 't2 ) Camlburg.nt;
        eaddr_shr_k: ( ( string * int ) ) Camlburg.nt;
        eaddr: ( ( string ) ) Camlburg.nt;
        disp: ( ( string ) ) Camlburg.nt;
        const8: ( ( string ) ) Camlburg.nt;
        const: ( ( string ) ) Camlburg.nt;
        bare_regabcd: ( 't1 ) Camlburg.nt;
        bare_reg: ( 't0 ) Camlburg.nt;
        ax: ( ( string ) ) Camlburg.nt;
        any: ( ( string ) ) Camlburg.nt;
        ahval: ( ( string ) ) Camlburg.nt;
        ah: ( ( string ) ) Camlburg.nt
    }

let rec
inf =
    {ah = (Camlburg.infinity)
    ;ahval = (Camlburg.infinity)
    ;any = (Camlburg.infinity)
    ;ax = (Camlburg.infinity)
    ;bare_reg = (Camlburg.infinity)
    ;bare_regabcd = (Camlburg.infinity)
    ;const = (Camlburg.infinity)
    ;const8 = (Camlburg.infinity)
    ;disp = (Camlburg.infinity)
    ;eaddr = (Camlburg.infinity)
    ;eaddr_shr_k = (Camlburg.infinity)
    ;eaddrbit0 = (Camlburg.infinity)
    ;eaddrbitk = (Camlburg.infinity)
    ;eaddri = (Camlburg.infinity)
    ;eaddrl = (Camlburg.infinity)
    ;eax = (Camlburg.infinity)
    ;ecx = (Camlburg.infinity)
    ;edi = (Camlburg.infinity)
    ;edx = (Camlburg.infinity)
    ;edx_eax = (Camlburg.infinity)
    ;eflags = (Camlburg.infinity)
    ;eight = (Camlburg.infinity)
    ;eip = (Camlburg.infinity)
    ;esi = (Camlburg.infinity)
    ;esp = (Camlburg.infinity)
    ;espl = (Camlburg.infinity)
    ;fiadd = (Camlburg.infinity)
    ;four = (Camlburg.infinity)
    ;fpccl = (Camlburg.infinity)
    ;fppop = (Camlburg.infinity)
    ;fppop2 = (Camlburg.infinity)
    ;fppush = (Camlburg.infinity)
    ;fpsp = (Camlburg.infinity)
    ;fpstack1 = (Camlburg.infinity)
    ;fpstack1l = (Camlburg.infinity)
    ;fpstacknext = (Camlburg.infinity)
    ;fpstacknextl = (Camlburg.infinity)
    ;fpstacktop = (Camlburg.infinity)
    ;fpstacktopl = (Camlburg.infinity)
    ;fpuctl = (Camlburg.infinity)
    ;fpuctll = (Camlburg.infinity)
    ;fpustatus = (Camlburg.infinity)
    ;fpustatusl = (Camlburg.infinity)
    ;i2f_mem = (Camlburg.infinity)
    ;immed = (Camlburg.infinity)
    ;immed8 = (Camlburg.infinity)
    ;inst = (Camlburg.infinity)
    ;inthandler = (Camlburg.infinity)
    ;lconst = (Camlburg.infinity)
    ;mem = (Camlburg.infinity)
    ;meml = (Camlburg.infinity)
    ;minusfour = (Camlburg.infinity)
    ;one3 = (Camlburg.infinity)
    ;pic = (Camlburg.infinity)
    ;pop = (Camlburg.infinity)
    ;push = (Camlburg.infinity)
    ;reg = (Camlburg.infinity)
    ;reg8 = (Camlburg.infinity)
    ;reg8H = (Camlburg.infinity)
    ;reg_cl = (Camlburg.infinity)
    ;reg_cll = (Camlburg.infinity)
    ;regabcd = (Camlburg.infinity)
    ;regabcdl = (Camlburg.infinity)
    ;regl = (Camlburg.infinity)
    ;regl8 = (Camlburg.infinity)
    ;regl8H = (Camlburg.infinity)
    ;regl_ecx = (Camlburg.infinity)
    ;regpair = (Camlburg.infinity)
    ;regpairl = (Camlburg.infinity)
    ;shamt = (Camlburg.infinity)
    ;slot = (Camlburg.infinity)
    ;slotaddr = (Camlburg.infinity)
    ;stacknext = (Camlburg.infinity)
    ;stacktop = (Camlburg.infinity)
    ;target = (Camlburg.infinity)
    ;two3 = (Camlburg.infinity)
    ;vfp = (Camlburg.infinity)
    ;vfpl = (Camlburg.infinity)
    ;zero = (Camlburg.infinity)
    ;_Add38 = (Camlburg.infinity)
    ;_Add48 = (Camlburg.infinity)
    ;_Add49 = (Camlburg.infinity)
    ;_Ah2flags73 = (Camlburg.infinity)
    ;_BitInsert72 = (Camlburg.infinity)
    ;_Cmp3 = (Camlburg.infinity)
    ;_F2f52 = (Camlburg.infinity)
    ;_F2f54 = (Camlburg.infinity)
    ;_F2i58 = (Camlburg.infinity)
    ;_Fadd60 = (Camlburg.infinity)
    ;_Fcmp70 = (Camlburg.infinity)
    ;_Fdiv64 = (Camlburg.infinity)
    ;_Fdiv66 = (Camlburg.infinity)
    ;_Fetch1 = (Camlburg.infinity)
    ;_Fetch13 = (Camlburg.infinity)
    ;_Fetch2 = (Camlburg.infinity)
    ;_Fetch24 = (Camlburg.infinity)
    ;_Fetch26 = (Camlburg.infinity)
    ;_Fetch28 = (Camlburg.infinity)
    ;_Fetch30 = (Camlburg.infinity)
    ;_Fetch31 = (Camlburg.infinity)
    ;_Fetch32 = (Camlburg.infinity)
    ;_Fetch41 = (Camlburg.infinity)
    ;_Fetch47 = (Camlburg.infinity)
    ;_Fetch7 = (Camlburg.infinity)
    ;_Fetch75 = (Camlburg.infinity)
    ;_Fetch9 = (Camlburg.infinity)
    ;_Flags2ah74 = (Camlburg.infinity)
    ;_Fmul68 = (Camlburg.infinity)
    ;_Frnd69 = (Camlburg.infinity)
    ;_Fsub62 = (Camlburg.infinity)
    ;_Goto25 = (Camlburg.infinity)
    ;_Goto36 = (Camlburg.infinity)
    ;_Goto40 = (Camlburg.infinity)
    ;_Goto42 = (Camlburg.infinity)
    ;_Goto45 = (Camlburg.infinity)
    ;_Goto46 = (Camlburg.infinity)
    ;_Guarded23 = (Camlburg.infinity)
    ;_I2f56 = (Camlburg.infinity)
    ;_Llr16 = (Camlburg.infinity)
    ;_Llr18 = (Camlburg.infinity)
    ;_Llr20 = (Camlburg.infinity)
    ;_Lobits12 = (Camlburg.infinity)
    ;_Lobits15 = (Camlburg.infinity)
    ;_Lobits5 = (Camlburg.infinity)
    ;_Mem27 = (Camlburg.infinity)
    ;_Mem29 = (Camlburg.infinity)
    ;_Par17 = (Camlburg.infinity)
    ;_Par19 = (Camlburg.infinity)
    ;_Par21 = (Camlburg.infinity)
    ;_Par43 = (Camlburg.infinity)
    ;_Reg14 = (Camlburg.infinity)
    ;_Store22 = (Camlburg.infinity)
    ;_Store37 = (Camlburg.infinity)
    ;_Store4 = (Camlburg.infinity)
    ;_Store44 = (Camlburg.infinity)
    ;_Store51 = (Camlburg.infinity)
    ;_Store53 = (Camlburg.infinity)
    ;_Store55 = (Camlburg.infinity)
    ;_Store57 = (Camlburg.infinity)
    ;_Store59 = (Camlburg.infinity)
    ;_Store61 = (Camlburg.infinity)
    ;_Store63 = (Camlburg.infinity)
    ;_Store65 = (Camlburg.infinity)
    ;_Store67 = (Camlburg.infinity)
    ;_Store71 = (Camlburg.infinity)
    ;_Sub39 = (Camlburg.infinity)
    ;_Sub50 = (Camlburg.infinity)
    ;_Sx6 = (Camlburg.infinity)
    ;_Sx8 = (Camlburg.infinity)
    ;_X86_subflags34 = (Camlburg.infinity)
    ;_X86_subflags35 = (Camlburg.infinity)
    ;_Zx10 = (Camlburg.infinity)
    ;_Zx11 = (Camlburg.infinity)
    ;_Zx33 = (Camlburg.infinity)
    }


let rec
update_ah =
    fun nt x ->
        if nt.Camlburg.cost >= x.ah.Camlburg.cost then
            x
        else
            { x with ah = (nt) }
and update_ahval =
    fun nt x ->
        if nt.Camlburg.cost >= x.ahval.Camlburg.cost then
            x
        else
            { x with ahval = (nt) }
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
                                
# 981 "x86rec.mlb"
                                ( "<" ^ any ^ ">" )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with any = (nt) }
and update_ax =
    fun nt x ->
        if nt.Camlburg.cost >= x.ax.Camlburg.cost then
            x
        else
            { x with ax = (nt) }
and update_bare_reg =
    fun nt x ->
        if nt.Camlburg.cost >= x.bare_reg.Camlburg.cost then
            x
        else
            (fun x ->
                (update_regl
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let bare_reg = x.bare_reg.Camlburg.action ()
                            in
                                
# 639 "x86rec.mlb"
                                ( regname 32 bare_reg )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with bare_reg = (nt) }
and update_bare_regabcd =
    fun nt x ->
        if nt.Camlburg.cost >= x.bare_regabcd.Camlburg.cost then
            x
        else
            (fun x ->
                (update_regabcdl
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let
                                bare_regabcd =
                                x.bare_regabcd.Camlburg.action ()
                            in
                                
# 641 "x86rec.mlb"
                                ( regname 32 bare_regabcd )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with bare_regabcd = (nt) }
and update_const =
    fun nt x ->
        if nt.Camlburg.cost >= x.const.Camlburg.cost then
            x
        else
            (fun x ->
                (update_immed
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let const = x.const.Camlburg.action ()
                            in
                                
# 677 "x86rec.mlb"
                                ( "$" ^ const )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with const = (nt) }
and update_const8 =
    fun nt x ->
        if nt.Camlburg.cost >= x.const8.Camlburg.cost then
            x
        else
            (fun x ->
                (update_immed8
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let const8 = x.const8.Camlburg.action ()
                            in
                                
# 678 "x86rec.mlb"
                                ( "$" ^ const8 )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with const8 = (nt) }
and update_disp =
    fun nt x ->
        if nt.Camlburg.cost >= x.disp.Camlburg.cost then
            x
        else
            { x with disp = (nt) }
and update_eaddr =
    fun nt x ->
        if nt.Camlburg.cost >= x.eaddr.Camlburg.cost then
            x
        else
            (fun x ->
                (update_eaddri
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let eaddr = x.eaddr.Camlburg.action ()
                            in
                                
# 671 "x86rec.mlb"
                                ( eaddr    )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with eaddr = (nt) }
and update_eaddr_shr_k =
    fun nt x ->
        if nt.Camlburg.cost >= x.eaddr_shr_k.Camlburg.cost then
            x
        else
            { x with eaddr_shr_k = (nt) }
and update_eaddrbit0 =
    fun nt x ->
        if nt.Camlburg.cost >= x.eaddrbit0.Camlburg.cost then
            x
        else
            { x with eaddrbit0 = (nt) }
and update_eaddrbitk =
    fun nt x ->
        if nt.Camlburg.cost >= x.eaddrbitk.Camlburg.cost then
            x
        else
            { x with eaddrbitk = (nt) }
and update_eaddri =
    fun nt x ->
        if nt.Camlburg.cost >= x.eaddri.Camlburg.cost then
            x
        else
            { x with eaddri = (nt) }
and update_eaddrl =
    fun nt x ->
        if nt.Camlburg.cost >= x.eaddrl.Camlburg.cost then
            x
        else
            { x with eaddrl = (nt) }
and update_eax =
    fun nt x ->
        if nt.Camlburg.cost >= x.eax.Camlburg.cost then
            x
        else
            { x with eax = (nt) }
and update_ecx =
    fun nt x ->
        if nt.Camlburg.cost >= x.ecx.Camlburg.cost then
            x
        else
            { x with ecx = (nt) }
and update_edi =
    fun nt x ->
        if nt.Camlburg.cost >= x.edi.Camlburg.cost then
            x
        else
            { x with edi = (nt) }
and update_edx =
    fun nt x ->
        if nt.Camlburg.cost >= x.edx.Camlburg.cost then
            x
        else
            { x with edx = (nt) }
and update_edx_eax =
    fun nt x ->
        if nt.Camlburg.cost >= x.edx_eax.Camlburg.cost then
            x
        else
            { x with edx_eax = (nt) }
and update_eflags =
    fun nt x ->
        if nt.Camlburg.cost >= x.eflags.Camlburg.cost then
            x
        else
            { x with eflags = (nt) }
and update_eight =
    fun nt x ->
        if nt.Camlburg.cost >= x.eight.Camlburg.cost then
            x
        else
            { x with eight = (nt) }
and update_eip =
    fun nt x ->
        if nt.Camlburg.cost >= x.eip.Camlburg.cost then
            x
        else
            { x with eip = (nt) }
and update_esi =
    fun nt x ->
        if nt.Camlburg.cost >= x.esi.Camlburg.cost then
            x
        else
            { x with esi = (nt) }
and update_esp =
    fun nt x ->
        if nt.Camlburg.cost >= x.esp.Camlburg.cost then
            x
        else
            { x with esp = (nt) }
and update_espl =
    fun nt x ->
        if nt.Camlburg.cost >= x.espl.Camlburg.cost then
            x
        else
            { x with espl = (nt) }
and update_fiadd =
    fun nt x ->
        if nt.Camlburg.cost >= x.fiadd.Camlburg.cost then
            x
        else
            { x with fiadd = (nt) }
and update_four =
    fun nt x ->
        if nt.Camlburg.cost >= x.four.Camlburg.cost then
            x
        else
            { x with four = (nt) }
and update_fpccl =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpccl.Camlburg.cost then
            x
        else
            { x with fpccl = (nt) }
and update_fppop =
    fun nt x ->
        if nt.Camlburg.cost >= x.fppop.Camlburg.cost then
            x
        else
            { x with fppop = (nt) }
and update_fppop2 =
    fun nt x ->
        if nt.Camlburg.cost >= x.fppop2.Camlburg.cost then
            x
        else
            { x with fppop2 = (nt) }
and update_fppush =
    fun nt x ->
        if nt.Camlburg.cost >= x.fppush.Camlburg.cost then
            x
        else
            { x with fppush = (nt) }
and update_fpsp =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpsp.Camlburg.cost then
            x
        else
            { x with fpsp = (nt) }
and update_fpstack1 =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpstack1.Camlburg.cost then
            x
        else
            { x with fpstack1 = (nt) }
and update_fpstack1l =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpstack1l.Camlburg.cost then
            x
        else
            { x with fpstack1l = (nt) }
and update_fpstacknext =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpstacknext.Camlburg.cost then
            x
        else
            { x with fpstacknext = (nt) }
and update_fpstacknextl =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpstacknextl.Camlburg.cost then
            x
        else
            { x with fpstacknextl = (nt) }
and update_fpstacktop =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpstacktop.Camlburg.cost then
            x
        else
            { x with fpstacktop = (nt) }
and update_fpstacktopl =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpstacktopl.Camlburg.cost then
            x
        else
            { x with fpstacktopl = (nt) }
and update_fpuctl =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpuctl.Camlburg.cost then
            x
        else
            { x with fpuctl = (nt) }
and update_fpuctll =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpuctll.Camlburg.cost then
            x
        else
            { x with fpuctll = (nt) }
and update_fpustatus =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpustatus.Camlburg.cost then
            x
        else
            { x with fpustatus = (nt) }
and update_fpustatusl =
    fun nt x ->
        if nt.Camlburg.cost >= x.fpustatusl.Camlburg.cost then
            x
        else
            { x with fpustatusl = (nt) }
and update_i2f_mem =
    fun nt x ->
        if nt.Camlburg.cost >= x.i2f_mem.Camlburg.cost then
            x
        else
            { x with i2f_mem = (nt) }
and update_immed =
    fun nt x ->
        if nt.Camlburg.cost >= x.immed.Camlburg.cost then
            x
        else
            (fun x ->
                (update_eaddri
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let immed = x.immed.Camlburg.action ()
                            in
                                
# 672 "x86rec.mlb"
                                ( immed    )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with immed = (nt) }
and update_immed8 =
    fun nt x ->
        if nt.Camlburg.cost >= x.immed8.Camlburg.cost then
            x
        else
            { x with immed8 = (nt) }
and update_inst =
    fun nt x ->
        if nt.Camlburg.cost >= x.inst.Camlburg.cost then
            x
        else
            { x with inst = (nt) }
and update_inthandler =
    fun nt x ->
        if nt.Camlburg.cost >= x.inthandler.Camlburg.cost then
            x
        else
            { x with inthandler = (nt) }
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
                                
# 865 "x86rec.mlb"
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
            (fun x ->
                (update_eaddrl
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let meml = x.meml.Camlburg.action ()
                            in
                                
# 658 "x86rec.mlb"
                                ( meml )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with meml = (nt) }
and update_minusfour =
    fun nt x ->
        if nt.Camlburg.cost >= x.minusfour.Camlburg.cost then
            x
        else
            { x with minusfour = (nt) }
and update_one3 =
    fun nt x ->
        if nt.Camlburg.cost >= x.one3.Camlburg.cost then
            x
        else
            { x with one3 = (nt) }
and update_pic =
    fun nt x ->
        if nt.Camlburg.cost >= x.pic.Camlburg.cost then
            x
        else
            { x with pic = (nt) }
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
and update_reg =
    fun nt x ->
        if nt.Camlburg.cost >= x.reg.Camlburg.cost then
            x
        else
            { x with reg = (nt) }
and update_reg8 =
    fun nt x ->
        if nt.Camlburg.cost >= x.reg8.Camlburg.cost then
            x
        else
            { x with reg8 = (nt) }
and update_reg8H =
    fun nt x ->
        if nt.Camlburg.cost >= x.reg8H.Camlburg.cost then
            x
        else
            { x with reg8H = (nt) }
and update_reg_cl =
    fun nt x ->
        if nt.Camlburg.cost >= x.reg_cl.Camlburg.cost then
            x
        else
            { x with reg_cl = (nt) }
and update_reg_cll =
    fun nt x ->
        if nt.Camlburg.cost >= x.reg_cll.Camlburg.cost then
            x
        else
            { x with reg_cll = (nt) }
and update_regabcd =
    fun nt x ->
        if nt.Camlburg.cost >= x.regabcd.Camlburg.cost then
            x
        else
            { x with regabcd = (nt) }
and update_regabcdl =
    fun nt x ->
        if nt.Camlburg.cost >= x.regabcdl.Camlburg.cost then
            x
        else
            { x with regabcdl = (nt) }
and update_regl =
    fun nt x ->
        if nt.Camlburg.cost >= x.regl.Camlburg.cost then
            x
        else
            (fun x ->
                (update_eaddrl
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let regl = x.regl.Camlburg.action ()
                            in
                                
# 657 "x86rec.mlb"
                                ( regl )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with regl = (nt) }
and update_regl8 =
    fun nt x ->
        if nt.Camlburg.cost >= x.regl8.Camlburg.cost then
            x
        else
            { x with regl8 = (nt) }
and update_regl8H =
    fun nt x ->
        if nt.Camlburg.cost >= x.regl8H.Camlburg.cost then
            x
        else
            { x with regl8H = (nt) }
and update_regl_ecx =
    fun nt x ->
        if nt.Camlburg.cost >= x.regl_ecx.Camlburg.cost then
            x
        else
            { x with regl_ecx = (nt) }
and update_regpair =
    fun nt x ->
        if nt.Camlburg.cost >= x.regpair.Camlburg.cost then
            x
        else
            { x with regpair = (nt) }
and update_regpairl =
    fun nt x ->
        if nt.Camlburg.cost >= x.regpairl.Camlburg.cost then
            x
        else
            { x with regpairl = (nt) }
and update_shamt =
    fun nt x ->
        if nt.Camlburg.cost >= x.shamt.Camlburg.cost then
            x
        else
            { x with shamt = (nt) }
and update_slot =
    fun nt x ->
        if nt.Camlburg.cost >= x.slot.Camlburg.cost then
            x
        else
            { x with slot = (nt) }
and update_slotaddr =
    fun nt x ->
        if nt.Camlburg.cost >= x.slotaddr.Camlburg.cost then
            x
        else
            { x with slotaddr = (nt) }
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
and update_target =
    fun nt x ->
        if nt.Camlburg.cost >= x.target.Camlburg.cost then
            x
        else
            { x with target = (nt) }
and update_two3 =
    fun nt x ->
        if nt.Camlburg.cost >= x.two3.Camlburg.cost then
            x
        else
            { x with two3 = (nt) }
and update_vfp =
    fun nt x ->
        if nt.Camlburg.cost >= x.vfp.Camlburg.cost then
            x
        else
            (fun x ->
                (update_slotaddr
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let vfp = x.vfp.Camlburg.action ()
                            in
                                
# 683 "x86rec.mlb"
                                ( "%vfp" )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with vfp = (nt) }
and update_vfpl =
    fun nt x ->
        if nt.Camlburg.cost >= x.vfpl.Camlburg.cost then
            x
        else
            { x with vfpl = (nt) }
and update_zero =
    fun nt x ->
        if nt.Camlburg.cost >= x.zero.Camlburg.cost then
            x
        else
            { x with zero = (nt) }
and update__Add38 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add38.Camlburg.cost then
            x
        else
            { x with _Add38 = (nt) }
and update__Add48 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add48.Camlburg.cost then
            x
        else
            { x with _Add48 = (nt) }
and update__Add49 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add49.Camlburg.cost then
            x
        else
            { x with _Add49 = (nt) }
and update__Ah2flags73 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Ah2flags73.Camlburg.cost then
            x
        else
            { x with _Ah2flags73 = (nt) }
and update__BitInsert72 =
    fun nt x ->
        if nt.Camlburg.cost >= x._BitInsert72.Camlburg.cost then
            x
        else
            { x with _BitInsert72 = (nt) }
and update__Cmp3 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Cmp3.Camlburg.cost then
            x
        else
            { x with _Cmp3 = (nt) }
and update__F2f52 =
    fun nt x ->
        if nt.Camlburg.cost >= x._F2f52.Camlburg.cost then
            x
        else
            { x with _F2f52 = (nt) }
and update__F2f54 =
    fun nt x ->
        if nt.Camlburg.cost >= x._F2f54.Camlburg.cost then
            x
        else
            { x with _F2f54 = (nt) }
and update__F2i58 =
    fun nt x ->
        if nt.Camlburg.cost >= x._F2i58.Camlburg.cost then
            x
        else
            { x with _F2i58 = (nt) }
and update__Fadd60 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fadd60.Camlburg.cost then
            x
        else
            { x with _Fadd60 = (nt) }
and update__Fcmp70 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fcmp70.Camlburg.cost then
            x
        else
            { x with _Fcmp70 = (nt) }
and update__Fdiv64 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fdiv64.Camlburg.cost then
            x
        else
            { x with _Fdiv64 = (nt) }
and update__Fdiv66 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fdiv66.Camlburg.cost then
            x
        else
            { x with _Fdiv66 = (nt) }
and update__Fetch1 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch1.Camlburg.cost then
            x
        else
            { x with _Fetch1 = (nt) }
and update__Fetch13 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch13.Camlburg.cost then
            x
        else
            { x with _Fetch13 = (nt) }
and update__Fetch2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch2.Camlburg.cost then
            x
        else
            { x with _Fetch2 = (nt) }
and update__Fetch24 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch24.Camlburg.cost then
            x
        else
            { x with _Fetch24 = (nt) }
and update__Fetch26 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch26.Camlburg.cost then
            x
        else
            { x with _Fetch26 = (nt) }
and update__Fetch28 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch28.Camlburg.cost then
            x
        else
            { x with _Fetch28 = (nt) }
and update__Fetch30 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch30.Camlburg.cost then
            x
        else
            { x with _Fetch30 = (nt) }
and update__Fetch31 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch31.Camlburg.cost then
            x
        else
            { x with _Fetch31 = (nt) }
and update__Fetch32 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch32.Camlburg.cost then
            x
        else
            { x with _Fetch32 = (nt) }
and update__Fetch41 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch41.Camlburg.cost then
            x
        else
            { x with _Fetch41 = (nt) }
and update__Fetch47 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch47.Camlburg.cost then
            x
        else
            { x with _Fetch47 = (nt) }
and update__Fetch7 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch7.Camlburg.cost then
            x
        else
            { x with _Fetch7 = (nt) }
and update__Fetch75 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch75.Camlburg.cost then
            x
        else
            { x with _Fetch75 = (nt) }
and update__Fetch9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch9.Camlburg.cost then
            x
        else
            { x with _Fetch9 = (nt) }
and update__Flags2ah74 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Flags2ah74.Camlburg.cost then
            x
        else
            { x with _Flags2ah74 = (nt) }
and update__Fmul68 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fmul68.Camlburg.cost then
            x
        else
            { x with _Fmul68 = (nt) }
and update__Frnd69 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Frnd69.Camlburg.cost then
            x
        else
            { x with _Frnd69 = (nt) }
and update__Fsub62 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fsub62.Camlburg.cost then
            x
        else
            { x with _Fsub62 = (nt) }
and update__Goto25 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto25.Camlburg.cost then
            x
        else
            { x with _Goto25 = (nt) }
and update__Goto36 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto36.Camlburg.cost then
            x
        else
            { x with _Goto36 = (nt) }
and update__Goto40 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto40.Camlburg.cost then
            x
        else
            { x with _Goto40 = (nt) }
and update__Goto42 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto42.Camlburg.cost then
            x
        else
            { x with _Goto42 = (nt) }
and update__Goto45 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto45.Camlburg.cost then
            x
        else
            { x with _Goto45 = (nt) }
and update__Goto46 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto46.Camlburg.cost then
            x
        else
            { x with _Goto46 = (nt) }
and update__Guarded23 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Guarded23.Camlburg.cost then
            x
        else
            { x with _Guarded23 = (nt) }
and update__I2f56 =
    fun nt x ->
        if nt.Camlburg.cost >= x._I2f56.Camlburg.cost then
            x
        else
            { x with _I2f56 = (nt) }
and update__Llr16 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Llr16.Camlburg.cost then
            x
        else
            { x with _Llr16 = (nt) }
and update__Llr18 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Llr18.Camlburg.cost then
            x
        else
            { x with _Llr18 = (nt) }
and update__Llr20 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Llr20.Camlburg.cost then
            x
        else
            { x with _Llr20 = (nt) }
and update__Lobits12 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Lobits12.Camlburg.cost then
            x
        else
            { x with _Lobits12 = (nt) }
and update__Lobits15 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Lobits15.Camlburg.cost then
            x
        else
            { x with _Lobits15 = (nt) }
and update__Lobits5 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Lobits5.Camlburg.cost then
            x
        else
            { x with _Lobits5 = (nt) }
and update__Mem27 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mem27.Camlburg.cost then
            x
        else
            { x with _Mem27 = (nt) }
and update__Mem29 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mem29.Camlburg.cost then
            x
        else
            { x with _Mem29 = (nt) }
and update__Par17 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Par17.Camlburg.cost then
            x
        else
            { x with _Par17 = (nt) }
and update__Par19 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Par19.Camlburg.cost then
            x
        else
            { x with _Par19 = (nt) }
and update__Par21 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Par21.Camlburg.cost then
            x
        else
            { x with _Par21 = (nt) }
and update__Par43 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Par43.Camlburg.cost then
            x
        else
            { x with _Par43 = (nt) }
and update__Reg14 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Reg14.Camlburg.cost then
            x
        else
            { x with _Reg14 = (nt) }
and update__Store22 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store22.Camlburg.cost then
            x
        else
            { x with _Store22 = (nt) }
and update__Store37 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store37.Camlburg.cost then
            x
        else
            { x with _Store37 = (nt) }
and update__Store4 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store4.Camlburg.cost then
            x
        else
            { x with _Store4 = (nt) }
and update__Store44 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store44.Camlburg.cost then
            x
        else
            { x with _Store44 = (nt) }
and update__Store51 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store51.Camlburg.cost then
            x
        else
            { x with _Store51 = (nt) }
and update__Store53 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store53.Camlburg.cost then
            x
        else
            { x with _Store53 = (nt) }
and update__Store55 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store55.Camlburg.cost then
            x
        else
            { x with _Store55 = (nt) }
and update__Store57 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store57.Camlburg.cost then
            x
        else
            { x with _Store57 = (nt) }
and update__Store59 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store59.Camlburg.cost then
            x
        else
            { x with _Store59 = (nt) }
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
and update__Store65 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store65.Camlburg.cost then
            x
        else
            { x with _Store65 = (nt) }
and update__Store67 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store67.Camlburg.cost then
            x
        else
            { x with _Store67 = (nt) }
and update__Store71 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store71.Camlburg.cost then
            x
        else
            { x with _Store71 = (nt) }
and update__Sub39 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub39.Camlburg.cost then
            x
        else
            { x with _Sub39 = (nt) }
and update__Sub50 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sub50.Camlburg.cost then
            x
        else
            { x with _Sub50 = (nt) }
and update__Sx6 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sx6.Camlburg.cost then
            x
        else
            { x with _Sx6 = (nt) }
and update__Sx8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sx8.Camlburg.cost then
            x
        else
            { x with _Sx8 = (nt) }
and update__X86_subflags34 =
    fun nt x ->
        if nt.Camlburg.cost >= x._X86_subflags34.Camlburg.cost then
            x
        else
            { x with _X86_subflags34 = (nt) }
and update__X86_subflags35 =
    fun nt x ->
        if nt.Camlburg.cost >= x._X86_subflags35.Camlburg.cost then
            x
        else
            { x with _X86_subflags35 = (nt) }
and update__Zx10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zx10.Camlburg.cost then
            x
        else
            { x with _Zx10 = (nt) }
and update__Zx11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zx11.Camlburg.cost then
            x
        else
            { x with _Zx11 = (nt) }
and update__Zx33 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zx33.Camlburg.cost then
            x
        else
            { x with _Zx33 = (nt) }


let rec
conZx =
    fun arg1 ->
        (update__Zx10
            {Camlburg.cost = (arg1._Fetch7.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch7.Camlburg.action ()
                    in
                        let (src, nw) = _v1 in (src ,nw))
            })
            ((update__Zx11
                {Camlburg.cost = (arg1._Fetch9.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch9.Camlburg.action ()
                        in
                            let (src, nw) = _v1 in (src ,nw))
                })
                ((update__Zx33
                    {Camlburg.cost = (arg1.reg_cl.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let reg_cl = arg1.reg_cl.Camlburg.action ()
                            in
                                reg_cl)
                    })
                    ((update_any
                        {Camlburg.cost = (arg1.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let any = arg1.any.Camlburg.action ()
                                in
                                    
# 1021 "x86rec.mlb"
                                    ( "Zx(" ^ any ^ ")" )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conXor =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.any.Camlburg.action ()
                    and y = arg2.any.Camlburg.action ()
                    in
                        
# 994 "x86rec.mlb"
                        ( "Xor(" ^ x ^ ", " ^ y ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conX86op =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let string = arg1
                    in
                        
# 1029 "x86rec.mlb"
                        ( "X86op(" ^ string ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conX86ccop =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let string = arg1
                    and cc = arg2.any.Camlburg.action ()
                    in
                        
# 1028 "x86rec.mlb"
                        ( "X86ccop(" ^ string ^ ", " ^ cc ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conX86_subflags =
    fun arg1 arg2 ->
        (update__X86_subflags34
            {Camlburg.cost =
                (arg1.eaddr.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let l = arg1.eaddr.Camlburg.action ()
                    and r = arg2.reg.Camlburg.action ()
                    in
                        (l ,r))
            })
            ((update__X86_subflags35
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2.eaddri.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let l = arg1.reg.Camlburg.action ()
                        and r = arg2.eaddri.Camlburg.action ()
                        in
                            (l ,r))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let l = arg1.any.Camlburg.action ()
                            and r = arg2.any.Camlburg.action ()
                            in
                                
# 1026 "x86rec.mlb"
                                ( "X86_subflags(" ^ l ^ ", " ^ r ^ ")" )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conX86_addflags =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let l = arg1.any.Camlburg.action ()
                    and r = arg2.any.Camlburg.action ()
                    in
                        
# 1027 "x86rec.mlb"
                        ( "X86_addflags(" ^ l ^ ", " ^ r ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conX86IdtPc =
    fun arg1 ->
        (update_inthandler
            {Camlburg.cost = (arg1.immed.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let immed = arg1.immed.Camlburg.action ()
                    in
                        
# 891 "x86rec.mlb"
                        ( immed )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conWithundefflags =
    fun arg1 arg2 arg3 arg4 arg5 ->
        (update_inst
            (Camlburg.choice
                [{Camlburg.cost =
                    (arg1.edx_eax.Camlburg.cost
                    +
                    (Camlburg.matches "quot") arg2
                    +
                    (Camlburg.matches "rem") arg3
                    +
                    arg4.eaddr.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let dst = arg1.edx_eax.Camlburg.action ()
                        and src = arg4.eaddr.Camlburg.action ()
                        and w = arg5
                        in
                            
# 796 "x86rec.mlb"
                            ( sprintf "idiv%s %s, %%eax" (suffix w) src )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.edx_eax.Camlburg.cost
                    +
                    (Camlburg.matches "divu") arg2
                    +
                    (Camlburg.matches "modu") arg3
                    +
                    arg4.eaddr.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let dst = arg1.edx_eax.Camlburg.action ()
                        and src = arg4.eaddr.Camlburg.action ()
                        and w = arg5
                        in
                            
# 800 "x86rec.mlb"
                            ( sprintf "div%s %s, %%eax" (suffix w) src )
                            
# 000 "/dev/stdout"
)
                }]))
            inf
and conWithlflags8H =
    fun arg1 arg2 arg3 arg4 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let dst = arg1.any.Camlburg.action ()
                    and string = arg2
                    and src = arg3.any.Camlburg.action ()
                    and fo = arg4
                    in
                        
# 1063 "x86rec.mlb"
                        ( "Withlflags8H(" ^ dst ^","^string^ "," ^ src ^ "," ^ fo ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (let logical = arg2
                        in
                            
# 847 "x86rec.mlb"
                            ( match logical with "and" | "or" | "xor" -> 0 | _ -> infinity )
                            
# 000 "/dev/stdout"

                        +
                        arg1.regabcdl.Camlburg.cost
                        +
                        arg3.reg8H.Camlburg.cost
                        +
                        (Camlburg.matches "x86_logicflags") arg4)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regabcdl.Camlburg.action ()
                            and logical = arg2
                            and src = arg3.reg8H.Camlburg.action ()
                            in
                                
# 848 "x86rec.mlb"
                                ( sprintf "%s%s %s,%s" logical (suffix 8)  src (hregname dst) )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (let logical = arg2
                        in
                            
# 851 "x86rec.mlb"
                            ( match logical with "and" | "or" | "xor" -> 0 | _ -> infinity )
                            
# 000 "/dev/stdout"

                        +
                        arg1.regabcdl.Camlburg.cost
                        +
                        arg3.immed8.Camlburg.cost
                        +
                        (Camlburg.matches "x86_logicflags") arg4)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regabcdl.Camlburg.action ()
                            and logical = arg2
                            and src = arg3.immed8.Camlburg.action ()
                            in
                                
# 852 "x86rec.mlb"
                                ( sprintf "%s%s %s,%s" logical (suffix 8) src (hregname dst) )
                                
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conWithlflags =
    fun arg1 arg2 arg3 arg4 arg5 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let dst = arg1.any.Camlburg.action ()
                    and string = arg2
                    and src = arg3.any.Camlburg.action ()
                    and w = arg4
                    and fo = arg5
                    in
                        
# 1059 "x86rec.mlb"
                        ( "Withlflags(" ^ dst ^","^string^ "," ^ src ^ "," ^ string_of_int w ^ 
      ","^ fo ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (let logical = arg2
                        and w = arg4
                        in
                            
# 835 "x86rec.mlb"
                            ( match logical with "and" | "or" | "xor" -> 0 | _ -> infinity )
                            
# 000 "/dev/stdout"

                        +
                        arg1.eaddrl.Camlburg.cost
                        +
                        arg3.reg.Camlburg.cost
                        +
                        (Camlburg.matches "x86_logicflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.eaddrl.Camlburg.action ()
                            and logical = arg2
                            and src = arg3.reg.Camlburg.action ()
                            and w = arg4
                            in
                                
# 836 "x86rec.mlb"
                                ( s "%s%s %s,%s" logical (suffix w) src dst )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (let logical = arg2
                        and w = arg4
                        in
                            
# 839 "x86rec.mlb"
                            ( match logical with "and" | "or" | "xor" -> 0 | _ -> infinity )
                            
# 000 "/dev/stdout"

                        +
                        arg1.regl.Camlburg.cost
                        +
                        arg3.eaddri.Camlburg.cost
                        +
                        (Camlburg.matches "x86_logicflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and logical = arg2
                            and src = arg3.eaddri.Camlburg.action ()
                            and w = arg4
                            in
                                
# 840 "x86rec.mlb"
                                ( s "%s%s %s,%s" logical (suffix w) src dst )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (let logical = arg2
                        and w = arg4
                        in
                            
# 843 "x86rec.mlb"
                            ( (guard (w=8)) + match logical with "and" | "or" | "xor" -> 0 | _ -> infinity )
                            
# 000 "/dev/stdout"

                        +
                        arg1.regl8H.Camlburg.cost
                        +
                        arg3.immed8.Camlburg.cost
                        +
                        (Camlburg.matches "x86_logicflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl8H.Camlburg.action ()
                            and logical = arg2
                            and src = arg3.immed8.Camlburg.action ()
                            and w = arg4
                            in
                                
# 844 "x86rec.mlb"
                                ( sprintf "%s%s %s,%s" logical (suffix 8)  src dst )
                                
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conWithcarryzero =
    fun arg1 arg2 arg3 arg4 arg5 ->
        (update_inst
            {Camlburg.cost =
                (arg1.eaddrl.Camlburg.cost + (Camlburg.matches "addc") arg2
                +
                arg3.reg.Camlburg.cost
                +
                (Camlburg.matches "x86_adcflags") arg5)
            ;Camlburg.action =
                (fun () ->
                    let dst = arg1.eaddrl.Camlburg.action ()
                    and src = arg3.reg.Camlburg.action ()
                    and w = arg4
                    in
                        
# 764 "x86rec.mlb"
                        ( s "add%s %s,%s" (suffix w) src dst )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conWithcarryflags =
    fun arg1 arg2 arg3 arg4 arg5 ->
        (update_inst
            (Camlburg.choice
                [{Camlburg.cost =
                    (arg1.eaddrl.Camlburg.cost
                    +
                    (Camlburg.matches "addc") arg2
                    +
                    arg3.reg.Camlburg.cost
                    +
                    (Camlburg.matches "x86_adcflags") arg5)
                ;Camlburg.action =
                    (fun () ->
                        let dst = arg1.eaddrl.Camlburg.action ()
                        and src = arg3.reg.Camlburg.action ()
                        and w = arg4
                        in
                            
# 758 "x86rec.mlb"
                            ( s "adc%s %s,%s" (suffix w) src dst )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.eaddrl.Camlburg.cost
                    +
                    (Camlburg.matches "subb") arg2
                    +
                    arg3.reg.Camlburg.cost
                    +
                    (Camlburg.matches "x86_sbbflags") arg5)
                ;Camlburg.action =
                    (fun () ->
                        let dst = arg1.eaddrl.Camlburg.action ()
                        and src = arg3.reg.Camlburg.action ()
                        and w = arg4
                        in
                            
# 761 "x86rec.mlb"
                            ( s "sbb%s %s,%s" (suffix w) src dst )
                            
# 000 "/dev/stdout"
)
                }]))
            inf
and conWithaflagsunary =
    fun arg1 arg2 arg3 arg4 ->
        (update_inst
            {Camlburg.cost =
                ((Camlburg.matches "neg") arg1 + arg2.eaddrl.Camlburg.cost
                +
                (Camlburg.matches "x86_negflags") arg4)
            ;Camlburg.action =
                (fun () ->
                    let dst = arg2.eaddrl.Camlburg.action ()
                    and w = arg3
                    in
                        
# 779 "x86rec.mlb"
                        ( s "neg%s %s" (suffix w) dst )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conWithaflags =
    fun arg1 arg2 arg3 arg4 arg5 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let dst = arg1.any.Camlburg.action ()
                    and string = arg2
                    and src = arg3.any.Camlburg.action ()
                    and w = arg4
                    and fo = arg5
                    in
                        
# 1055 "x86rec.mlb"
                        ( "Withaflags(" ^ dst ^","^string^ "," ^ src ^ "," ^ string_of_int w ^ 
      ","^ fo ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1.eaddrl.Camlburg.cost
                        +
                        (Camlburg.matches "add") arg2
                        +
                        arg3.reg.Camlburg.cost
                        +
                        (Camlburg.matches "x86_addflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.eaddrl.Camlburg.action ()
                            and src = arg3.reg.Camlburg.action ()
                            and w = arg4
                            in
                                
# 767 "x86rec.mlb"
                                ( s "add%s %s,%s" (suffix w) src dst )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost
                        +
                        (Camlburg.matches "add") arg2
                        +
                        arg3.eaddri.Camlburg.cost
                        +
                        (Camlburg.matches "x86_addflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and src = arg3.eaddri.Camlburg.action ()
                            and w = arg4
                            in
                                
# 770 "x86rec.mlb"
                                ( s "add%s %s,%s" (suffix w) src dst )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.eaddrl.Camlburg.cost
                        +
                        (Camlburg.matches "sub") arg2
                        +
                        arg3.reg.Camlburg.cost
                        +
                        (Camlburg.matches "x86_subflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.eaddrl.Camlburg.action ()
                            and src = arg3.reg.Camlburg.action ()
                            and w = arg4
                            in
                                
# 773 "x86rec.mlb"
                                ( s "sub%s %s,%s" (suffix w) src dst )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost
                        +
                        (Camlburg.matches "sub") arg2
                        +
                        arg3.eaddri.Camlburg.cost
                        +
                        (Camlburg.matches "x86_subflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and src = arg3.eaddri.Camlburg.action ()
                            and w = arg4
                            in
                                
# 776 "x86rec.mlb"
                                ( s "sub%s %s,%s" (suffix w) src dst )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.eaddrl.Camlburg.cost
                        +
                        (Camlburg.matches "mul") arg2
                        +
                        arg3.reg.Camlburg.cost
                        +
                        (Camlburg.matches "x86_mulflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.eaddrl.Camlburg.action ()
                            and src = arg3.reg.Camlburg.action ()
                            and w = arg4
                            in
                                
# 782 "x86rec.mlb"
                                ( s "imul %s %s, %s" (suffix w) src dst )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost
                        +
                        (Camlburg.matches "mul") arg2
                        +
                        arg3.eaddri.Camlburg.cost
                        +
                        (Camlburg.matches "x86_mulflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and src = arg3.eaddri.Camlburg.action ()
                            and w = arg4
                            in
                                
# 785 "x86rec.mlb"
                                ( s "imul%s %s,%s" (suffix w) src dst )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost
                        +
                        (Camlburg.matches "shl") arg2
                        +
                        arg3._Zx33.Camlburg.cost
                        +
                        (Camlburg.matches "x86_shlflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg3._Zx33.Camlburg.action ()
                            and w = arg4
                            in
                                let reg_cl = _v1
                                in
                                    
# 803 "x86rec.mlb"
                                    ( sprintf "shl%s %%cl, %s" (suffix w) dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost
                        +
                        (Camlburg.matches "shra") arg2
                        +
                        arg3._Zx33.Camlburg.cost
                        +
                        (Camlburg.matches "x86_shraflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg3._Zx33.Camlburg.action ()
                            and w = arg4
                            in
                                let reg_cl = _v1
                                in
                                    
# 806 "x86rec.mlb"
                                    ( sprintf "sar%s %%cl, %s" (suffix w) dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost
                        +
                        (Camlburg.matches "shrl") arg2
                        +
                        arg3._Zx33.Camlburg.cost
                        +
                        (Camlburg.matches "x86_shrlflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg3._Zx33.Camlburg.action ()
                            and w = arg4
                            in
                                let reg_cl = _v1
                                in
                                    
# 809 "x86rec.mlb"
                                    ( sprintf "shr%s %%cl, %s" (suffix w) dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost
                        +
                        (Camlburg.matches "rotl") arg2
                        +
                        arg3._Zx33.Camlburg.cost
                        +
                        (Camlburg.matches "x86_rotlflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg3._Zx33.Camlburg.action ()
                            and w = arg4
                            in
                                let reg_cl = _v1
                                in
                                    
# 812 "x86rec.mlb"
                                    ( sprintf "rol%s %%cl, %s" (suffix w) dst )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.regl.Camlburg.cost
                        +
                        (Camlburg.matches "rotr") arg2
                        +
                        arg3._Zx33.Camlburg.cost
                        +
                        (Camlburg.matches "x86_rotrflags") arg5)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and _v1 = arg3._Zx33.Camlburg.action ()
                            and w = arg4
                            in
                                let reg_cl = _v1
                                in
                                    
# 815 "x86rec.mlb"
                                    ( sprintf "ror%s %%cl, %s" (suffix w) dst )
                                    
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conUnaryInPlace =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let dst = arg1.any.Camlburg.action ()
                    and string = arg2
                    and w = arg3
                    in
                        
# 1052 "x86rec.mlb"
                        ( "UnaryInPlace(" ^ dst ^","^string^ "," ^ string_of_int w ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1.regl.Camlburg.cost
                        +
                        (Camlburg.matches "com") arg2)
                    ;Camlburg.action =
                        (fun () ->
                            let dst = arg1.regl.Camlburg.action ()
                            and w = arg3
                            in
                                
# 832 "x86rec.mlb"
                                ( s "not%s %s" (suffix w) dst )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.fpstacktopl.Camlburg.cost
                        +
                        (Camlburg.matches "fabs") arg2)
                    ;Camlburg.action =
                        (fun () ->
                            let
                                fpstacktopl =
                                arg1.fpstacktopl.Camlburg.action ()
                            and w = arg3
                            in
                                
# 939 "x86rec.mlb"
                                ( "fabs" )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.fpstacktopl.Camlburg.cost
                        +
                        (Camlburg.matches "fsqrt") arg2)
                    ;Camlburg.action =
                        (fun () ->
                            let
                                fpstacktopl =
                                arg1.fpstacktopl.Camlburg.action ()
                            and w = arg3
                            in
                                
# 940 "x86rec.mlb"
                                ( "fsqrt" )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1.fpstacktopl.Camlburg.cost
                        +
                        (Camlburg.matches "fneg") arg2)
                    ;Camlburg.action =
                        (fun () ->
                            let
                                fpstacktopl =
                                arg1.fpstacktopl.Camlburg.action ()
                            and w = arg3
                            in
                                
# 941 "x86rec.mlb"
                                ( "fchs" )
                                
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conTrue =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 983 "x86rec.mlb"
                    ( "True"  )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conSx =
    fun arg1 ->
        (update__Sx6
            {Camlburg.cost = (arg1._Fetch7.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch7.Camlburg.action ()
                    in
                        let (src, nw) = _v1 in (src ,nw))
            })
            ((update__Sx8
                {Camlburg.cost = (arg1._Fetch9.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch9.Camlburg.action ()
                        in
                            let (src, nw) = _v1 in (src ,nw))
                })
                ((update_any
                    {Camlburg.cost = (arg1.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let any = arg1.any.Camlburg.action ()
                            in
                                
# 1020 "x86rec.mlb"
                                ( "Sx(" ^ any ^ ")" )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conSub =
    fun arg1 arg2 ->
        (update__Sub39
            {Camlburg.cost =
                (arg1.esp.Camlburg.cost + arg2.four.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let esp = arg1.esp.Camlburg.action ()
                    and four = arg2.four.Camlburg.action ()
                    in
                        (esp ,four))
            })
            ((update__Sub50
                {Camlburg.cost =
                    (arg1._Fetch47.Camlburg.cost + arg2.one3.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch47.Camlburg.action ()
                        and one3 = arg2.one3.Camlburg.action ()
                        in
                            let (fpsp, w) = _v1 in (fpsp ,w ,one3))
                })
                inf)
and conStore =
    fun arg1 arg2 arg3 ->
        (update__Store22
            {Camlburg.cost =
                (arg1._Mem27.Camlburg.cost + arg2._Fetch28.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Mem27.Camlburg.action ()
                    and _v2 = arg2._Fetch28.Camlburg.action ()
                    and w8 = arg3
                    in
                        let (edi2, w5, c6, w7) = _v2
                        in
                            let (esi2, w4, c11) = _v1
                            in
                                (esi2 ,w4 ,c11 ,edi2 ,w5 ,c6 ,w7 ,w8))
            })
            ((update__Store37
                {Camlburg.cost =
                    (arg1.espl.Camlburg.cost + arg2.eaddri.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let espl = arg1.espl.Camlburg.action ()
                        and frame = arg2.eaddri.Camlburg.action ()
                        and w = arg3
                        in
                            (espl ,frame ,w))
                })
                ((update__Store4
                    {Camlburg.cost =
                        (arg1.espl.Camlburg.cost
                        +
                        arg2.slotaddr.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let espl = arg1.espl.Camlburg.action ()
                            and s2 = arg2.slotaddr.Camlburg.action ()
                            and w = arg3
                            in
                                (espl ,s2 ,w))
                    })
                    ((update__Store44
                        {Camlburg.cost =
                            (arg1.stacknext.Camlburg.cost
                            +
                            arg2._Fetch26.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let
                                    stacknext =
                                    arg1.stacknext.Camlburg.action ()
                                and _v1 = arg2._Fetch26.Camlburg.action ()
                                and y = arg3
                                in
                                    let (eip, x) = _v1
                                    in
                                        (stacknext ,eip ,x ,y))
                        })
                        ((update__Store51
                            {Camlburg.cost =
                                (arg1.fpstacknextl.Camlburg.cost
                                +
                                arg2._F2f52.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let
                                        fpstacknextl =
                                        arg1.fpstacknextl.Camlburg.action ()
                                    and _v1 = arg2._F2f52.Camlburg.action ()
                                    and w2 = arg3
                                    in
                                        let (s, d, mem) = _v1
                                        in
                                            (fpstacknextl ,s ,d ,mem ,w2))
                            })
                            ((update__Store53
                                {Camlburg.cost =
                                    (arg1.meml.Camlburg.cost
                                    +
                                    arg2._F2f54.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            dst =
                                            arg1.meml.Camlburg.action ()
                                        and
                                            _v1 =
                                            arg2._F2f54.Camlburg.action ()
                                        and w2 = arg3
                                        in
                                            let (s, d, fpstacktop) = _v1
                                            in
                                                (dst ,s ,d ,fpstacktop ,w2))
                                })
                                ((update__Store55
                                    {Camlburg.cost =
                                        (arg1.fpstacknextl.Camlburg.cost
                                        +
                                        arg2._I2f56.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                fpstacknextl =
                                                arg1.fpstacknextl.Camlburg.action
                                                    ()
                                            and
                                                _v1 =
                                                arg2._I2f56.Camlburg.action
                                                    ()
                                            and w2 = arg3
                                            in
                                                let (s, d, mem) = _v1
                                                in
                                                    (fpstacknextl
                                                    ,s
                                                    ,d
                                                    ,mem
                                                    ,w2))
                                    })
                                    ((update__Store57
                                        {Camlburg.cost =
                                            (arg1.meml.Camlburg.cost
                                            +
                                            arg2._F2i58.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    dst =
                                                    arg1.meml.Camlburg.action
                                                        ()
                                                and
                                                    _v1 =
                                                    arg2._F2i58.Camlburg.action
                                                        ()
                                                and w2 = arg3
                                                in
                                                    let
                                                        (s, d, fpstacktop) =
                                                        _v1
                                                    in
                                                        (dst
                                                        ,s
                                                        ,d
                                                        ,fpstacktop
                                                        ,w2))
                                        })
                                        ((update__Store59
                                            {Camlburg.cost =
                                                (arg1.fpstack1l.Camlburg.cost
                                                +
                                                arg2._Fadd60.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        fpstack1l =
                                                        arg1.fpstack1l.Camlburg.action
                                                            ()
                                                    and
                                                        _v1 =
                                                        arg2._Fadd60.Camlburg.action
                                                            ()
                                                    and w = arg3
                                                    in
                                                        let
                                                            (fpstacktop,
                                                            fpstack1) =
                                                            _v1
                                                        in
                                                            (fpstack1l
                                                            ,fpstacktop
                                                            ,fpstack1
                                                            ,w))
                                            })
                                            ((update__Store61
                                                {Camlburg.cost =
                                                    (arg1.fpstack1l.Camlburg.cost
                                                    +
                                                    arg2._Fsub62.Camlburg.cost)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        let
                                                            fpstack1l =
                                                            arg1.fpstack1l.Camlburg.action
                                                                ()
                                                        and
                                                            _v1 =
                                                            arg2._Fsub62.Camlburg.action
                                                                ()
                                                        and w = arg3
                                                        in
                                                            let
                                                                (fpstacktop,
                                                                fpstack1) =
                                                                _v1
                                                            in
                                                                (fpstack1l
                                                                ,fpstacktop
                                                                ,fpstack1
                                                                ,w))
                                                })
                                                ((update__Store63
                                                    {Camlburg.cost =
                                                        (arg1.fpstack1l.Camlburg.cost
                                                        +
                                                        arg2._Fdiv64.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                fpstack1l =
                                                                arg1.fpstack1l.Camlburg.action
                                                                    ()
                                                            and
                                                                _v1 =
                                                                arg2._Fdiv64.Camlburg.action
                                                                    ()
                                                            and w = arg3
                                                            in
                                                                let
                                                                    (fpstacktop,
                                                                    fpstack1) =
                                                                    _v1
                                                                in
                                                                    (fpstack1l
                                                                    ,fpstacktop
                                                                    ,fpstack1
                                                                    ,w))
                                                    })
                                                    ((update__Store65
                                                        {Camlburg.cost =
                                                            (arg1.fpstack1l.Camlburg.cost
                                                            +
                                                            arg2._Fdiv66.Camlburg.cost)
                                                        ;Camlburg.action =
                                                            (fun () ->
                                                                let
                                                                    fpstack1l =
                                                                    arg1.fpstack1l.Camlburg.action
                                                                        ()
                                                                and
                                                                    _v1 =
                                                                    arg2._Fdiv66.Camlburg.action
                                                                        ()
                                                                and w = arg3
                                                                in
                                                                    let
                                                                        (fpstack1,
                                                                        fpstacktop) =
                                                                        _v1
                                                                    in
                                                                        (fpstack1l
                                                                        ,fpstack1
                                                                        ,fpstacktop
                                                                        ,w))
                                                        })
                                                        ((update__Store67
                                                            {Camlburg.cost =
                                                                (arg1.fpstack1l.Camlburg.cost
                                                                +
                                                                arg2._Fmul68.Camlburg.cost)
                                                            ;Camlburg.action =
                                                                (fun () ->
                                                                    let
                                                                        fpstack1l =
                                                                        arg1.fpstack1l.Camlburg.action
                                                                            ()
                                                                    and
                                                                        _v1 =
                                                                        arg2._Fmul68.Camlburg.action
                                                                            ()
                                                                    and
                                                                        w =
                                                                        arg3
                                                                    in
                                                                        let
                                                                            (fpstacktop,
                                                                            fpstack1) =
                                                                            _v1
                                                                        in
                                                                            (fpstack1l
                                                                            ,fpstacktop
                                                                            ,fpstack1
                                                                            ,w))
                                                            })
                                                            ((update__Store71
                                                                {Camlburg.cost =
                                                                    (arg1.fpccl.Camlburg.cost
                                                                    +
                                                                    arg2._Fcmp70.Camlburg.cost)
                                                                ;Camlburg.action =
                                                                    (fun
                                                                    ()
                                                                    ->
                                                                        let
                                                                            fpccl =
                                                                            arg1.fpccl.Camlburg.action
                                                                                ()
                                                                        and
                                                                            _v1 =
                                                                            arg2._Fcmp70.Camlburg.action
                                                                                ()
                                                                        and
                                                                            w =
                                                                            arg3
                                                                        in
                                                                            let
                                                                                (fpstacktop,
                                                                                fpstack1) =
                                                                                _v1
                                                                            in
                                                                                (fpccl
                                                                                ,fpstacktop
                                                                                ,fpstack1
                                                                                ,w))
                                                                })
                                                                ((update_any
                                                                    {Camlburg.cost =
                                                                        (arg1.any.Camlburg.cost
                                                                        +
                                                                        arg2.any.Camlburg.cost)
                                                                    ;Camlburg.action =
                                                                        (fun
                                                                        ()
                                                                        ->
                                                                            let
                                                                                dst =
                                                                                arg1.any.Camlburg.action
                                                                                    ()
                                                                            and
                                                                                src =
                                                                                arg2.any.Camlburg.action
                                                                                    ()
                                                                            and
                                                                                w =
                                                                                arg3
                                                                            in
                                                                                
# 1039 "x86rec.mlb"
                                                                                ( "Store(" ^ dst ^ "," ^ src ^ "," ^ string_of_int w ^ ")" )
                                                                                
# 000 "/dev/stdout"
)
                                                                    })
                                                                    ((update_inst
                                                                        (Camlburg.choice
                                                                            [{Camlburg.cost =
                                                                                (arg1.regl.Camlburg.cost
                                                                                +
                                                                                arg2.slotaddr.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.regl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        slotaddr =
                                                                                        arg2.slotaddr.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 690 "x86rec.mlb"
                                                                                        ( sprintf "%s := %s" dst slotaddr )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.slot.Camlburg.cost
                                                                                +
                                                                                arg2._Lobits5.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        slot =
                                                                                        arg1.slot.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Lobits5.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        nw =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (reg,
                                                                                            lw) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 695 "x86rec.mlb"
                                                                                            ( s "bits%d[%s] := %%lobits%d(%s)" nw slot nw reg )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.eaddrl.Camlburg.cost
                                                                                +
                                                                                arg2.pic.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.eaddrl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        pic =
                                                                                        arg2.pic.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 704 "x86rec.mlb"
                                                                                        ( s "mov%s %s,%s" (suffix w) pic dst )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.eaddrl.Camlburg.cost
                                                                                +
                                                                                arg2.immed.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.eaddrl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        src =
                                                                                        arg2.immed.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 707 "x86rec.mlb"
                                                                                        ( s "mov%s %s,%s" (suffix w) src dst )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.regl.Camlburg.cost
                                                                                +
                                                                                arg2.lconst.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.regl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        src =
                                                                                        arg2.lconst.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 710 "x86rec.mlb"
                                                                                        ( s "lea%s %s,%s" (suffix w) src dst )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.eaddrl.Camlburg.cost
                                                                                +
                                                                                arg2.reg.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.eaddrl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        src =
                                                                                        arg2.reg.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 714 "x86rec.mlb"
                                                                                        ( s "mov%s %s,%s" (suffix w) src dst )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.regl.Camlburg.cost
                                                                                +
                                                                                arg2.eaddri.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.regl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        src =
                                                                                        arg2.eaddri.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 717 "x86rec.mlb"
                                                                                        ( s "mov%s %s,%s" (suffix w) src dst )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.eaddrl.Camlburg.cost
                                                                                +
                                                                                arg2._Sx6.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.eaddrl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Sx6.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (src,
                                                                                            nw) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 721 "x86rec.mlb"
                                                                                            ( s "movs%s%s %s,%s" (suffix nw) (suffix w) src dst )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.regl.Camlburg.cost
                                                                                +
                                                                                arg2._Sx8.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.regl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Sx8.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (src,
                                                                                            nw) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 724 "x86rec.mlb"
                                                                                            ( s "movs%s%s %s,%s" (suffix nw) (suffix w) src dst )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.eaddrl.Camlburg.cost
                                                                                +
                                                                                arg2._Zx10.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.eaddrl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Zx10.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (src,
                                                                                            nw) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 728 "x86rec.mlb"
                                                                                            ( s "movz%s%s %s,%s" (suffix nw) (suffix w) src dst )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.regl.Camlburg.cost
                                                                                +
                                                                                arg2._Zx11.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.regl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Zx11.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (src,
                                                                                            nw) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 731 "x86rec.mlb"
                                                                                            ( s "movz%s%s %s,%s" (suffix nw) (suffix w) src dst )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.eaddrl.Camlburg.cost
                                                                                +
                                                                                arg2._Lobits12.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.eaddrl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Lobits12.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        nw =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (n,
                                                                                            fw,
                                                                                            lw) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 735 "x86rec.mlb"
                                                                                            ( s "mov%s %s,%s" (suffix nw) (regname nw n) dst )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.regl.Camlburg.cost
                                                                                +
                                                                                arg2._Lobits15.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.regl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Lobits15.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        nw =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (src,
                                                                                            sw) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 738 "x86rec.mlb"
                                                                                            ( s "mov%s %s,%s" (suffix nw) src dst )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (let
                                                                                    w =
                                                                                    arg3
                                                                                in
                                                                                    
# 751 "x86rec.mlb"
                                                                                    (guard (w = 32))
                                                                                    
# 000 "/dev/stdout"

                                                                                +
                                                                                arg1.regl.Camlburg.cost
                                                                                +
                                                                                arg2.disp.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        dst =
                                                                                        arg1.regl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        disp =
                                                                                        arg2.disp.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 752 "x86rec.mlb"
                                                                                        ( s "leal %s, %s" disp dst )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.fpstacktopl.Camlburg.cost
                                                                                +
                                                                                arg2._Frnd69.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        fpstacktopl =
                                                                                        arg1.fpstacktopl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Frnd69.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            fpstacktop =
                                                                                            _v1
                                                                                        in
                                                                                            
# 942 "x86rec.mlb"
                                                                                            ( "frndint" )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.fpstacktopl.Camlburg.cost
                                                                                +
                                                                                arg2.fiadd.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        fpstacktopl =
                                                                                        arg1.fpstacktopl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        fiadd =
                                                                                        arg2.fiadd.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 947 "x86rec.mlb"
                                                                                        ( let s, mem = fiadd in sprintf "fiadd%s %s" (suffix s) mem )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.fpccl.Camlburg.cost
                                                                                +
                                                                                arg2._Fcmp70.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        fpccl =
                                                                                        arg1.fpccl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Fcmp70.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (fpstacktop,
                                                                                            fpstack1) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 955 "x86rec.mlb"
                                                                                            ( "fcom" )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.meml.Camlburg.cost
                                                                                +
                                                                                arg2.fpuctl.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        meml =
                                                                                        arg1.meml.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        fpuctl =
                                                                                        arg2.fpuctl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 961 "x86rec.mlb"
                                                                                        ( s "fnstcw %s" meml )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.fpuctll.Camlburg.cost
                                                                                +
                                                                                arg2.mem.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        fpuctll =
                                                                                        arg1.fpuctll.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        mem =
                                                                                        arg2.mem.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 962 "x86rec.mlb"
                                                                                        ( s "fldcw %s"  mem  )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.eax.Camlburg.cost
                                                                                +
                                                                                arg2._BitInsert72.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        eax =
                                                                                        arg1.eax.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._BitInsert72.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        sw =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (zero,
                                                                                            eax,
                                                                                            w,
                                                                                            fpustatus) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 974 "x86rec.mlb"
                                                                                            ( "fstsw %ax" )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (let
                                                                                    w =
                                                                                    arg3
                                                                                in
                                                                                    
# 975 "x86rec.mlb"
                                                                                    ( guard (w = 16) )
                                                                                    
# 000 "/dev/stdout"

                                                                                +
                                                                                arg1.ax.Camlburg.cost
                                                                                +
                                                                                arg2.fpustatus.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        ax =
                                                                                        arg1.ax.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        fpustatus =
                                                                                        arg2.fpustatus.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        
# 975 "x86rec.mlb"
                                                                                        ( s "fstsw %s" ax )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            }
                                                                            ;{Camlburg.cost =
                                                                                (arg1.ah.Camlburg.cost
                                                                                +
                                                                                arg2._Flags2ah74.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        ah =
                                                                                        arg1.ah.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Flags2ah74.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (eflags,
                                                                                            fw) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 977 "x86rec.mlb"
                                                                                            ( "lahf" )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            }]))
                                                                        ((update_pop
                                                                            {Camlburg.cost =
                                                                                (arg1.espl.Camlburg.cost
                                                                                +
                                                                                arg2._Add38.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        espl =
                                                                                        arg1.espl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        _v1 =
                                                                                        arg2._Add38.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        sw =
                                                                                        arg3
                                                                                    in
                                                                                        let
                                                                                            (esp,
                                                                                            four) =
                                                                                            _v1
                                                                                        in
                                                                                            
# 875 "x86rec.mlb"
                                                                                            ( () )
                                                                                            
# 000 "/dev/stdout"
)
                                                                            })
                                                                            inf))))))))))))))))
and conSlice =
    fun arg1 arg2 arg3 ->
        (update_ah
            {Camlburg.cost =
                (let w = arg1
                and lsb = arg2
                in
                    
# 970 "x86rec.mlb"
                    ( guard (w = 8  && lsb = 8))
                    
# 000 "/dev/stdout"

                +
                arg3.eax.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let w = arg1
                    and lsb = arg2
                    and eax = arg3.eax.Camlburg.action ()
                    in
                        
# 970 "x86rec.mlb"
                        ( "%ah" )
                        
# 000 "/dev/stdout"
)
            })
            ((update_any
                {Camlburg.cost = (arg3.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let n = arg1
                        and lsb = arg2
                        and y = arg3.any.Camlburg.action ()
                        in
                            
# 1016 "x86rec.mlb"
                            ( sprintf "Slice(%d, %d, %s)" n lsb y )
                            
# 000 "/dev/stdout"
)
                })
                ((update_ax
                    {Camlburg.cost =
                        (let w = arg1
                        and lsb = arg2
                        in
                            
# 969 "x86rec.mlb"
                            ( guard (w = 16 && lsb = 0))
                            
# 000 "/dev/stdout"

                        +
                        arg3.eax.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let w = arg1
                            and lsb = arg2
                            and eax = arg3.eax.Camlburg.action ()
                            in
                                
# 969 "x86rec.mlb"
                                ( "%ax" )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_reg_cll
                        {Camlburg.cost =
                            (let sw = arg1
                            and n = arg2
                            in
                                
# 666 "x86rec.mlb"
                                ( guard (sw=8 && n=0) )
                                
# 000 "/dev/stdout"

                            +
                            arg3.regl_ecx.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let sw = arg1
                                and n = arg2
                                and
                                    regl_ecx =
                                    arg3.regl_ecx.Camlburg.action ()
                                in
                                    
# 666 "x86rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_regl8
                            {Camlburg.cost =
                                (let w = arg1
                                and i = arg2
                                in
                                    
# 642 "x86rec.mlb"
                                    ( guard (w=8) )
                                    
# 000 "/dev/stdout"

                                +
                                arg3.bare_regabcd.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let w = arg1
                                    and i = arg2
                                    and
                                        bare_regabcd =
                                        arg3.bare_regabcd.Camlburg.action ()
                                    in
                                        
# 642 "x86rec.mlb"
                                        ( Rg.regname8 i bare_regabcd )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_regl8H
                                {Camlburg.cost =
                                    (let w = arg1
                                    and i = arg2
                                    in
                                        
# 643 "x86rec.mlb"
                                        ( guard (w=8 && i=8) )
                                        
# 000 "/dev/stdout"

                                    +
                                    arg3.regabcdl.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let w = arg1
                                        and i = arg2
                                        and
                                            regabcdl =
                                            arg3.regabcdl.Camlburg.action ()
                                        in
                                            
# 643 "x86rec.mlb"
                                            ( hregname regabcdl )
                                            
# 000 "/dev/stdout"
)
                                })
                                inf)))))
and conSinglebit =
    fun arg1 arg2 arg3 arg4 ->
        (update_any
            {Camlburg.cost =
                (arg2.any.Camlburg.cost + arg3.any.Camlburg.cost
                +
                arg4.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let string = arg1
                    and x = arg2.any.Camlburg.action ()
                    and y = arg3.any.Camlburg.action ()
                    and z = arg4.any.Camlburg.action ()
                    in
                        
# 1006 "x86rec.mlb"
                        ( sprintf "Singlebit(%s, %s, %s, %s)" string x y z )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conShift =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost =
                (arg2.any.Camlburg.cost + arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let string = arg1
                    and x = arg2.any.Camlburg.action ()
                    and y = arg3.any.Camlburg.action ()
                    in
                        
# 1032 "x86rec.mlb"
                        ( sprintf "Shift(%s, %s, %s)" string x y )
                        
# 000 "/dev/stdout"
)
            })
            ((update_eaddr_shr_k
                (Camlburg.choice
                    [{Camlburg.cost =
                        ((Camlburg.matches "shra") arg1
                        +
                        arg2.eaddr.Camlburg.cost
                        +
                        arg3.shamt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let eaddr = arg2.eaddr.Camlburg.action ()
                            and shamt = arg3.shamt.Camlburg.action ()
                            in
                                
# 857 "x86rec.mlb"
                                ( eaddr, shamt )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        ((Camlburg.matches "shrl") arg1
                        +
                        arg2.eaddr.Camlburg.cost
                        +
                        arg3.shamt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let eaddr = arg2.eaddr.Camlburg.action ()
                            and shamt = arg3.shamt.Camlburg.action ()
                            in
                                
# 858 "x86rec.mlb"
                                ( eaddr, shamt )
                                
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conSetflags =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 1048 "x86rec.mlb"
                        ( "Setflags(" ^ any ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost = (arg1._X86_subflags34.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._X86_subflags34.Camlburg.action ()
                            in
                                let (l, r) = _v1
                                in
                                    
# 828 "x86rec.mlb"
                                    ( s "cmp%s %s,%s" (suffix 32) r l )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost = (arg1._X86_subflags35.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._X86_subflags35.Camlburg.action ()
                            in
                                let (l, r) = _v1
                                in
                                    
# 830 "x86rec.mlb"
                                    ( s "cmp%s %s,%s" (suffix 32) r l )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost = (arg1._Ah2flags73.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Ah2flags73.Camlburg.action ()
                            in
                                let ahval = _v1
                                in
                                    
# 976 "x86rec.mlb"
                                    ( "sahf" )
                                    
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conRepmovs =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost
                +
                arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let esi = arg1.any.Camlburg.action ()
                    and edi = arg2.any.Camlburg.action ()
                    and ecx = arg3.any.Camlburg.action ()
                    in
                        
# 1043 "x86rec.mlb"
                        ( "Repmovs(" ^ esi ^ "," ^ edi ^ "," ^ ecx ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conRegPair =
    fun arg1 arg2 ->
        (update_edx_eax
            {Camlburg.cost =
                (arg1.edx.Camlburg.cost + arg2.eax.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let edx = arg1.edx.Camlburg.action ()
                    and eax = arg2.eax.Camlburg.action ()
                    in
                        
# 793 "x86rec.mlb"
                        ( () )
                        
# 000 "/dev/stdout"
)
            })
            ((update_regpairl
                {Camlburg.cost =
                    (arg1.regl.Camlburg.cost + arg2.regl.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let reg1 = arg1.regl.Camlburg.action ()
                        and reg2 = arg2.regl.Camlburg.action ()
                        in
                            
# 645 "x86rec.mlb"
                            ( reg2 )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conReg =
    fun arg1 arg2 ->
        (update__Reg14
            {Camlburg.cost = ((Camlburg.matches 'r') arg1)
            ;Camlburg.action = (fun () -> let n = arg2 in n)
            })
            ((update_any
                {Camlburg.cost = (0)
                ;Camlburg.action =
                    (fun () ->
                        let char = arg1
                        and n = arg2
                        in
                            
# 1035 "x86rec.mlb"
                            ( sprintf "Reg(%s, %d)"   (Char.escaped char) n )
                            
# 000 "/dev/stdout"
)
                })
                ((update_bare_reg
                    {Camlburg.cost = ((Camlburg.matches 'r') arg1)
                    ;Camlburg.action =
                        (fun () ->
                            let n = arg2
                            in
                                
# 638 "x86rec.mlb"
                                ( n )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_bare_regabcd
                        {Camlburg.cost =
                            (let n = arg2
                            in
                                
# 640 "x86rec.mlb"
                                ( guard (n < 4) )
                                
# 000 "/dev/stdout"

                            +
                            (Camlburg.matches 'r') arg1)
                        ;Camlburg.action =
                            (fun () ->
                                let n = arg2
                                in
                                    
# 640 "x86rec.mlb"
                                    ( n )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_eax
                            {Camlburg.cost =
                                (let n = arg2
                                in
                                    
# 964 "x86rec.mlb"
                                    ( guard (n = 0))
                                    
# 000 "/dev/stdout"

                                +
                                (Camlburg.matches 'r') arg1)
                            ;Camlburg.action =
                                (fun () ->
                                    let n = arg2
                                    in
                                        
# 964 "x86rec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_ecx
                                {Camlburg.cost =
                                    (let n = arg2
                                    in
                                        
# 965 "x86rec.mlb"
                                        ( guard (n = 1))
                                        
# 000 "/dev/stdout"

                                    +
                                    (Camlburg.matches 'r') arg1)
                                ;Camlburg.action =
                                    (fun () ->
                                        let n = arg2
                                        in
                                            
# 965 "x86rec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_edi
                                    {Camlburg.cost =
                                        (let n = arg2
                                        in
                                            
# 968 "x86rec.mlb"
                                            ( guard (n = 7))
                                            
# 000 "/dev/stdout"

                                        +
                                        (Camlburg.matches 'r') arg1)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let n = arg2
                                            in
                                                
# 968 "x86rec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_edx
                                        {Camlburg.cost =
                                            (let n = arg2
                                            in
                                                
# 966 "x86rec.mlb"
                                                ( guard (n = 2))
                                                
# 000 "/dev/stdout"

                                            +
                                            (Camlburg.matches 'r') arg1)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let n = arg2
                                                in
                                                    
# 966 "x86rec.mlb"
                                                    ( () )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        ((update_eflags
                                            {Camlburg.cost =
                                                (let n = arg2
                                                in
                                                    
# 854 "x86rec.mlb"
                                                    ( guard (n = SS.indices.SS.cc))
                                                    
# 000 "/dev/stdout"

                                                +
                                                (Camlburg.matches 'c') arg1)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let n = arg2
                                                    in
                                                        
# 854 "x86rec.mlb"
                                                        ( () )
                                                        
# 000 "/dev/stdout"
)
                                            })
                                            ((update_eip
                                                {Camlburg.cost =
                                                    (let n = arg2
                                                    in
                                                        
# 870 "x86rec.mlb"
                                                        ( guard (n = SS.indices.SS.pc))
                                                        
# 000 "/dev/stdout"

                                                    +
                                                    (Camlburg.matches 'c')
                                                        arg1)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        let n = arg2
                                                        in
                                                            
# 870 "x86rec.mlb"
                                                            ( () )
                                                            
# 000 "/dev/stdout"
)
                                                })
                                                ((update_esi
                                                    {Camlburg.cost =
                                                        (let n = arg2
                                                        in
                                                            
# 967 "x86rec.mlb"
                                                            ( guard (n = 6))
                                                            
# 000 "/dev/stdout"

                                                        +
                                                        (Camlburg.matches
                                                            'r')
                                                            arg1)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let n = arg2
                                                            in
                                                                
# 967 "x86rec.mlb"
                                                                ( () )
                                                                
# 000 "/dev/stdout"
)
                                                    })
                                                    ((update_espl
                                                        {Camlburg.cost =
                                                            (let n = arg2
                                                            in
                                                                
# 869 "x86rec.mlb"
                                                                ( guard (n = 4))
                                                                
# 000 "/dev/stdout"

                                                            +
                                                            (Camlburg.matches
                                                                'r')
                                                                arg1)
                                                        ;Camlburg.action =
                                                            (fun () ->
                                                                let n = arg2
                                                                in
                                                                    
# 869 "x86rec.mlb"
                                                                    ( () )
                                                                    
# 000 "/dev/stdout"
)
                                                        })
                                                        ((update_fpsp
                                                            {Camlburg.cost =
                                                                (let n = arg2
                                                                in
                                                                    
# 904 "x86rec.mlb"
                                                                    ( guard (n = 0))
                                                                    
# 000 "/dev/stdout"

                                                                +
                                                                (Camlburg.matches
                                                                    'F')
                                                                    arg1)
                                                            ;Camlburg.action =
                                                                (fun () ->
                                                                    let
                                                                        n =
                                                                        arg2
                                                                    in
                                                                        
# 904 "x86rec.mlb"
                                                                        ( () )
                                                                        
# 000 "/dev/stdout"
)
                                                            })
                                                            ((update_regl
                                                                {Camlburg.cost =
                                                                    ((Camlburg.matches
                                                                        't')
                                                                        arg1)
                                                                ;Camlburg.action =
                                                                    (fun
                                                                    ()
                                                                    ->
                                                                        let
                                                                            n =
                                                                            arg2
                                                                        in
                                                                            
# 652 "x86rec.mlb"
                                                                            ( sprintf "temporary register %d" n )
                                                                            
# 000 "/dev/stdout"
)
                                                                })
                                                                ((update_regl_ecx
                                                                    {Camlburg.cost =
                                                                        (let
                                                                            n =
                                                                            arg2
                                                                        in
                                                                            
# 644 "x86rec.mlb"
                                                                            ( guard (n=1) )
                                                                            
# 000 "/dev/stdout"

                                                                        +
                                                                        (Camlburg.matches
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
                                                                                
# 644 "x86rec.mlb"
                                                                                ( () )
                                                                                
# 000 "/dev/stdout"
)
                                                                    })
                                                                    ((update_vfpl
                                                                        {Camlburg.cost =
                                                                            (let
                                                                                n =
                                                                                arg2
                                                                            in
                                                                                
# 653 "x86rec.mlb"
                                                                                ( guard (n=0) )
                                                                                
# 000 "/dev/stdout"

                                                                            +
                                                                            (Camlburg.matches
                                                                                'V')
                                                                                arg1)
                                                                        ;Camlburg.action =
                                                                            (fun
                                                                            ()
                                                                            ->
                                                                                let
                                                                                    n =
                                                                                    arg2
                                                                                in
                                                                                    
# 653 "x86rec.mlb"
                                                                                    ( () )
                                                                                    
# 000 "/dev/stdout"
)
                                                                        })
                                                                        inf)))))))))))))))
and conPar =
    fun arg1 arg2 ->
        (update__Par17
            {Camlburg.cost =
                (arg1._Llr18.Camlburg.cost + arg2._Par19.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Llr18.Camlburg.action ()
                    and _v2 = arg2._Par19.Camlburg.action ()
                    in
                        let
                            (edi1,
                            n2,
                            w3,
                            esi2,
                            w4,
                            c11,
                            edi2,
                            w5,
                            c6,
                            w7,
                            w8,
                            ecx2,
                            w9,
                            eip,
                            w10) =
                            _v2
                        in
                            let (esi1, n1, w2) = _v1
                            in
                                (esi1
                                ,n1
                                ,w2
                                ,edi1
                                ,n2
                                ,w3
                                ,esi2
                                ,w4
                                ,c11
                                ,edi2
                                ,w5
                                ,c6
                                ,w7
                                ,w8
                                ,ecx2
                                ,w9
                                ,eip
                                ,w10))
            })
            ((update__Par19
                {Camlburg.cost =
                    (arg1._Llr20.Camlburg.cost + arg2._Par21.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Llr20.Camlburg.action ()
                        and _v2 = arg2._Par21.Camlburg.action ()
                        in
                            let
                                (esi2,
                                w4,
                                c11,
                                edi2,
                                w5,
                                c6,
                                w7,
                                w8,
                                ecx2,
                                w9,
                                eip,
                                w10) =
                                _v2
                            in
                                let (edi1, n2, w3) = _v1
                                in
                                    (edi1
                                    ,n2
                                    ,w3
                                    ,esi2
                                    ,w4
                                    ,c11
                                    ,edi2
                                    ,w5
                                    ,c6
                                    ,w7
                                    ,w8
                                    ,ecx2
                                    ,w9
                                    ,eip
                                    ,w10))
                })
                ((update__Par21
                    {Camlburg.cost =
                        (arg1._Store22.Camlburg.cost
                        +
                        arg2._Guarded23.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Store22.Camlburg.action ()
                            and _v2 = arg2._Guarded23.Camlburg.action ()
                            in
                                let (ecx2, w9, eip, w10) = _v2
                                in
                                    let
                                        (esi2,
                                        w4,
                                        c11,
                                        edi2,
                                        w5,
                                        c6,
                                        w7,
                                        w8) =
                                        _v1
                                    in
                                        (esi2
                                        ,w4
                                        ,c11
                                        ,edi2
                                        ,w5
                                        ,c6
                                        ,w7
                                        ,w8
                                        ,ecx2
                                        ,w9
                                        ,eip
                                        ,w10))
                    })
                    ((update__Par43
                        {Camlburg.cost =
                            (arg1._Store44.Camlburg.cost
                            +
                            arg2.push.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Store44.Camlburg.action ()
                                and push = arg2.push.Camlburg.action ()
                                in
                                    let (stacknext, eip, x, y) = _v1
                                    in
                                        (stacknext ,eip ,x ,y ,push))
                        })
                        ((update_any
                            {Camlburg.cost =
                                (arg1.any.Camlburg.cost
                                +
                                arg2.any.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let l = arg1.any.Camlburg.action ()
                                    and r = arg2.any.Camlburg.action ()
                                    in
                                        
# 1046 "x86rec.mlb"
                                        ( "Par(" ^ l ^ "," ^ r ^ ")" )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_inst
                                (Camlburg.choice
                                    [{Camlburg.cost =
                                        (arg1._Llr16.Camlburg.cost
                                        +
                                        arg2._Par17.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Llr16.Camlburg.action
                                                    ()
                                            and
                                                _v2 =
                                                arg2._Par17.Camlburg.action
                                                    ()
                                            in
                                                let
                                                    (esi1,
                                                    n1,
                                                    w2,
                                                    edi1,
                                                    n2,
                                                    w3,
                                                    esi2,
                                                    w4,
                                                    c11,
                                                    edi2,
                                                    w5,
                                                    c6,
                                                    w7,
                                                    w8,
                                                    ecx2,
                                                    w9,
                                                    eip,
                                                    w10) =
                                                    _v2
                                                in
                                                    let (ecx1, n0, w1) = _v1
                                                    in
                                                        
# 748 "x86rec.mlb"
                                                        ( "rep movsb" )
                                                        
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Goto36.Camlburg.cost
                                        +
                                        arg2._Store37.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Goto36.Camlburg.action
                                                    ()
                                            and
                                                _v2 =
                                                arg2._Store37.Camlburg.action
                                                    ()
                                            in
                                                let (espl, frame, w) = _v2
                                                in
                                                    let target = _v1
                                                    in
                                                        
# 873 "x86rec.mlb"
                                                        ( s "movl %s, %%esp; jmp %s" frame target )
                                                        
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Goto40.Camlburg.cost
                                        +
                                        arg2.pop.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Goto40.Camlburg.action
                                                    ()
                                            and
                                                pop =
                                                arg2.pop.Camlburg.action ()
                                            in
                                                let (stacktop, w) = _v1
                                                in
                                                    
# 882 "x86rec.mlb"
                                                    ( "ret" )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Goto42.Camlburg.cost
                                        +
                                        arg2._Par43.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Goto42.Camlburg.action
                                                    ()
                                            and
                                                _v2 =
                                                arg2._Par43.Camlburg.action
                                                    ()
                                            in
                                                let
                                                    (stacknext,
                                                    eip,
                                                    x,
                                                    y,
                                                    push) =
                                                    _v2
                                                in
                                                    let eaddr = _v1
                                                    in
                                                        
# 884 "x86rec.mlb"
                                                        ( "call *" ^ eaddr )
                                                        
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Goto45.Camlburg.cost
                                        +
                                        arg2._Par43.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Goto45.Camlburg.action
                                                    ()
                                            and
                                                _v2 =
                                                arg2._Par43.Camlburg.action
                                                    ()
                                            in
                                                let
                                                    (stacknext,
                                                    eip,
                                                    x,
                                                    y,
                                                    push) =
                                                    _v2
                                                in
                                                    let lconst = _v1
                                                    in
                                                        
# 886 "x86rec.mlb"
                                                        ( "call " ^ lconst )
                                                        
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Goto46.Camlburg.cost
                                        +
                                        arg2._Par43.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Goto46.Camlburg.action
                                                    ()
                                            and
                                                _v2 =
                                                arg2._Par43.Camlburg.action
                                                    ()
                                            in
                                                let
                                                    (stacknext,
                                                    eip,
                                                    x,
                                                    y,
                                                    push) =
                                                    _v2
                                                in
                                                    let inthandler = _v1
                                                    in
                                                        
# 889 "x86rec.mlb"
                                                        ( "int " ^ inthandler )
                                                        
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store51.Camlburg.cost
                                        +
                                        arg2.fppush.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store51.Camlburg.action
                                                    ()
                                            and
                                                fppush =
                                                arg2.fppush.Camlburg.action
                                                    ()
                                            in
                                                let
                                                    (fpstacknextl,
                                                    s,
                                                    d,
                                                    mem,
                                                    w2) =
                                                    _v1
                                                in
                                                    
# 919 "x86rec.mlb"
                                                    ( sprintf "fld%s %s" (fsuffix s) mem )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store53.Camlburg.cost
                                        +
                                        arg2.fppop.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store53.Camlburg.action
                                                    ()
                                            and
                                                fppop =
                                                arg2.fppop.Camlburg.action ()
                                            in
                                                let
                                                    (dst,
                                                    s,
                                                    d,
                                                    fpstacktop,
                                                    w2) =
                                                    _v1
                                                in
                                                    
# 921 "x86rec.mlb"
                                                    ( sprintf "fstp%s %s" (fsuffix d) dst )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store55.Camlburg.cost
                                        +
                                        arg2.fppush.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store55.Camlburg.action
                                                    ()
                                            and
                                                fppush =
                                                arg2.fppush.Camlburg.action
                                                    ()
                                            in
                                                let
                                                    (fpstacknextl,
                                                    s,
                                                    d,
                                                    mem,
                                                    w2) =
                                                    _v1
                                                in
                                                    
# 924 "x86rec.mlb"
                                                    ( sprintf "fild%s %s" (fisuffix s) mem )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store57.Camlburg.cost
                                        +
                                        arg2.fppop.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store57.Camlburg.action
                                                    ()
                                            and
                                                fppop =
                                                arg2.fppop.Camlburg.action ()
                                            in
                                                let
                                                    (dst,
                                                    s,
                                                    d,
                                                    fpstacktop,
                                                    w2) =
                                                    _v1
                                                in
                                                    
# 926 "x86rec.mlb"
                                                    ( sprintf "fistp%s %s" (fisuffix d) dst )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store59.Camlburg.cost
                                        +
                                        arg2.fppop.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store59.Camlburg.action
                                                    ()
                                            and
                                                fppop =
                                                arg2.fppop.Camlburg.action ()
                                            in
                                                let
                                                    (fpstack1l,
                                                    fpstacktop,
                                                    fpstack1,
                                                    w) =
                                                    _v1
                                                in
                                                    
# 929 "x86rec.mlb"
                                                    ( "faddp" )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store61.Camlburg.cost
                                        +
                                        arg2.fppop.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store61.Camlburg.action
                                                    ()
                                            and
                                                fppop =
                                                arg2.fppop.Camlburg.action ()
                                            in
                                                let
                                                    (fpstack1l,
                                                    fpstacktop,
                                                    fpstack1,
                                                    w) =
                                                    _v1
                                                in
                                                    
# 931 "x86rec.mlb"
                                                    ( "fsubp" )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store63.Camlburg.cost
                                        +
                                        arg2.fppop.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store63.Camlburg.action
                                                    ()
                                            and
                                                fppop =
                                                arg2.fppop.Camlburg.action ()
                                            in
                                                let
                                                    (fpstack1l,
                                                    fpstacktop,
                                                    fpstack1,
                                                    w) =
                                                    _v1
                                                in
                                                    
# 933 "x86rec.mlb"
                                                    ( "fdivp" )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store65.Camlburg.cost
                                        +
                                        arg2.fppop.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store65.Camlburg.action
                                                    ()
                                            and
                                                fppop =
                                                arg2.fppop.Camlburg.action ()
                                            in
                                                let
                                                    (fpstack1l,
                                                    fpstack1,
                                                    fpstacktop,
                                                    w) =
                                                    _v1
                                                in
                                                    
# 935 "x86rec.mlb"
                                                    ( "fdivrp" )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store67.Camlburg.cost
                                        +
                                        arg2.fppop.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store67.Camlburg.action
                                                    ()
                                            and
                                                fppop =
                                                arg2.fppop.Camlburg.action ()
                                            in
                                                let
                                                    (fpstack1l,
                                                    fpstacktop,
                                                    fpstack1,
                                                    w) =
                                                    _v1
                                                in
                                                    
# 937 "x86rec.mlb"
                                                    ( "fmulp" )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store71.Camlburg.cost
                                        +
                                        arg2.fppop.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store71.Camlburg.action
                                                    ()
                                            and
                                                fppop =
                                                arg2.fppop.Camlburg.action ()
                                            in
                                                let
                                                    (fpccl,
                                                    fpstacktop,
                                                    fpstack1,
                                                    w) =
                                                    _v1
                                                in
                                                    
# 957 "x86rec.mlb"
                                                    ( "fcomp" )
                                                    
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1._Store71.Camlburg.cost
                                        +
                                        arg2.fppop2.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Store71.Camlburg.action
                                                    ()
                                            and
                                                fppop2 =
                                                arg2.fppop2.Camlburg.action
                                                    ()
                                            in
                                                let
                                                    (fpccl,
                                                    fpstacktop,
                                                    fpstack1,
                                                    w) =
                                                    _v1
                                                in
                                                    
# 959 "x86rec.mlb"
                                                    ( "fcompp" )
                                                    
# 000 "/dev/stdout"
)
                                    }]))
                                inf)))))
and conPairdestwithflags =
    fun arg1 arg2 arg3 arg4 arg5 arg6 ->
        (update_inst
            (Camlburg.choice
                [{Camlburg.cost =
                    (arg1.edx.Camlburg.cost + arg2.eax.Camlburg.cost
                    +
                    arg3._Fetch32.Camlburg.cost
                    +
                    (Camlburg.matches "mulx") arg4
                    +
                    arg5.eaddr.Camlburg.cost
                    +
                    (Camlburg.matches "x86_mulxflags") arg6)
                ;Camlburg.action =
                    (fun () ->
                        let edx = arg1.edx.Camlburg.action ()
                        and eax = arg2.eax.Camlburg.action ()
                        and _v1 = arg3._Fetch32.Camlburg.action ()
                        and eaddr = arg5.eaddr.Camlburg.action ()
                        in
                            let (eax, w) = _v1
                            in
                                
# 788 "x86rec.mlb"
                                ( s "imull %s" eaddr )
                                
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.edx.Camlburg.cost + arg2.eax.Camlburg.cost
                    +
                    arg3._Fetch32.Camlburg.cost
                    +
                    (Camlburg.matches "mulux") arg4
                    +
                    arg5.eaddr.Camlburg.cost
                    +
                    (Camlburg.matches "x86_muluxflags") arg6)
                ;Camlburg.action =
                    (fun () ->
                        let edx = arg1.edx.Camlburg.action ()
                        and eax = arg2.eax.Camlburg.action ()
                        and _v1 = arg3._Fetch32.Camlburg.action ()
                        and eaddr = arg5.eaddr.Camlburg.action ()
                        in
                            let (eax, w) = _v1
                            in
                                
# 791 "x86rec.mlb"
                                ( s "mull %s" eaddr )
                                
# 000 "/dev/stdout"
)
                }]))
            inf
and conOr =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.any.Camlburg.action ()
                    and y = arg2.any.Camlburg.action ()
                    in
                        
# 993 "x86rec.mlb"
                        ( "Or("  ^ x ^ ", " ^ y ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conNop =
    fun () ->
        (update_inst
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 979 "x86rec.mlb"
                    ( "nop" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conNonLlrOp =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let string = arg1
                    in
                        
# 1031 "x86rec.mlb"
                        ( "NonLlrOp(" ^ string ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.any.Camlburg.action ()
                    and y = arg2.any.Camlburg.action ()
                    in
                        
# 999 "x86rec.mlb"
                        ( "Mul(" ^ x ^ ", " ^ y ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conMem =
    fun arg1 arg2 ->
        (update__Mem27
            {Camlburg.cost = (arg1._Fetch31.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch31.Camlburg.action ()
                    and c11 = arg2
                    in
                        let (esi2, w4) = _v1 in (esi2 ,w4 ,c11))
            })
            ((update__Mem29
                {Camlburg.cost = (arg1._Fetch30.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch30.Camlburg.action ()
                        and c6 = arg2
                        in
                            let (edi2, w5) = _v1 in (edi2 ,w5 ,c6))
                })
                ((update_any
                    {Camlburg.cost = (arg1.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let any = arg1.any.Camlburg.action ()
                            and c = arg2
                            in
                                
# 1034 "x86rec.mlb"
                                ( "Mem(" ^any ^ "," ^ string_of_int c ^ ")" )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_meml
                        (Camlburg.choice
                            [{Camlburg.cost = (arg1.reg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let reg = arg1.reg.Camlburg.action ()
                                    and c = arg2
                                    in
                                        
# 646 "x86rec.mlb"
                                        ( "(" ^ reg ^ ")" )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost = (arg1.disp.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let disp = arg1.disp.Camlburg.action ()
                                    and c = arg2
                                    in
                                        
# 647 "x86rec.mlb"
                                        ( disp )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost = (arg1.lconst.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let
                                        lconst =
                                        arg1.lconst.Camlburg.action ()
                                    and c = arg2
                                    in
                                        
# 648 "x86rec.mlb"
                                        ( lconst )
                                        
# 000 "/dev/stdout"
)
                            }]))
                        ((update_slot
                            {Camlburg.cost = (arg1.slotaddr.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let
                                        slotaddr =
                                        arg1.slotaddr.Camlburg.action ()
                                    and c = arg2
                                    in
                                        
# 692 "x86rec.mlb"
                                        ( slotaddr )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_stacknext
                                {Camlburg.cost =
                                    (let mc = arg2
                                    in
                                        
# 880 "x86rec.mlb"
                                        (guard (mc = 4))
                                        
# 000 "/dev/stdout"

                                    +
                                    arg1._Sub39.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            _v1 =
                                            arg1._Sub39.Camlburg.action ()
                                        and mc = arg2
                                        in
                                            let (esp, four) = _v1
                                            in
                                                
# 880 "x86rec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                })
                                ((update_stacktop
                                    {Camlburg.cost =
                                        (let mc = arg2
                                        in
                                            
# 879 "x86rec.mlb"
                                            (guard (mc = 4))
                                            
# 000 "/dev/stdout"

                                        +
                                        arg1.esp.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                esp =
                                                arg1.esp.Camlburg.action ()
                                            and mc = arg2
                                            in
                                                
# 879 "x86rec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    inf))))))
and conLobits =
    fun arg1 arg2 ->
        (update__Lobits12
            {Camlburg.cost = (arg1._Fetch13.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch13.Camlburg.action ()
                    and lw = arg2
                    in
                        let (n, fw) = _v1 in (n ,fw ,lw))
            })
            ((update__Lobits15
                {Camlburg.cost = (arg1.mem.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let src = arg1.mem.Camlburg.action ()
                        and sw = arg2
                        in
                            (src ,sw))
                })
                ((update__Lobits5
                    {Camlburg.cost = (arg1.reg.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let reg = arg1.reg.Camlburg.action ()
                            and lw = arg2
                            in
                                (reg ,lw))
                    })
                    ((update_any
                        {Camlburg.cost = (arg1.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let any = arg1.any.Camlburg.action ()
                                and w = arg2
                                in
                                    
# 1025 "x86rec.mlb"
                                    ( "Lobits(" ^ any ^ ", " ^ string_of_int w ^ ")" )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_eaddrbit0
                            {Camlburg.cost =
                                (let w = arg2
                                in
                                    
# 855 "x86rec.mlb"
                                    ( guard (w=1) )
                                    
# 000 "/dev/stdout"

                                +
                                arg1.eaddr.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let eaddr = arg1.eaddr.Camlburg.action ()
                                    and w = arg2
                                    in
                                        
# 855 "x86rec.mlb"
                                        ( eaddr )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_eaddrbitk
                                {Camlburg.cost =
                                    (let w = arg2
                                    in
                                        
# 856 "x86rec.mlb"
                                        ( guard (w=1) )
                                        
# 000 "/dev/stdout"

                                    +
                                    arg1.eaddr_shr_k.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            eaddr_shr_k =
                                            arg1.eaddr_shr_k.Camlburg.action
                                                ()
                                        and w = arg2
                                        in
                                            
# 856 "x86rec.mlb"
                                            ( eaddr_shr_k )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_reg8
                                    {Camlburg.cost =
                                        (let nw = arg2
                                        in
                                            
# 662 "x86rec.mlb"
                                            ( guard (nw=8) )
                                            
# 000 "/dev/stdout"

                                        +
                                        arg1._Fetch1.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Fetch1.Camlburg.action
                                                    ()
                                            and nw = arg2
                                            in
                                                let (regl8, w) = _v1
                                                in
                                                    
# 662 "x86rec.mlb"
                                                    ( regl8     )
                                                    
# 000 "/dev/stdout"
)
                                    })
                                    ((update_reg_cl
                                        {Camlburg.cost =
                                            (let nw = arg2
                                            in
                                                
# 665 "x86rec.mlb"
                                                ( guard (nw=8) )
                                                
# 000 "/dev/stdout"

                                            +
                                            arg1._Fetch2.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    _v1 =
                                                    arg1._Fetch2.Camlburg.action
                                                        ()
                                                and nw = arg2
                                                in
                                                    let (regl_ecx, w) = _v1
                                                    in
                                                        
# 665 "x86rec.mlb"
                                                        ( () )
                                                        
# 000 "/dev/stdout"
)
                                        })
                                        inf)))))))
and conLlr =
    fun arg1 arg2 arg3 arg4 ->
        (update__Llr16
            {Camlburg.cost =
                (arg1.ecx.Camlburg.cost + (Camlburg.matches "sub") arg2
                +
                arg3.const.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let ecx1 = arg1.ecx.Camlburg.action ()
                    and n0 = arg3.const.Camlburg.action ()
                    and w1 = arg4
                    in
                        (ecx1 ,n0 ,w1))
            })
            ((update__Llr18
                {Camlburg.cost =
                    (arg1.esi.Camlburg.cost + (Camlburg.matches "sub") arg2
                    +
                    arg3.const.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let esi1 = arg1.esi.Camlburg.action ()
                        and n1 = arg3.const.Camlburg.action ()
                        and w2 = arg4
                        in
                            (esi1 ,n1 ,w2))
                })
                ((update__Llr20
                    {Camlburg.cost =
                        (arg1.edi.Camlburg.cost
                        +
                        (Camlburg.matches "sub") arg2
                        +
                        arg3.const.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let edi1 = arg1.edi.Camlburg.action ()
                            and n2 = arg3.const.Camlburg.action ()
                            and w3 = arg4
                            in
                                (edi1 ,n2 ,w3))
                    })
                    ((update_any
                        {Camlburg.cost =
                            (arg1.any.Camlburg.cost + arg3.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let dst = arg1.any.Camlburg.action ()
                                and string = arg2
                                and src = arg3.any.Camlburg.action ()
                                and w = arg4
                                in
                                    
# 1050 "x86rec.mlb"
                                    ( "Llr(" ^ dst ^","^string^ "," ^ src ^ "," ^ string_of_int w ^ ")" )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_fppop
                            {Camlburg.cost =
                                (arg1.fpsp.Camlburg.cost
                                +
                                (Camlburg.matches "add") arg2
                                +
                                arg3.one3.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let fpsp = arg1.fpsp.Camlburg.action ()
                                    and one3 = arg3.one3.Camlburg.action ()
                                    and w = arg4
                                    in
                                        
# 905 "x86rec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_fppop2
                                {Camlburg.cost =
                                    (arg1.fpsp.Camlburg.cost
                                    +
                                    (Camlburg.matches "add") arg2
                                    +
                                    arg3.two3.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            fpsp =
                                            arg1.fpsp.Camlburg.action ()
                                        and
                                            two3 =
                                            arg3.two3.Camlburg.action ()
                                        and w = arg4
                                        in
                                            
# 906 "x86rec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_fppush
                                    {Camlburg.cost =
                                        (arg1.fpsp.Camlburg.cost
                                        +
                                        (Camlburg.matches "sub") arg2
                                        +
                                        arg3.one3.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                fpsp =
                                                arg1.fpsp.Camlburg.action ()
                                            and
                                                one3 =
                                                arg3.one3.Camlburg.action ()
                                            and w = arg4
                                            in
                                                
# 907 "x86rec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_inst
                                        (Camlburg.choice
                                            [{Camlburg.cost =
                                                (let w = arg4
                                                in
                                                    
# 754 "x86rec.mlb"
                                                    (guard (w=32))
                                                    
# 000 "/dev/stdout"

                                                +
                                                arg1.regl.Camlburg.cost
                                                +
                                                (Camlburg.matches "add") arg2
                                                +
                                                arg3.const.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        dst =
                                                        arg1.regl.Camlburg.action
                                                            ()
                                                    and
                                                        const =
                                                        arg3.const.Camlburg.action
                                                            ()
                                                    and w = arg4
                                                    in
                                                        
# 755 "x86rec.mlb"
                                                        ( s "leal %s(%s), %s" const dst dst )
                                                        
# 000 "/dev/stdout"
)
                                            }
                                            ;{Camlburg.cost =
                                                (arg1.regl.Camlburg.cost
                                                +
                                                (Camlburg.matches "shl") arg2
                                                +
                                                arg3.immed.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        dst =
                                                        arg1.regl.Camlburg.action
                                                            ()
                                                    and
                                                        immed =
                                                        arg3.immed.Camlburg.action
                                                            ()
                                                    and w = arg4
                                                    in
                                                        
# 818 "x86rec.mlb"
                                                        ( sprintf "shl%s %s, %s" (suffix w) immed dst )
                                                        
# 000 "/dev/stdout"
)
                                            }
                                            ;{Camlburg.cost =
                                                (arg1.regl.Camlburg.cost
                                                +
                                                (Camlburg.matches "shra")
                                                    arg2
                                                +
                                                arg3.immed.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        dst =
                                                        arg1.regl.Camlburg.action
                                                            ()
                                                    and
                                                        immed =
                                                        arg3.immed.Camlburg.action
                                                            ()
                                                    and w = arg4
                                                    in
                                                        
# 821 "x86rec.mlb"
                                                        ( sprintf "sar%s %s, %s" (suffix w) immed dst )
                                                        
# 000 "/dev/stdout"
)
                                            }
                                            ;{Camlburg.cost =
                                                (arg1.regl.Camlburg.cost
                                                +
                                                (Camlburg.matches "shrl")
                                                    arg2
                                                +
                                                arg3.immed.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        dst =
                                                        arg1.regl.Camlburg.action
                                                            ()
                                                    and
                                                        immed =
                                                        arg3.immed.Camlburg.action
                                                            ()
                                                    and w = arg4
                                                    in
                                                        
# 824 "x86rec.mlb"
                                                        ( sprintf "shr%s %s, %s" (suffix w) immed dst )
                                                        
# 000 "/dev/stdout"
)
                                            }
                                            ;{Camlburg.cost =
                                                (arg1.eflags.Camlburg.cost
                                                +
                                                (Camlburg.matches
                                                    "x86_setcarry")
                                                    arg2
                                                +
                                                arg3.eaddrbit0.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        eflags =
                                                        arg1.eflags.Camlburg.action
                                                            ()
                                                    and
                                                        eaddrbit0 =
                                                        arg3.eaddrbit0.Camlburg.action
                                                            ()
                                                    and w = arg4
                                                    in
                                                        
# 860 "x86rec.mlb"
                                                        ( sprintf "bt%s $0,%s" (suffix 32) eaddrbit0 )
                                                        
# 000 "/dev/stdout"
)
                                            }
                                            ;{Camlburg.cost =
                                                (arg1.eflags.Camlburg.cost
                                                +
                                                (Camlburg.matches
                                                    "x86_setcarry")
                                                    arg2
                                                +
                                                arg3.zero.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        eflags =
                                                        arg1.eflags.Camlburg.action
                                                            ()
                                                    and
                                                        zero =
                                                        arg3.zero.Camlburg.action
                                                            ()
                                                    and w = arg4
                                                    in
                                                        
# 862 "x86rec.mlb"
                                                        ( "clc" )
                                                        
# 000 "/dev/stdout"
)
                                            }]))
                                        ((update_pop
                                            {Camlburg.cost =
                                                (arg1.espl.Camlburg.cost
                                                +
                                                (Camlburg.matches "add") arg2
                                                +
                                                arg3.four.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        espl =
                                                        arg1.espl.Camlburg.action
                                                            ()
                                                    and
                                                        four =
                                                        arg3.four.Camlburg.action
                                                            ()
                                                    and w = arg4
                                                    in
                                                        
# 876 "x86rec.mlb"
                                                        ( () )
                                                        
# 000 "/dev/stdout"
)
                                            })
                                            ((update_push
                                                (Camlburg.choice
                                                    [{Camlburg.cost =
                                                        (arg1.espl.Camlburg.cost
                                                        +
                                                        (Camlburg.matches
                                                            "sub")
                                                            arg2
                                                        +
                                                        arg3.four.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                espl =
                                                                arg1.espl.Camlburg.action
                                                                    ()
                                                            and
                                                                four =
                                                                arg3.four.Camlburg.action
                                                                    ()
                                                            and w = arg4
                                                            in
                                                                
# 877 "x86rec.mlb"
                                                                ( () )
                                                                
# 000 "/dev/stdout"
)
                                                    }
                                                    ;{Camlburg.cost =
                                                        (arg1.espl.Camlburg.cost
                                                        +
                                                        (Camlburg.matches
                                                            "add")
                                                            arg2
                                                        +
                                                        arg3.minusfour.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                espl =
                                                                arg1.espl.Camlburg.action
                                                                    ()
                                                            and
                                                                minusfour =
                                                                arg3.minusfour.Camlburg.action
                                                                    ()
                                                            and w = arg4
                                                            in
                                                                
# 878 "x86rec.mlb"
                                                                ( () )
                                                                
# 000 "/dev/stdout"
)
                                                    }]))
                                                inf)))))))))
and conLink =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let symbol = arg1
                    and w = arg2
                    in
                        
# 985 "x86rec.mlb"
                        ( "Link(" ^ symbol#mangled_text ^ "," ^ string_of_int w ^ ")" )
                        
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
                            
# 628 "x86rec.mlb"
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
                        
# 986 "x86rec.mlb"
                        ( "Late(" ^ string ^ "," ^ string_of_int w ^")" )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost = (0)
                ;Camlburg.action =
                    (fun () ->
                        let string = arg1
                        and w = arg2
                        in
                            
# 685 "x86rec.mlb"
                            ( string )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conKill =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 1040 "x86rec.mlb"
                        ( "Kill(" ^ any ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conJcc =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let string = arg1
                    and any = arg2.any.Camlburg.action ()
                    in
                        
# 1065 "x86rec.mlb"
                        ( "Jcc(" ^ string ^ "," ^ any ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                {Camlburg.cost = (arg2.lconst.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let cc = arg1
                        and lconst = arg2.lconst.Camlburg.action ()
                        in
                            
# 867 "x86rec.mlb"
                            ( "j" ^ cc ^ " " ^ lconst )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conI2f =
    fun arg1 arg2 arg3 ->
        (update__I2f56
            {Camlburg.cost = (arg3.mem.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let s = arg1
                    and d = arg2
                    and mem = arg3.mem.Camlburg.action ()
                    in
                        (s ,d ,mem))
            })
            ((update_any
                {Camlburg.cost = (arg3.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let n = arg1
                        and w = arg2
                        and any = arg3.any.Camlburg.action ()
                        in
                            
# 1024 "x86rec.mlb"
                            ( sprintf "I2f(%d, %d, %s)" n w any )
                            
# 000 "/dev/stdout"
)
                })
                ((update_i2f_mem
                    {Camlburg.cost =
                        (let s = arg1
                        and d = arg2
                        in
                            
# 944 "x86rec.mlb"
                            ( guard (s = 32 || s = 16) )
                            
# 000 "/dev/stdout"

                        +
                        arg3.mem.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let s = arg1
                            and d = arg2
                            and mem = arg3.mem.Camlburg.action ()
                            in
                                
# 944 "x86rec.mlb"
                                ( s, mem )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conGuarded =
    fun arg1 arg2 ->
        (update__Guarded23
            {Camlburg.cost =
                (arg1._Fetch24.Camlburg.cost + arg2._Goto25.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch24.Camlburg.action ()
                    and _v2 = arg2._Goto25.Camlburg.action ()
                    in
                        let (eip, w10) = _v2
                        in
                            let (ecx2, w9) = _v1 in (ecx2 ,w9 ,eip ,w10))
            })
            ((update_any
                {Camlburg.cost =
                    (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let guard = arg1.any.Camlburg.action ()
                        and any = arg2.any.Camlburg.action ()
                        in
                            
# 1045 "x86rec.mlb"
                            ( "Guarded(" ^ guard ^ "," ^ any ^ ")" )
                            
# 000 "/dev/stdout"
)
                })
                ((update_inst
                    {Camlburg.cost =
                        (arg1._Cmp3.Camlburg.cost
                        +
                        arg2._Store4.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Cmp3.Camlburg.action ()
                            and _v2 = arg2._Store4.Camlburg.action ()
                            in
                                let (espl, s2, w) = _v2
                                in
                                    let (string, esp, slotaddr) = _v1
                                    in
                                        
# 689 "x86rec.mlb"
                                        ( "adjust %esp" )
                                        
# 000 "/dev/stdout"
)
                    })
                    inf))
and conGoto =
    fun arg1 ->
        (update__Goto25
            {Camlburg.cost = (arg1._Fetch26.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch26.Camlburg.action ()
                    in
                        let (eip, w10) = _v1 in (eip ,w10))
            })
            ((update__Goto36
                {Camlburg.cost = (arg1.target.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let target = arg1.target.Camlburg.action ()
                        in
                            target)
                })
                ((update__Goto40
                    {Camlburg.cost = (arg1._Fetch41.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Fetch41.Camlburg.action ()
                            in
                                let (stacktop, w) = _v1 in (stacktop ,w))
                    })
                    ((update__Goto42
                        {Camlburg.cost = (arg1.eaddr.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let eaddr = arg1.eaddr.Camlburg.action ()
                                in
                                    eaddr)
                        })
                        ((update__Goto45
                            {Camlburg.cost = (arg1.lconst.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let
                                        lconst =
                                        arg1.lconst.Camlburg.action ()
                                    in
                                        lconst)
                            })
                            ((update__Goto46
                                {Camlburg.cost =
                                    (arg1.inthandler.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            inthandler =
                                            arg1.inthandler.Camlburg.action
                                                ()
                                        in
                                            inthandler)
                                })
                                ((update_any
                                    {Camlburg.cost = (arg1.any.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                any =
                                                arg1.any.Camlburg.action ()
                                            in
                                                
# 1047 "x86rec.mlb"
                                                ( "Goto(" ^ any ^ ")" )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_inst
                                        {Camlburg.cost =
                                            (arg1.target.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    target =
                                                    arg1.target.Camlburg.action
                                                        ()
                                                in
                                                    
# 866 "x86rec.mlb"
                                                    ( "jmp " ^ target )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        inf)))))))
and conFsub =
    fun arg1 arg2 ->
        (update__Fsub62
            {Camlburg.cost =
                (arg1.fpstacktop.Camlburg.cost + arg2.fpstack1.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let fpstacktop = arg1.fpstacktop.Camlburg.action ()
                    and fpstack1 = arg2.fpstack1.Camlburg.action ()
                    in
                        (fpstacktop ,fpstack1))
            })
            ((update_any
                {Camlburg.cost =
                    (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.any.Camlburg.action ()
                        and y = arg2.any.Camlburg.action ()
                        in
                            
# 1001 "x86rec.mlb"
                            ( sprintf "Fsub(%s, %s)" x y )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conFrnd =
    fun arg1 ->
        (update__Frnd69
            {Camlburg.cost = (arg1.fpstacktop.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let fpstacktop = arg1.fpstacktop.Camlburg.action ()
                    in
                        fpstacktop)
            })
            inf
and conFpustatusl =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 1012 "x86rec.mlb"
                    ( "Fpustatus()" )
                    
# 000 "/dev/stdout"
)
            })
            ((update_fpustatusl
                {Camlburg.cost = (0)
                ;Camlburg.action =
                    (fun () ->
                        
# 950 "x86rec.mlb"
                        ( () )
                        
# 000 "/dev/stdout"
)
                })
                inf)
and conFpuctll =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 1013 "x86rec.mlb"
                    ( "Fpuctl()" )
                    
# 000 "/dev/stdout"
)
            })
            ((update_fpuctll
                {Camlburg.cost = (0)
                ;Camlburg.action =
                    (fun () ->
                        
# 952 "x86rec.mlb"
                        ( () )
                        
# 000 "/dev/stdout"
)
                })
                inf)
and conFpreg =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 1036 "x86rec.mlb"
                        ( sprintf "Fpreg(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            ((update_fpstack1l
                {Camlburg.cost = (arg1._Add49.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Add49.Camlburg.action ()
                        in
                            let (fpsp, w, one3) = _v1
                            in
                                
# 911 "x86rec.mlb"
                                ( () )
                                
# 000 "/dev/stdout"
)
                })
                ((update_fpstacknextl
                    {Camlburg.cost = (arg1._Sub50.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Sub50.Camlburg.action ()
                            in
                                let (fpsp, w, one3) = _v1
                                in
                                    
# 912 "x86rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                    })
                    ((update_fpstacktopl
                        (Camlburg.choice
                            [{Camlburg.cost = (arg1._Fetch47.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let
                                        _v1 =
                                        arg1._Fetch47.Camlburg.action ()
                                    in
                                        let (fpsp, w) = _v1
                                        in
                                            
# 909 "x86rec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost = (arg1._Add48.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let _v1 = arg1._Add48.Camlburg.action ()
                                    in
                                        let (fpsp, w, zero) = _v1
                                        in
                                            
# 910 "x86rec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                            }]))
                        inf)))
and conFpccl =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 1011 "x86rec.mlb"
                    ( "Fpcc()" )
                    
# 000 "/dev/stdout"
)
            })
            ((update_fpccl
                {Camlburg.cost = (0)
                ;Camlburg.action =
                    (fun () ->
                        
# 949 "x86rec.mlb"
                        ( () )
                        
# 000 "/dev/stdout"
)
                })
                inf)
and conFneg =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 1008 "x86rec.mlb"
                        ( sprintf "Fneg(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conFmul =
    fun arg1 arg2 ->
        (update__Fmul68
            {Camlburg.cost =
                (arg1.fpstacktop.Camlburg.cost + arg2.fpstack1.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let fpstacktop = arg1.fpstacktop.Camlburg.action ()
                    and fpstack1 = arg2.fpstack1.Camlburg.action ()
                    in
                        (fpstacktop ,fpstack1))
            })
            ((update_any
                {Camlburg.cost =
                    (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.any.Camlburg.action ()
                        and y = arg2.any.Camlburg.action ()
                        in
                            
# 1002 "x86rec.mlb"
                            ( sprintf "Fmul(%s, %s)" x y )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conFlags2ah =
    fun arg1 ->
        (update__Flags2ah74
            {Camlburg.cost = (arg1._Fetch75.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch75.Camlburg.action ()
                    in
                        let (eflags, fw) = _v1 in (eflags ,fw))
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        in
                            
# 1018 "x86rec.mlb"
                            ( sprintf "Flags2ah(%s)" any )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conFetch =
    fun arg1 arg2 ->
        (update__Fetch1
            {Camlburg.cost = (arg1.regl8.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let regl8 = arg1.regl8.Camlburg.action ()
                    and w = arg2
                    in
                        (regl8 ,w))
            })
            ((update__Fetch13
                {Camlburg.cost = (arg1._Reg14.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Reg14.Camlburg.action ()
                        and fw = arg2
                        in
                            let n = _v1 in (n ,fw))
                })
                ((update__Fetch2
                    {Camlburg.cost = (arg1.regl_ecx.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let regl_ecx = arg1.regl_ecx.Camlburg.action ()
                            and w = arg2
                            in
                                (regl_ecx ,w))
                    })
                    ((update__Fetch24
                        {Camlburg.cost = (arg1.ecx.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let ecx2 = arg1.ecx.Camlburg.action ()
                                and w9 = arg2
                                in
                                    (ecx2 ,w9))
                        })
                        ((update__Fetch26
                            {Camlburg.cost = (arg1.eip.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let eip = arg1.eip.Camlburg.action ()
                                    and w10 = arg2
                                    in
                                        (eip ,w10))
                            })
                            ((update__Fetch28
                                {Camlburg.cost = (arg1._Mem29.Camlburg.cost)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            _v1 =
                                            arg1._Mem29.Camlburg.action ()
                                        and w7 = arg2
                                        in
                                            let (edi2, w5, c6) = _v1
                                            in
                                                (edi2 ,w5 ,c6 ,w7))
                                })
                                ((update__Fetch30
                                    {Camlburg.cost = (arg1.edi.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                edi2 =
                                                arg1.edi.Camlburg.action ()
                                            and w5 = arg2
                                            in
                                                (edi2 ,w5))
                                    })
                                    ((update__Fetch31
                                        {Camlburg.cost =
                                            (arg1.esi.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    esi2 =
                                                    arg1.esi.Camlburg.action
                                                        ()
                                                and w4 = arg2
                                                in
                                                    (esi2 ,w4))
                                        })
                                        ((update__Fetch32
                                            {Camlburg.cost =
                                                (arg1.eax.Camlburg.cost)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        eax =
                                                        arg1.eax.Camlburg.action
                                                            ()
                                                    and w = arg2
                                                    in
                                                        (eax ,w))
                                            })
                                            ((update__Fetch41
                                                {Camlburg.cost =
                                                    (arg1.stacktop.Camlburg.cost)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        let
                                                            stacktop =
                                                            arg1.stacktop.Camlburg.action
                                                                ()
                                                        and w = arg2
                                                        in
                                                            (stacktop ,w))
                                                })
                                                ((update__Fetch47
                                                    {Camlburg.cost =
                                                        (arg1.fpsp.Camlburg.cost)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                fpsp =
                                                                arg1.fpsp.Camlburg.action
                                                                    ()
                                                            and w = arg2
                                                            in
                                                                (fpsp ,w))
                                                    })
                                                    ((update__Fetch7
                                                        {Camlburg.cost =
                                                            (arg1.regl.Camlburg.cost)
                                                        ;Camlburg.action =
                                                            (fun () ->
                                                                let
                                                                    src =
                                                                    arg1.regl.Camlburg.action
                                                                        ()
                                                                and nw = arg2
                                                                in
                                                                    (src
                                                                    ,nw))
                                                        })
                                                        ((update__Fetch75
                                                            {Camlburg.cost =
                                                                (arg1.eflags.Camlburg.cost)
                                                            ;Camlburg.action =
                                                                (fun () ->
                                                                    let
                                                                        eflags =
                                                                        arg1.eflags.Camlburg.action
                                                                            ()
                                                                    and
                                                                        fw =
                                                                        arg2
                                                                    in
                                                                        (eflags
                                                                        ,fw))
                                                            })
                                                            ((update__Fetch9
                                                                {Camlburg.cost =
                                                                    (arg1.eaddrl.Camlburg.cost)
                                                                ;Camlburg.action =
                                                                    (fun
                                                                    ()
                                                                    ->
                                                                        let
                                                                            src =
                                                                            arg1.eaddrl.Camlburg.action
                                                                                ()
                                                                        and
                                                                            nw =
                                                                            arg2
                                                                        in
                                                                            (src
                                                                            ,nw))
                                                                })
                                                                ((update_ahval
                                                                    {Camlburg.cost =
                                                                        (let
                                                                            w =
                                                                            arg2
                                                                        in
                                                                            
# 972 "x86rec.mlb"
                                                                            ( guard (w=8) )
                                                                            
# 000 "/dev/stdout"

                                                                        +
                                                                        arg1.ah.Camlburg.cost)
                                                                    ;Camlburg.action =
                                                                        (fun
                                                                        ()
                                                                        ->
                                                                            let
                                                                                ah =
                                                                                arg1.ah.Camlburg.action
                                                                                    ()
                                                                            and
                                                                                w =
                                                                                arg2
                                                                            in
                                                                                
# 972 "x86rec.mlb"
                                                                                ( ah )
                                                                                
# 000 "/dev/stdout"
)
                                                                    })
                                                                    ((update_any
                                                                        {Camlburg.cost =
                                                                            (arg1.any.Camlburg.cost)
                                                                        ;Camlburg.action =
                                                                            (fun
                                                                            ()
                                                                            ->
                                                                                let
                                                                                    any =
                                                                                    arg1.any.Camlburg.action
                                                                                        ()
                                                                                and
                                                                                    w =
                                                                                    arg2
                                                                                in
                                                                                    
# 990 "x86rec.mlb"
                                                                                    ( "Fetch(" ^ any ^ "," ^ string_of_int w ^ ")" )
                                                                                    
# 000 "/dev/stdout"
)
                                                                        })
                                                                        ((update_eaddr
                                                                            {Camlburg.cost =
                                                                                (arg1.eaddrl.Camlburg.cost)
                                                                            ;Camlburg.action =
                                                                                (fun
                                                                                ()
                                                                                ->
                                                                                    let
                                                                                        eaddrl =
                                                                                        arg1.eaddrl.Camlburg.action
                                                                                            ()
                                                                                    and
                                                                                        w =
                                                                                        arg2
                                                                                    in
                                                                                        
# 670 "x86rec.mlb"
                                                                                        ( eaddrl   )
                                                                                        
# 000 "/dev/stdout"
)
                                                                            })
                                                                            ((update_esp
                                                                                {Camlburg.cost =
                                                                                    (arg1.espl.Camlburg.cost)
                                                                                ;Camlburg.action =
                                                                                    (fun
                                                                                    ()
                                                                                    ->
                                                                                        let
                                                                                            espl =
                                                                                            arg1.espl.Camlburg.action
                                                                                                ()
                                                                                        and
                                                                                            w =
                                                                                            arg2
                                                                                        in
                                                                                            
# 654 "x86rec.mlb"
                                                                                            ( espl   )
                                                                                            
# 000 "/dev/stdout"
)
                                                                                })
                                                                                ((update_fpstack1
                                                                                    {Camlburg.cost =
                                                                                        (let
                                                                                            w =
                                                                                            arg2
                                                                                        in
                                                                                            
# 915 "x86rec.mlb"
                                                                                            ( guard (w=80) )
                                                                                            
# 000 "/dev/stdout"

                                                                                        +
                                                                                        arg1.fpstack1l.Camlburg.cost)
                                                                                    ;Camlburg.action =
                                                                                        (fun
                                                                                        ()
                                                                                        ->
                                                                                            let
                                                                                                fpstack1l =
                                                                                                arg1.fpstack1l.Camlburg.action
                                                                                                    ()
                                                                                            and
                                                                                                w =
                                                                                                arg2
                                                                                            in
                                                                                                
# 915 "x86rec.mlb"
                                                                                                ( fpstack1l )
                                                                                                
# 000 "/dev/stdout"
)
                                                                                    })
                                                                                    ((update_fpstacknext
                                                                                        {Camlburg.cost =
                                                                                            (let
                                                                                                w =
                                                                                                arg2
                                                                                            in
                                                                                                
# 916 "x86rec.mlb"
                                                                                                ( guard (w=80) )
                                                                                                
# 000 "/dev/stdout"

                                                                                            +
                                                                                            arg1.fpstacknextl.Camlburg.cost)
                                                                                        ;Camlburg.action =
                                                                                            (fun
                                                                                            ()
                                                                                            ->
                                                                                                let
                                                                                                    fpstacknextl =
                                                                                                    arg1.fpstacknextl.Camlburg.action
                                                                                                        ()
                                                                                                and
                                                                                                    w =
                                                                                                    arg2
                                                                                                in
                                                                                                    
# 916 "x86rec.mlb"
                                                                                                    ( fpstacknextl )
                                                                                                    
# 000 "/dev/stdout"
)
                                                                                        })
                                                                                        ((update_fpstacktop
                                                                                            {Camlburg.cost =
                                                                                                (let
                                                                                                    w =
                                                                                                    arg2
                                                                                                in
                                                                                                    
# 914 "x86rec.mlb"
                                                                                                    ( guard (w=80) )
                                                                                                    
# 000 "/dev/stdout"

                                                                                                +
                                                                                                arg1.fpstacktopl.Camlburg.cost)
                                                                                            ;Camlburg.action =
                                                                                                (fun
                                                                                                ()
                                                                                                ->
                                                                                                    let
                                                                                                        fpstacktopl =
                                                                                                        arg1.fpstacktopl.Camlburg.action
                                                                                                            ()
                                                                                                    and
                                                                                                        w =
                                                                                                        arg2
                                                                                                    in
                                                                                                        
# 914 "x86rec.mlb"
                                                                                                        ( fpstacktopl )
                                                                                                        
# 000 "/dev/stdout"
)
                                                                                            })
                                                                                            ((update_fpuctl
                                                                                                {Camlburg.cost =
                                                                                                    (arg1.fpuctll.Camlburg.cost)
                                                                                                ;Camlburg.action =
                                                                                                    (fun
                                                                                                    ()
                                                                                                    ->
                                                                                                        let
                                                                                                            fpuctll =
                                                                                                            arg1.fpuctll.Camlburg.action
                                                                                                                ()
                                                                                                        and
                                                                                                            w =
                                                                                                            arg2
                                                                                                        in
                                                                                                            
# 953 "x86rec.mlb"
                                                                                                            ( () )
                                                                                                            
# 000 "/dev/stdout"
)
                                                                                                })
                                                                                                ((update_fpustatus
                                                                                                    {Camlburg.cost =
                                                                                                        (arg1.fpustatusl.Camlburg.cost)
                                                                                                    ;Camlburg.action =
                                                                                                        (fun
                                                                                                        ()
                                                                                                        ->
                                                                                                            let
                                                                                                                fpustatusl =
                                                                                                                arg1.fpustatusl.Camlburg.action
                                                                                                                    ()
                                                                                                            and
                                                                                                                w =
                                                                                                                arg2
                                                                                                            in
                                                                                                                
# 951 "x86rec.mlb"
                                                                                                                ( () )
                                                                                                                
# 000 "/dev/stdout"
)
                                                                                                    })
                                                                                                    ((update_mem
                                                                                                        {Camlburg.cost =
                                                                                                            (arg1.meml.Camlburg.cost)
                                                                                                        ;Camlburg.action =
                                                                                                            (fun
                                                                                                            ()
                                                                                                            ->
                                                                                                                let
                                                                                                                    meml =
                                                                                                                    arg1.meml.Camlburg.action
                                                                                                                        ()
                                                                                                                and
                                                                                                                    w =
                                                                                                                    arg2
                                                                                                                in
                                                                                                                    
# 669 "x86rec.mlb"
                                                                                                                    ( meml     )
                                                                                                                    
# 000 "/dev/stdout"
)
                                                                                                        })
                                                                                                        ((update_reg
                                                                                                            {Camlburg.cost =
                                                                                                                (arg1.regl.Camlburg.cost)
                                                                                                            ;Camlburg.action =
                                                                                                                (fun
                                                                                                                ()
                                                                                                                ->
                                                                                                                    let
                                                                                                                        regl =
                                                                                                                        arg1.regl.Camlburg.action
                                                                                                                            ()
                                                                                                                    and
                                                                                                                        w =
                                                                                                                        arg2
                                                                                                                    in
                                                                                                                        
# 660 "x86rec.mlb"
                                                                                                                        ( regl     )
                                                                                                                        
# 000 "/dev/stdout"
)
                                                                                                            })
                                                                                                            ((update_reg8H
                                                                                                                {Camlburg.cost =
                                                                                                                    (arg1.regl8H.Camlburg.cost)
                                                                                                                ;Camlburg.action =
                                                                                                                    (fun
                                                                                                                    ()
                                                                                                                    ->
                                                                                                                        let
                                                                                                                            regl8H =
                                                                                                                            arg1.regl8H.Camlburg.action
                                                                                                                                ()
                                                                                                                        and
                                                                                                                            w =
                                                                                                                            arg2
                                                                                                                        in
                                                                                                                            
# 664 "x86rec.mlb"
                                                                                                                            ( regl8H )
                                                                                                                            
# 000 "/dev/stdout"
)
                                                                                                                })
                                                                                                                ((update_reg_cl
                                                                                                                    {Camlburg.cost =
                                                                                                                        (arg1.reg_cll.Camlburg.cost)
                                                                                                                    ;Camlburg.action =
                                                                                                                        (fun
                                                                                                                        ()
                                                                                                                        ->
                                                                                                                            let
                                                                                                                                reg_cll =
                                                                                                                                arg1.reg_cll.Camlburg.action
                                                                                                                                    ()
                                                                                                                            and
                                                                                                                                w =
                                                                                                                                arg2
                                                                                                                            in
                                                                                                                                
# 667 "x86rec.mlb"
                                                                                                                                ( () )
                                                                                                                                
# 000 "/dev/stdout"
)
                                                                                                                    })
                                                                                                                    ((update_regabcd
                                                                                                                        {Camlburg.cost =
                                                                                                                            (arg1.regabcdl.Camlburg.cost)
                                                                                                                        ;Camlburg.action =
                                                                                                                            (fun
                                                                                                                            ()
                                                                                                                            ->
                                                                                                                                let
                                                                                                                                    regabcdl =
                                                                                                                                    arg1.regabcdl.Camlburg.action
                                                                                                                                        ()
                                                                                                                                and
                                                                                                                                    w =
                                                                                                                                    arg2
                                                                                                                                in
                                                                                                                                    
# 661 "x86rec.mlb"
                                                                                                                                    ( regabcdl )
                                                                                                                                    
# 000 "/dev/stdout"
)
                                                                                                                        })
                                                                                                                        ((update_regpair
                                                                                                                            {Camlburg.cost =
                                                                                                                                (arg1.regpairl.Camlburg.cost)
                                                                                                                            ;Camlburg.action =
                                                                                                                                (fun
                                                                                                                                ()
                                                                                                                                ->
                                                                                                                                    let
                                                                                                                                        regpairl =
                                                                                                                                        arg1.regpairl.Camlburg.action
                                                                                                                                            ()
                                                                                                                                    and
                                                                                                                                        w =
                                                                                                                                        arg2
                                                                                                                                    in
                                                                                                                                        
# 668 "x86rec.mlb"
                                                                                                                                        ( regpairl )
                                                                                                                                        
# 000 "/dev/stdout"
)
                                                                                                                            })
                                                                                                                            ((update_target
                                                                                                                                {Camlburg.cost =
                                                                                                                                    (arg1.eaddrl.Camlburg.cost)
                                                                                                                                ;Camlburg.action =
                                                                                                                                    (fun
                                                                                                                                    ()
                                                                                                                                    ->
                                                                                                                                        let
                                                                                                                                            eaddrl =
                                                                                                                                            arg1.eaddrl.Camlburg.action
                                                                                                                                                ()
                                                                                                                                        and
                                                                                                                                            w =
                                                                                                                                            arg2
                                                                                                                                        in
                                                                                                                                            
# 864 "x86rec.mlb"
                                                                                                                                            ( "*" ^ eaddrl )
                                                                                                                                            
# 000 "/dev/stdout"
)
                                                                                                                                })
                                                                                                                                ((update_vfp
                                                                                                                                    {Camlburg.cost =
                                                                                                                                        (arg1.vfpl.Camlburg.cost)
                                                                                                                                    ;Camlburg.action =
                                                                                                                                        (fun
                                                                                                                                        ()
                                                                                                                                        ->
                                                                                                                                            let
                                                                                                                                                vfpl =
                                                                                                                                                arg1.vfpl.Camlburg.action
                                                                                                                                                    ()
                                                                                                                                            and
                                                                                                                                                w =
                                                                                                                                                arg2
                                                                                                                                            in
                                                                                                                                                
# 655 "x86rec.mlb"
                                                                                                                                                ( vfpl   )
                                                                                                                                                
# 000 "/dev/stdout"
)
                                                                                                                                    })
                                                                                                                                    inf))))))))))))))))))))))))))))))
and conFdiv =
    fun arg1 arg2 ->
        (update__Fdiv64
            {Camlburg.cost =
                (arg1.fpstacktop.Camlburg.cost + arg2.fpstack1.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let fpstacktop = arg1.fpstacktop.Camlburg.action ()
                    and fpstack1 = arg2.fpstack1.Camlburg.action ()
                    in
                        (fpstacktop ,fpstack1))
            })
            ((update__Fdiv66
                {Camlburg.cost =
                    (arg1.fpstack1.Camlburg.cost
                    +
                    arg2.fpstacktop.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let fpstack1 = arg1.fpstack1.Camlburg.action ()
                        and fpstacktop = arg2.fpstacktop.Camlburg.action ()
                        in
                            (fpstack1 ,fpstacktop))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.any.Camlburg.action ()
                            and y = arg2.any.Camlburg.action ()
                            in
                                
# 1003 "x86rec.mlb"
                                ( sprintf "Fdiv(%s, %s)" x y )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conFcmp =
    fun arg1 arg2 ->
        (update__Fcmp70
            {Camlburg.cost =
                (arg1.fpstacktop.Camlburg.cost + arg2.fpstack1.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let fpstacktop = arg1.fpstacktop.Camlburg.action ()
                    and fpstack1 = arg2.fpstack1.Camlburg.action ()
                    in
                        (fpstacktop ,fpstack1))
            })
            inf
and conFalse =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 984 "x86rec.mlb"
                    ( "False" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conFadd =
    fun arg1 arg2 ->
        (update__Fadd60
            {Camlburg.cost =
                (arg1.fpstacktop.Camlburg.cost + arg2.fpstack1.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let fpstacktop = arg1.fpstacktop.Camlburg.action ()
                    and fpstack1 = arg2.fpstack1.Camlburg.action ()
                    in
                        (fpstacktop ,fpstack1))
            })
            ((update_any
                {Camlburg.cost =
                    (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg1.any.Camlburg.action ()
                        and y = arg2.any.Camlburg.action ()
                        in
                            
# 1000 "x86rec.mlb"
                            ( sprintf "Fadd(%s, %s)" x y )
                            
# 000 "/dev/stdout"
)
                })
                ((update_fiadd
                    {Camlburg.cost =
                        (arg1.fpstacktop.Camlburg.cost
                        +
                        arg2.i2f_mem.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let
                                fpstacktop =
                                arg1.fpstacktop.Camlburg.action ()
                            and i2f_mem = arg2.i2f_mem.Camlburg.action ()
                            in
                                
# 945 "x86rec.mlb"
                                ( i2f_mem )
                                
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
                        
# 1009 "x86rec.mlb"
                        ( sprintf "Fabs(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conF2i =
    fun arg1 arg2 arg3 ->
        (update__F2i58
            {Camlburg.cost = (arg3.fpstacktop.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let s = arg1
                    and d = arg2
                    and fpstacktop = arg3.fpstacktop.Camlburg.action ()
                    in
                        (s ,d ,fpstacktop))
            })
            ((update_any
                {Camlburg.cost = (arg3.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let n = arg1
                        and w = arg2
                        and any = arg3.any.Camlburg.action ()
                        in
                            
# 1023 "x86rec.mlb"
                            ( sprintf "F2i(%d, %d, %s)" n w any )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conF2f =
    fun arg1 arg2 arg3 ->
        (update__F2f52
            {Camlburg.cost = (arg3.mem.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let s = arg1
                    and d = arg2
                    and mem = arg3.mem.Camlburg.action ()
                    in
                        (s ,d ,mem))
            })
            ((update__F2f54
                {Camlburg.cost = (arg3.fpstacktop.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let s = arg1
                        and d = arg2
                        and fpstacktop = arg3.fpstacktop.Camlburg.action ()
                        in
                            (s ,d ,fpstacktop))
                })
                ((update_any
                    {Camlburg.cost = (arg3.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let n = arg1
                            and w = arg2
                            and any = arg3.any.Camlburg.action ()
                            in
                                
# 1022 "x86rec.mlb"
                                ( sprintf "F2f(%d, %d, %s)" n w any )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
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
                        
# 987 "x86rec.mlb"
                        ( "Diff(" ^ c1 ^ "," ^ c2 ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            ((update_pic
                {Camlburg.cost =
                    (arg1.lconst.Camlburg.cost + arg2.lconst.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let c1 = arg1.lconst.Camlburg.action ()
                        and c2 = arg2.lconst.Camlburg.action ()
                        in
                            
# 629 "x86rec.mlb"
                            ( c1 ^ " - " ^ c2 )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conCom =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.any.Camlburg.action ()
                    in
                        
# 995 "x86rec.mlb"
                        ( "Com(" ^ x ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conCmp =
    fun arg1 arg2 arg3 ->
        (update__Cmp3
            {Camlburg.cost =
                (arg2.esp.Camlburg.cost + arg3.slotaddr.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let string = arg1
                    and esp = arg2.esp.Camlburg.action ()
                    and slotaddr = arg3.slotaddr.Camlburg.action ()
                    in
                        (string ,esp ,slotaddr))
            })
            inf
and conBits =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let bits = arg1
                    in
                        
# 988 "x86rec.mlb"
                        ( sprintf "Bits(%s)" (Bits.to_string bits) )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const
                {Camlburg.cost =
                    (let b = arg1
                    in
                        
# 630 "x86rec.mlb"
                        ( guard (Bits.width b = 32) )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let b = arg1
                        in
                            
# 630 "x86rec.mlb"
                            ( native b )
                            
# 000 "/dev/stdout"
)
                })
                ((update_const8
                    {Camlburg.cost =
                        (let b = arg1
                        in
                            
# 631 "x86rec.mlb"
                            ( guard (Bits.width b = 8) )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let b = arg1
                            in
                                
# 631 "x86rec.mlb"
                                ( native' 8 b )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_eight
                        {Camlburg.cost =
                            (let b = arg1
                            in
                                
# 901 "x86rec.mlb"
                                ( guard (Bits.width b > 3 &&
                                Bits.Ops.eq (Bits.U.of_int 8 (Bits.width b)) b) )
                                
# 000 "/dev/stdout"
)
                        ;Camlburg.action =
                            (fun () ->
                                let b = arg1
                                in
                                    
# 903 "x86rec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_four
                            {Camlburg.cost =
                                (let b = arg1
                                in
                                    
# 634 "x86rec.mlb"
                                    ( guard (Bits.width b > 3 && Bits.Ops.eq (Bits.U.of_int 4 (Bits.width b)) b) )
                                    
# 000 "/dev/stdout"
)
                            ;Camlburg.action =
                                (fun () ->
                                    let b = arg1
                                    in
                                        
# 634 "x86rec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_minusfour
                                {Camlburg.cost =
                                    (let b = arg1
                                    in
                                        
# 636 "x86rec.mlb"
                                        ( guard (Bits.width b > 3 && Bits.Ops.eq (Bits.S.of_int (-4) (Bits.width b)) b) )
                                        
# 000 "/dev/stdout"
)
                                ;Camlburg.action =
                                    (fun () ->
                                        let b = arg1
                                        in
                                            
# 636 "x86rec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_one3
                                    {Camlburg.cost =
                                        (let b = arg1
                                        in
                                            
# 894 "x86rec.mlb"
                                            ( guard (Bits.width b = 3 && Bits.Ops.eq (Bits.U.of_int 1 3) b) )
                                            
# 000 "/dev/stdout"
)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let b = arg1
                                            in
                                                
# 895 "x86rec.mlb"
                                                ( () )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_shamt
                                        {Camlburg.cost =
                                            (let b = arg1
                                            in
                                                
# 632 "x86rec.mlb"
                                                ( guard (is_shamt b) )
                                                
# 000 "/dev/stdout"
)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let b = arg1
                                                in
                                                    
# 632 "x86rec.mlb"
                                                    ( Bits.U.to_int b )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        ((update_two3
                                            {Camlburg.cost =
                                                (let b = arg1
                                                in
                                                    
# 897 "x86rec.mlb"
                                                    ( guard (Bits.width b = 3 && Bits.Ops.eq (Bits.U.of_int 2 3) b) )
                                                    
# 000 "/dev/stdout"
)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let b = arg1
                                                    in
                                                        
# 898 "x86rec.mlb"
                                                        ( () )
                                                        
# 000 "/dev/stdout"
)
                                            })
                                            ((update_zero
                                                {Camlburg.cost =
                                                    (let b = arg1
                                                    in
                                                        
# 899 "x86rec.mlb"
                                                        ( guard (Bits.Ops.eq (Bits.U.of_int 0 (Bits.width b)) b) )
                                                        
# 000 "/dev/stdout"
)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        let b = arg1
                                                        in
                                                            
# 900 "x86rec.mlb"
                                                            ( () )
                                                            
# 000 "/dev/stdout"
)
                                                })
                                                inf)))))))))
and conBitInsert =
    fun arg1 arg2 arg3 ->
        (update__BitInsert72
            {Camlburg.cost =
                (arg1.zero.Camlburg.cost + arg2._Fetch32.Camlburg.cost
                +
                arg3.fpustatus.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let zero = arg1.zero.Camlburg.action ()
                    and _v1 = arg2._Fetch32.Camlburg.action ()
                    and fpustatus = arg3.fpustatus.Camlburg.action ()
                    in
                        let (eax, w) = _v1 in (zero ,eax ,w ,fpustatus))
            })
            ((update_any
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
                            
# 1014 "x86rec.mlb"
                            ( sprintf "BitInsert(%s, %s, %s)" x y z )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conBitExtract =
    fun arg1 arg2 arg3 ->
        (update_ahval
            {Camlburg.cost =
                (let w = arg3
                in
                    
# 971 "x86rec.mlb"
                    ( guard (w = 8) )
                    
# 000 "/dev/stdout"

                +
                arg1.eight.Camlburg.cost
                +
                arg2._Fetch32.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let eight = arg1.eight.Camlburg.action ()
                    and _v1 = arg2._Fetch32.Camlburg.action ()
                    and w = arg3
                    in
                        let (eax, fw) = _v1
                        in
                            
# 971 "x86rec.mlb"
                            ( "%ah" )
                            
# 000 "/dev/stdout"
)
            })
            ((update_any
                {Camlburg.cost =
                    (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let lsb = arg1.any.Camlburg.action ()
                        and y = arg2.any.Camlburg.action ()
                        and n = arg3
                        in
                            
# 1015 "x86rec.mlb"
                            ( sprintf "BitExtract(%s, %s, %d)" lsb y n )
                            
# 000 "/dev/stdout"
)
                })
                ((update_reg8H
                    {Camlburg.cost =
                        (let w = arg3
                        in
                            
# 663 "x86rec.mlb"
                            ( guard (w=8) )
                            
# 000 "/dev/stdout"

                        +
                        arg1.eight.Camlburg.cost
                        +
                        arg2.regabcd.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let eight = arg1.eight.Camlburg.action ()
                            and regabcd = arg2.regabcd.Camlburg.action ()
                            and w = arg3
                            in
                                
# 663 "x86rec.mlb"
                                ( hregname regabcd )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAnd =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost =
                (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1.any.Camlburg.action ()
                    and y = arg2.any.Camlburg.action ()
                    in
                        
# 992 "x86rec.mlb"
                        ( "And(" ^ x ^ ", " ^ y ^ ")" )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conAh2flags =
    fun arg1 ->
        (update__Ah2flags73
            {Camlburg.cost = (arg1.ahval.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let ahval = arg1.ahval.Camlburg.action () in ahval)
            })
            ((update_any
                {Camlburg.cost = (arg1.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let any = arg1.any.Camlburg.action ()
                        in
                            
# 1017 "x86rec.mlb"
                            ( sprintf "Ah2flags(%s)" any )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conAdd =
    fun arg1 arg2 ->
        (update__Add38
            {Camlburg.cost =
                (arg1.esp.Camlburg.cost + arg2.four.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let esp = arg1.esp.Camlburg.action ()
                    and four = arg2.four.Camlburg.action ()
                    in
                        (esp ,four))
            })
            ((update__Add48
                {Camlburg.cost =
                    (arg1._Fetch47.Camlburg.cost + arg2.zero.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch47.Camlburg.action ()
                        and zero = arg2.zero.Camlburg.action ()
                        in
                            let (fpsp, w) = _v1 in (fpsp ,w ,zero))
                })
                ((update__Add49
                    {Camlburg.cost =
                        (arg1._Fetch47.Camlburg.cost
                        +
                        arg2.one3.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Fetch47.Camlburg.action ()
                            and one3 = arg2.one3.Camlburg.action ()
                            in
                                let (fpsp, w) = _v1 in (fpsp ,w ,one3))
                    })
                    ((update_any
                        {Camlburg.cost =
                            (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let x = arg1.any.Camlburg.action ()
                                and y = arg2.any.Camlburg.action ()
                                in
                                    
# 998 "x86rec.mlb"
                                    ( "Add(" ^ x ^ ", " ^ y ^ ")" )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_const
                            {Camlburg.cost =
                                (arg1.const.Camlburg.cost
                                +
                                arg2.const.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let l = arg1.const.Camlburg.action ()
                                    and r = arg2.const.Camlburg.action ()
                                    in
                                        
# 686 "x86rec.mlb"
                                        ( sprintf "%s+%s" l r )
                                        
# 000 "/dev/stdout"
)
                            })
                            ((update_disp
                                (Camlburg.choice
                                    [{Camlburg.cost =
                                        (arg1.reg.Camlburg.cost
                                        +
                                        arg2.const.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                reg =
                                                arg1.reg.Camlburg.action ()
                                            and
                                                const =
                                                arg2.const.Camlburg.action ()
                                            in
                                                
# 649 "x86rec.mlb"
                                                (  const ^ "(" ^ reg ^ ")" )
                                                
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.const.Camlburg.cost
                                        +
                                        arg2.reg.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                const =
                                                arg1.const.Camlburg.action ()
                                            and
                                                reg =
                                                arg2.reg.Camlburg.action ()
                                            in
                                                
# 650 "x86rec.mlb"
                                                (  const ^ "(" ^ reg ^ ")" )
                                                
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.vfp.Camlburg.cost
                                        +
                                        arg2.const.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                vfp =
                                                arg1.vfp.Camlburg.action ()
                                            and
                                                const =
                                                arg2.const.Camlburg.action ()
                                            in
                                                
# 699 "x86rec.mlb"
                                                (  const ^ "(%vfp)" )
                                                
# 000 "/dev/stdout"
)
                                    }
                                    ;{Camlburg.cost =
                                        (arg1.const.Camlburg.cost
                                        +
                                        arg2.vfp.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                const =
                                                arg1.const.Camlburg.action ()
                                            and
                                                vfp =
                                                arg2.vfp.Camlburg.action ()
                                            in
                                                
# 700 "x86rec.mlb"
                                                (  const ^ "(%vfp)" )
                                                
# 000 "/dev/stdout"
)
                                    }]))
                                ((update_slotaddr
                                    {Camlburg.cost =
                                        (arg1.vfp.Camlburg.cost
                                        +
                                        arg2.const.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                vfp =
                                                arg1.vfp.Camlburg.action ()
                                            and
                                                const =
                                                arg2.const.Camlburg.action ()
                                            in
                                                
# 682 "x86rec.mlb"
                                                ( sprintf "%%vfp+%s" const )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    inf))))))



# 71 "x86rec.mlb"

    (*s: code to follow the labeler *)
    let rec const = function
      | RP.Bool(true)             -> conTrue  ()
      | RP.Bool(false)            -> conFalse ()
      | RP.Link(s,_,w)            -> conLink s w
      | RP.Diff(c1,c2)            -> conDiff (const c1) (const c2)
      | RP.Late(s,w)              -> conLate s w
      | RP.Bits(b)                -> conBits(b)
    (*x: code to follow the labeler *)
    let rec exp = function
      | RP.Const(k)               -> const (k)
      | RP.Fetch(l,w)             -> conFetch (loc l) w
      (*s: special cases for particular operators *)
      | RP.App(("and", [w]), [x; y]) -> conAnd (exp x) (exp y)
      | RP.App(("or",  [w]), [x; y]) -> conOr (exp x) (exp y)
      | RP.App(("xor", [w]), [x; y]) -> conXor (exp x) (exp y)
      | RP.App(("com", [w]), [x])    -> conCom (exp x)
      | RP.App(("add", [w]), [x; y]) -> conAdd (exp x) (exp y)
      | RP.App(("sub", [w]), [x; y]) -> conSub (exp x) (exp y)
      | RP.App((("addc"|"subb"|"carry"|"borrow") as op, [w]), [x; y; z]) ->
          conSinglebit op (exp x) (exp y) (exp z)
      (*| RP.App(("neg", [w]), [x])    -> conNeg (exp x)*)
      | RP.App(("mul", [w]), [x; y]) -> conMul (exp x) (exp y)
      | RP.App(("sx",  [n;w]), [x])   -> conSx  (exp x)
      | RP.App(("zx",  [n;w]), [x])   -> conZx  (exp x)
      | RP.App(("f2f", [n;w]), [x; rm])   -> conF2f n w (exp x) (* need to assert rm *)
      | RP.App(("f2i", [n;w]), [x; rm])   -> conF2i n w (exp x) (* need to assert rm *)
      | RP.App(("i2f", [n;w]), [x; rm])   -> conI2f n w (exp x) (* need to assert rm *)
      | RP.App(("fadd", [w]), [x; y; rm]) -> conFadd (exp x) (exp y) (* need to assert rm *)
      | RP.App(("fsub", [w]), [x; y; rm]) -> conFsub (exp x) (exp y) (* need to assert rm *)
      | RP.App(("fmul", [w]), [x; y; rm]) -> conFmul (exp x) (exp y) (* need to assert rm *)
      | RP.App(("fdiv", [w]), [x; y; rm]) -> conFdiv (exp x) (exp y) (* need to assert rm *)
      | RP.App(("fneg", [w]), [x]) -> conFneg (exp x)
      | RP.App(("fabs", [w]), [x]) -> conFabs (exp x)
      | RP.App(("x86_fcmp", [w]), [x; y]) -> conFcmp (exp x) (exp y)
      | RP.App((("x86_e" | "x86_ne" | "x86_l" | "x86_le" | "x86_g" | "x86_ge"
                | "x86_b" | "x86_be" | "x86_a" | "x86_ae") as o, _), [x]) ->
                    conX86ccop o (exp x)
      | RP.App(("lobits", [w;n]), [x])   -> conLobits (exp x) n
      | RP.App(("x86_subflags", [w]), [x; y]) -> conX86_subflags (exp x) (exp y)
      | RP.App(("x86_addflags", [w]), [x; y]) -> conX86_addflags (exp x) (exp y)
      | RP.App(("x86_ah2flags", []), [x]) -> conAh2flags (exp x)
      | RP.App(("x86_flags2ah", []), [x]) -> conFlags2ah (exp x)
      | RP.App(("x86_repmovs",   [w]), [x;y;z]) -> conRepmovs (exp x) (exp y) (exp z)
      | RP.App(("bitInsert", [w; n]), [lsb; dst; src]) ->
          conBitInsert (exp lsb) (exp dst) (exp src)
      | RP.App(("bitExtract", [w; n]), [lsb; src]) -> conBitExtract (exp lsb) (exp src) n
      | RP.App((("gt"|"lt"|"ge"|"le"|"eq"|"ne"), [w]) as op, [l; r]) ->
          conCmp op (exp l) (exp r)
      | RP.App((("shra"|"shrl"|"shl"|"rotl"|"rotr") as op, [w]), [x; y]) -> conShift op (exp x) (exp y)
      | RP.App((("quot"|"rem"|"div"|"mod"|"divu"|"modu"|"mulx"|"mulux") as op, [w]), [x; y])->
          conNonLlrOp op
      | RP.App((("neg") as op, [w]), [x]) -> conNonLlrOp op
      | RP.App(("x86_idt_pc", []), [x]) -> conX86IdtPc (exp x)
      | RP.App((("add"|"sub"|"mul"|"sx"|"zx"|"lobits"|"x86_subflags"|"bitInsert"|
                 "bitExtract"|"fabs"|"fneg"|"fdiv"|"fmul"|"fsub"|"fadd"|"f2f"|"f2i"|
                 "i2f"|"and"|"or"|"xor"|"com") as op, ws), xs)->
          Impossible.impossible
            (Printf.sprintf
               "operator %%%s specialized to %d widths & applied to %d arguments"
               op (List.length ws) (List.length xs))
      (*e: special cases for particular operators *)
      | RP.App((o,_),_) when String.length o > 4 && String.sub o 0 4 =$= "x86_" ->
          conX86op(o)
      | RP.App((o,_),_)           -> error ("unknown operator " ^ o)
    (*x: code to follow the labeler *)
    and loc l = match l with
      | RP.Mem(('m',_,_), (RP.C c), e, ass) -> conMem (exp e) c
      | RP.Mem(('f',_,_), c, e, ass) -> conFpreg (exp e)
      | (RP.Reg(_, _, _) | RP.Slice(_, _, _)) when RU.Eq.loc l fpcc -> conFpccl ()
      | RP.Reg(sp, i, w) when RU.Eq.loc l fpustatus -> conFpustatusl()
      | RP.Reg(sp, i, w) when RU.Eq.loc l fpuctl    -> conFpuctll()
      | RP.Reg((sp,_,_), i, _) -> conReg sp i
      | RP.Mem(_, _, _, _)     -> error "non-mem, non-reg cell"
      | RP.Var _ | RP.Global _ -> error "var found"
      | RP.Slice(w,i,l)        -> conSlice w i (loc l)
    (*x: code to follow the labeler *)
    and effect = function
      | RP.Store(l, RP.App((o, _), [RP.Fetch(l',_); r]), w) 
        when l =/ l'                      -> conLlr (loc l) o (exp r) w
      | RP.Store(l, RP.App((o, [w']), [RP.Fetch(l',_)]), w)
        when l =/ l' && w = w'            -> conUnaryInPlace (loc l) o w
      | RP.Store(RP.Reg(('c',_,_),i, w),r,_) 
        when (i = SS.indices.SS.pc)       -> conGoto (exp r)
      | RP.Store(RP.Reg(('c', _, _), i, w),r,_)
        when (i = SS.indices.SS.cc)       -> conSetflags (exp r)
      | RP.Store(RP.Reg(('c', _, _), i, _), r, w) -> error ("set $c["^string_of_int i^"]")
      | RP.Store(l,e,w)                   -> conStore (loc l) (exp e) w
      | RP.Kill(l)                        -> conKill (loc l)
    (*x: code to follow the labeler *)
    and regpair = function
      | RP.App(("or",_),[ RP.App(("shl",_), [RP.App(("zx",_), [RP.Fetch(msw,_)]);_])
                        ; RP.App(("zx",_),                    [RP.Fetch(lsw,_)])])
          -> conRegPair (loc msw) (loc lsw)
      | x -> Impossible.impossible "Argument is not a register pair"
    (*x: code to follow the labeler *)
    and rtl (RP.Rtl es) = geffects es
    and geffects = function
        | [] -> conNop ()
        | [g, s] -> guarded g s
        (*s: horrible pattern match for [[Pairdestwithflags]] *)
        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[hi]] := hi32([[l]] [[o]] [[r]] at 32) *)
            RP.Store(hi, RP.App (("lobits", [64;32]),
                                 [RP.App (("shrl", [64]), 
                                          [RP.App((o, _), [l; r]);
                                           RP.Const (RP.Bits k32)])]), 32)
            (*e: pattern for [[hi]] := hi32([[l]] [[o]] [[r]] at 32) *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
             , 
            (*s: pattern for [[lo]] := lo32([[l']] [[o']] [[r']] at 32) *)
            RP.Store(lo, RP.App (("lobits", [64;32]), [RP.App((o', _), [l'; r'])]), 32)
            (*e: pattern for [[lo]] := lo32([[l']] [[o']] [[r']] at 32) *)
             )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for flags := [[fo]] ([[l'']], [[r'']]) *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _),
                     RP.App((fo, _), [RP.Fetch(l'',_); r'']), _)
            (*e: pattern for flags := [[fo]] ([[l'']], [[r'']]) *)
            )
          ] when l =// l' && o =$= o' && r =// r' && l =// RP.Fetch(l'', 32) && r =// r''
              && is32 k32 && flag_index = SS.indices.SS.cc ->
                conPairdestwithflags (loc hi) (loc lo) (exp l) o (exp r) fo
        (*e: horrible pattern match for [[Pairdestwithflags]] *)
        (*s: horrible pattern match for [[Withflags]] *)
        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] *)
            RP.Store(l, RP.App((o, _), [RP.Fetch(l',_); r]), w) 
            (*e: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[l2]] := [[l2']] [[o2]] [[r2]] at [[w2]] *)
            RP.Store(l2, RP.App((o2, _), [RP.Fetch(l2',_); r2]), w2) 
            (*e: pattern for [[l2]] := [[l2']] [[o2]] [[r2]] at [[w2]] *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            ,  
            (*s: pattern for flags := [[%x86_undefflags()]] *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _), RP.App(("x86_undefflags", _), []), _)
            (*e: pattern for flags := [[%x86_undefflags()]] *)
            )
          ] when l =/ l' && l2 =/ l2' && w = w2 && flag_index = SS.indices.SS.cc ->
              conWithundefflags (loc l) o o2 (exp r) w
        (*x: horrible pattern match for [[Withflags]] *)
        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] *)
            RP.Store(l, RP.App((o, _), [RP.Fetch(l',_); r]), w) 
            (*e: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
           , 
           (*s: pattern for flags := [[fo]] ([[l'']], [[r']]) *)
           RP.Store(RP.Reg(('c', _, _), flag_index, _),
                    RP.App((fo, _), [RP.Fetch(l'',_); r']), _)
           (*e: pattern for flags := [[fo]] ([[l'']], [[r']]) *)
            )
          ] when l =/ l' && l =/ l'' && r =// r' && flag_index = SS.indices.SS.cc ->
              conWithaflags (loc l) o (exp r) w fo
        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[l]] := [[l']] at [[w]] *)
            RP.Store(l, RP.Fetch(l',_), w) 
            (*e: pattern for [[l]] := [[l']] at [[w]] *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for flags := [[fo]] ([[l'']], [[Const b]]) *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _),
                     RP.App((fo, _), [RP.Fetch(l'',_); RP.Const (RP.Bits b)]), _)
            (*e: pattern for flags := [[fo]] ([[l'']], [[Const b]]) *)
             )
          ] when l =/ l' && l =/ l'' && Bits.is_zero b && flag_index = SS.indices.SS.cc ->
                conWithaflags (loc l) "add" (exp (RP.Const (RP.Bits b))) w fo
        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[l]] := [[o]] [[l']] at [[w]] *)
            RP.Store(l, RP.App((o, _), [RP.Fetch(l',_)]), w) 
            (*e: pattern for [[l]] := [[o]] [[l']] at [[w]] *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for flags := [[fo]] [[l'']]) *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _),
                     RP.App((fo, _), [RP.Fetch(l'',_)]), _)
            (*e: pattern for flags := [[fo]] [[l'']]) *)
            )
          ] when l =/ l' && l =/ l'' && flag_index = SS.indices.SS.cc ->
              conWithaflagsunary o (loc l) w fo
        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[d]] := rp:[[l1/l2]] [[o]] [[r]] at [[w]] *)
            RP.Store(d, RP.App((o, _),
              [ (RP.App(("or",_), [ RP.App(("shl",_),
                                           [RP.App(("zx",_), [RP.Fetch(l1,_)]);_]);
                                            RP.App(("zx",_), [RP.Fetch(l2,_)])]) as rp);
                r]), w)
            (*e: pattern for [[d]] := rp:[[l1/l2]] [[o]] [[r]] at [[w]] *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for flags := [[fo]] ([[l1'/l2']], [[r']]) *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _),
                     RP.App((fo, _), 
                          [ RP.App(("or",_), [ RP.App(("shl",_),
                                                 [RP.App(("zx",_), [RP.Fetch(l1',_)]);_])
                                 ;                RP.App(("zx",_), [RP.Fetch(l2',_)])])
                          ; r']), _)
            (*e: pattern for flags := [[fo]] ([[l1'/l2']], [[r']]) *)
            )
          ] when l1 =/ l1' && l2 =/ l2' && r =// r' && flag_index = SS.indices.SS.cc ->
              conWithaflags (regpair rp) o (exp r) w fo
        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[d]] := rp:[[l1/l2]] [[o]] [[r]] at [[w]] *)
            RP.Store(d, RP.App((o, _),
              [ (RP.App(("or",_), [ RP.App(("shl",_),
                                           [RP.App(("zx",_), [RP.Fetch(l1,_)]);_]);
                                            RP.App(("zx",_), [RP.Fetch(l2,_)])]) as rp);
                r]), w)
            (*e: pattern for [[d]] := rp:[[l1/l2]] [[o]] [[r]] at [[w]] *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[d']] := rp':[[l1'/l2']] [[o']] [[r']] at [[w']] *)
            RP.Store(d', RP.App((o', _),
              [ (RP.App(("or",_), [ RP.App(("shl",_),
                                           [RP.App(("zx",_), [RP.Fetch(l1',_)]);_]);
                                            RP.App(("zx",_), [RP.Fetch(l2',_)])]) as rp');
                r']), w')
            (*e: pattern for [[d']] := rp':[[l1'/l2']] [[o']] [[r']] at [[w']] *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for flags := [[%x86_undefflags()]] *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _), RP.App(("x86_undefflags", _), []), _)
            (*e: pattern for flags := [[%x86_undefflags()]] *)
            )
          ] when w = w' && r =// r' && rp =// rp' && flag_index = SS.indices.SS.cc ->
              conWithundefflags (regpair rp) o o' (exp r) w
        | [ (
             (*s: truepat *)
             RP.Const(RP.Bool true)
             (*e: truepat *)
             , 
             (*s: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] *)
             RP.Store(l, RP.App((o, _), [RP.Fetch(l',_); r]), w) 
             (*e: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] *)
             )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for flags := [[fo]] ([[l'']] [[o']] [[r']]) *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _),
                     RP.App((fo, _), [RP.App ((o', _), [RP.Fetch(l'',_); r'])]), _)

            (*e: pattern for flags := [[fo]] ([[l'']] [[o']] [[r']]) *)
             )
          ] when l =/ l' && l =/ l'' && r =// r' && o =$= o'
            && flag_index = SS.indices.SS.cc ->
              conWithlflags (loc l) o (exp r) w fo
        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
           , 
           (*s: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] at H register *)
           RP.Store(l, RP.App(("bitInsert", [32; 8]),
                              [RP.Const (RP.Bits eightbits);
                               RP.Fetch(l', _);
                               RP.App((o, _),
                                   [ RP.App(("bitExtract", [32; 8]),
                                              [RP.Const (RP.Bits eightbits'); RP.Fetch(l'',_)]);
                                       r]) as flag_left]),
                    w)
           (*e: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] at H register *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for flags := [[fo]] [[flag_left']] *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _), RP.App((fo, _), [flag_left']), _)
            (*e: pattern for flags := [[fo]] [[flag_left']] *)
            )
          ] when is8 eightbits && is8 eightbits' && l =/ l' && l =/ l''
              && flag_left =// flag_left' && flag_index = SS.indices.SS.cc ->
              conWithlflags8H (loc l) o (exp r) fo

        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
           , 
           (*s: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] at H register *)
           RP.Store(l, RP.App(("bitInsert", [32; 8]),
                              [RP.Const (RP.Bits eightbits);
                               RP.Fetch(l', _);
                               RP.App((o, _),
                                   [ RP.App(("bitExtract", [32; 8]),
                                              [RP.Const (RP.Bits eightbits'); RP.Fetch(l'',_)]);
                                       r]) as flag_left]),
                    w)
           (*e: pattern for [[l]] := [[l']] [[o]] [[r]] at [[w]] at H register *)
            )
          ; _
          ] when is8 eightbits && is8 eightbits' && l =/ l' && l =/ l'' ->
              conWithlflags8H (loc l) o (exp r) "x86_mumbojumbo"
        (*
          $r[0] := %%bitInsert(8, $r[0], %%and(%%bitExtract(8, $r[0]), 69)) 
        | $c[2] := %%x86_logicflags(%%and(%%bitExtract(8, $r[0]), 69))
        *)
        (*e: horrible pattern match for [[Withflags]] *)
        (*s: horrible pattern match for [[addc]] and [[subb]] *)
        | [ (
             (*s: truepat *)
             RP.Const(RP.Bool true)
             (*e: truepat *)
             , 
             (*s: pattern for [[l]] := [[o(l',r,CF)]] at [[w]] *)
             RP.Store(l, RP.App((o, _), [RP.Fetch(l',_); r; 
                 (*s: cfpat 1 *)
                 RP.App (("x86_carrybit", _), [RP.Fetch(RP.Reg(('c', _, _), cf_index1, _), _)])
                 (*e: cfpat 1 *)
                ]), w) 
             (*e: pattern for [[l]] := [[o(l',r,CF)]] at [[w]] *)
             )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for flags := [[fo]] ([[l'', r', CF]]) *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _),
                     RP.App((fo, _), [RP.Fetch(l'',_); r';
                  (*s: cfpat 2 *)
                  RP.App (("x86_carrybit", _), [RP.Fetch(RP.Reg(('c', _, _), cf_index2, _), _)])
                  (*e: cfpat 2 *)
                ]), _)
            (*e: pattern for flags := [[fo]] ([[l'', r', CF]]) *)
             )
          ] when l =/ l' && l =/ l'' && r =// r' && flag_index = SS.indices.SS.cc &&
                 cf_index1 = flag_index && cf_index2 = flag_index ->
              conWithcarryflags (loc l) o (exp r) w fo
        (*x: horrible pattern match for [[addc]] and [[subb]] *)
        | [ (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for [[l]] := [[o(l',r,z1)]] at [[w]] *)
            RP.Store(l, RP.App((o, _), [RP.Fetch(l',_); r; RP.Const (RP.Bits z1)]), w) 
            (*e: pattern for [[l]] := [[o(l',r,z1)]] at [[w]] *)
            )
          ; (
            (*s: truepat *)
            RP.Const(RP.Bool true)
            (*e: truepat *)
            , 
            (*s: pattern for flags := [[fo]] ([[l'', r', z2]]) *)
            RP.Store(RP.Reg(('c', _, _), flag_index, _),
                     RP.App((fo, _), [RP.Fetch(l'',_); r';RP.Const (RP.Bits z2)]), _)
            (*e: pattern for flags := [[fo]] ([[l'', r', z2]]) *)
            )
          ] when l =/ l' && l =/ l'' && r =// r' && flag_index = SS.indices.SS.cc &&
                 Bits.is_zero z1 && Bits.is_zero z2 ->
              conWithcarryzero (loc l) o (exp r) w fo
        (*e: horrible pattern match for [[addc]] and [[subb]] *)
        | (g, s) :: t -> conPar (guarded g s) (geffects t)
    and is8 b =
      Bits.width b > 3 & Bits.Ops.eq b (Bits.U.of_int 8  (Bits.width b))
    and is32 b =
      Bits.width b > 5 & Bits.Ops.eq b (Bits.U.of_int 32 (Bits.width b))
    (*x: code to follow the labeler *)
    and guarded g eff = match g with
    | RP.Const(RP.Bool b) -> if b then effect eff else conNop()
    | RP.App((compare, [32]), [RP.Fetch(RP.Reg(('c', _, _), n, _), _)])
        when n = SS.indices.SS.cc && begins_x86_ compare ->
          (match eff with
          | RP.Store(RP.Reg(('c', _, _), i, _), r, w)
            when (i = SS.indices.SS.pc) ->
              conJcc (String.sub compare 4 (String.length compare - 4)) (exp r)
          | _ -> error "guard on x86 non-branch")
    | _ -> conGuarded (exp g) (effect eff)
    and begins_x86_ s =
      String.length s >= 4 &&
      s.[0] =<= 'x' && s.[1] =<= '8' && s.[2] =<= '6' && s.[3] =<= '_'
    (*x: code to follow the labeler *)
    let errmsg r msg =
      List.iter prerr_string
        [ "recognizer error: "; msg; " on "; RU.ToString.rtl r; "\n" ]

    let to_asm r =
      try
        let plan = rtl (Dn.rtl r) in
        plan.inst.Camlburg.action ()
      with 
      | Camlburg.Uncovered -> s " not an instruction: %s"  (RU.ToString.rtl r)
      | Error msg -> (errmsg r msg; " error in recognizer: " ^ msg)

    let () = Debug.register "x86rec" "diagnose rejected instructions in x86 recognizer"

    let is_instruction r =
      try
        let plan = rtl (Dn.rtl r) in
        if plan.inst.Camlburg.cost < 100 then  (* should be true, but shade this... *)
          true
        else
          (if Debug.on "x86rec" then
             Printf.eprintf "x86rec: rejected RTL %s\n" (plan.inst.Camlburg.action());
           false)
      with 
      | Camlburg.Uncovered -> false
      | Error msg -> (errmsg r msg; false)
    (*e: code to follow the labeler *)
  end (* of M *) 


# 000 "/dev/stdout"
