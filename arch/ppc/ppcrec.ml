
# 2 "ppcrec.mlb"
 
    (*s: modules *)
    open Nopoly
    module BO   = Bits.Ops
    module RP   = Rtl.Private
    module RU   = Rtlutil
    module SS   = Space.Standard32
    module Down = Rtl.Dn      (* Convert Down  to private repr. *)
    module Up   = Rtl.Up      (* Convert Up    to abstract repr. *)

    exception Error of string
    let error msg = raise (Error msg)
    let sprintf = Printf.sprintf
    (*e: modules *)
         module M = struct
           (*s: code to precede the labeler *)
           let s = Printf.sprintf
           (*x: code to precede the labeler *)
           let infinity = Camlburg.inf_cost
           let guard b = if b then 0 else infinity
           (*x: code to precede the labeler *)
           let imports = ref ([] : string list)

           let ind_addr name =
             if List.exists ((=$=) name) (!imports) then "L" ^ name ^ "$stub" else name

           let ppc_op = function
             | "ltu" -> ("l", "lt")
             | "leu" -> ("l", "le")
             | "gtu" -> ("l", "gt")
             | "geu" -> ("l", "ge")
             | op    -> ("" , op  )
           (*e: code to precede the labeler *)
      

# 000 "/dev/stdout"


type
    (
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
        _Zx9: ( 't59 ) Camlburg.nt;
        _Zx7: ( 't58 ) Camlburg.nt;
        _Zx13: ( 't57 ) Camlburg.nt;
        _Zx11: ( 't56 ) Camlburg.nt;
        _Unop32: ( 't55 ) Camlburg.nt;
        _Sxlo23: ( 't54 ) Camlburg.nt;
        _Sxlo16: ( 't53 ) Camlburg.nt;
        _Sxlo15: ( 't52 ) Camlburg.nt;
        _Sx18: ( 't51 ) Camlburg.nt;
        _Sx17: ( 't50 ) Camlburg.nt;
        _Store26: ( 't49 ) Camlburg.nt;
        _Store25: ( 't48 ) Camlburg.nt;
        _OvSet28: ( 't47 ) Camlburg.nt;
        _Mem6: ( 't46 ) Camlburg.nt;
        _Mem4: ( 't45 ) Camlburg.nt;
        _Mem1: ( 't44 ) Camlburg.nt;
        _Lobits20: ( 't43 ) Camlburg.nt;
        _Lobits19: ( 't42 ) Camlburg.nt;
        _Goto27: ( 't41 ) Camlburg.nt;
        _Goto24: ( 't40 ) Camlburg.nt;
        _Fetch8: ( 't39 ) Camlburg.nt;
        _Fetch5: ( 't38 ) Camlburg.nt;
        _Fetch3: ( 't37 ) Camlburg.nt;
        _Fetch29: ( 't36 ) Camlburg.nt;
        _Fetch14: ( 't35 ) Camlburg.nt;
        _Fetch12: ( 't34 ) Camlburg.nt;
        _Fetch10: ( 't33 ) Camlburg.nt;
        _Diff2: ( 't32 ) Camlburg.nt;
        _Binop34: ( 't31 ) Camlburg.nt;
        _Binop33: ( 't30 ) Camlburg.nt;
        _Add31: ( 't29 ) Camlburg.nt;
        _Add30: ( 't28 ) Camlburg.nt;
        _Add22: ( 't27 ) Camlburg.nt;
        _Add21: ( 't26 ) Camlburg.nt;
        xerl: ( 't25 ) Camlburg.nt;
        spl: ( 't24 ) Camlburg.nt;
        sp: ( 't23 ) Camlburg.nt;
        regl: ( 't22 ) Camlburg.nt;
        reg: ( 't21 ) Camlburg.nt;
        pic: ( 't20 ) Camlburg.nt;
        pcl: ( 't19 ) Camlburg.nt;
        pc: ( 't18 ) Camlburg.nt;
        next: ( 't17 ) Camlburg.nt;
        ndx_addr: ( 't16 ) Camlburg.nt;
        lrl: ( 't15 ) Camlburg.nt;
        lr: ( 't14 ) Camlburg.nt;
        lconst: ( 't13 ) Camlburg.nt;
        k4: ( 't12 ) Camlburg.nt;
        k16: ( 't11 ) Camlburg.nt;
        k15: ( 't10 ) Camlburg.nt;
        inst: ( 't9 ) Camlburg.nt;
        ha16: ( 't8 ) Camlburg.nt;
        crl: ( 't7 ) Camlburg.nt;
        cr: ( 't6 ) Camlburg.nt;
        const16: ( 't5 ) Camlburg.nt;
        cmp: ( 't4 ) Camlburg.nt;
        cial: ( 't3 ) Camlburg.nt;
        cia: ( 't2 ) Camlburg.nt;
        any: ( 't1 ) Camlburg.nt;
        addr: ( 't0 ) Camlburg.nt
    }

let rec
inf =
    {addr = (Camlburg.infinity)
    ;any = (Camlburg.infinity)
    ;cia = (Camlburg.infinity)
    ;cial = (Camlburg.infinity)
    ;cmp = (Camlburg.infinity)
    ;const16 = (Camlburg.infinity)
    ;cr = (Camlburg.infinity)
    ;crl = (Camlburg.infinity)
    ;ha16 = (Camlburg.infinity)
    ;inst = (Camlburg.infinity)
    ;k15 = (Camlburg.infinity)
    ;k16 = (Camlburg.infinity)
    ;k4 = (Camlburg.infinity)
    ;lconst = (Camlburg.infinity)
    ;lr = (Camlburg.infinity)
    ;lrl = (Camlburg.infinity)
    ;ndx_addr = (Camlburg.infinity)
    ;next = (Camlburg.infinity)
    ;pc = (Camlburg.infinity)
    ;pcl = (Camlburg.infinity)
    ;pic = (Camlburg.infinity)
    ;reg = (Camlburg.infinity)
    ;regl = (Camlburg.infinity)
    ;sp = (Camlburg.infinity)
    ;spl = (Camlburg.infinity)
    ;xerl = (Camlburg.infinity)
    ;_Add21 = (Camlburg.infinity)
    ;_Add22 = (Camlburg.infinity)
    ;_Add30 = (Camlburg.infinity)
    ;_Add31 = (Camlburg.infinity)
    ;_Binop33 = (Camlburg.infinity)
    ;_Binop34 = (Camlburg.infinity)
    ;_Diff2 = (Camlburg.infinity)
    ;_Fetch10 = (Camlburg.infinity)
    ;_Fetch12 = (Camlburg.infinity)
    ;_Fetch14 = (Camlburg.infinity)
    ;_Fetch29 = (Camlburg.infinity)
    ;_Fetch3 = (Camlburg.infinity)
    ;_Fetch5 = (Camlburg.infinity)
    ;_Fetch8 = (Camlburg.infinity)
    ;_Goto24 = (Camlburg.infinity)
    ;_Goto27 = (Camlburg.infinity)
    ;_Lobits19 = (Camlburg.infinity)
    ;_Lobits20 = (Camlburg.infinity)
    ;_Mem1 = (Camlburg.infinity)
    ;_Mem4 = (Camlburg.infinity)
    ;_Mem6 = (Camlburg.infinity)
    ;_OvSet28 = (Camlburg.infinity)
    ;_Store25 = (Camlburg.infinity)
    ;_Store26 = (Camlburg.infinity)
    ;_Sx17 = (Camlburg.infinity)
    ;_Sx18 = (Camlburg.infinity)
    ;_Sxlo15 = (Camlburg.infinity)
    ;_Sxlo16 = (Camlburg.infinity)
    ;_Sxlo23 = (Camlburg.infinity)
    ;_Unop32 = (Camlburg.infinity)
    ;_Zx11 = (Camlburg.infinity)
    ;_Zx13 = (Camlburg.infinity)
    ;_Zx7 = (Camlburg.infinity)
    ;_Zx9 = (Camlburg.infinity)
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
                                
# 267 "ppcrec.mlb"
                                ( s "<%s>" any )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with any = (nt) }
and update_cia =
    fun nt x ->
        if nt.Camlburg.cost >= x.cia.Camlburg.cost then
            x
        else
            { x with cia = (nt) }
and update_cial =
    fun nt x ->
        if nt.Camlburg.cost >= x.cial.Camlburg.cost then
            x
        else
            { x with cial = (nt) }
and update_cmp =
    fun nt x ->
        if nt.Camlburg.cost >= x.cmp.Camlburg.cost then
            x
        else
            { x with cmp = (nt) }
and update_const16 =
    fun nt x ->
        if nt.Camlburg.cost >= x.const16.Camlburg.cost then
            x
        else
            (fun x ->
                (update_addr
                    {Camlburg.cost = (nt.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let const16 = x.const16.Camlburg.action ()
                            in
                                
# 202 "ppcrec.mlb"
                                ( s "%s(r0)" const16     )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with const16 = (nt) }
and update_cr =
    fun nt x ->
        if nt.Camlburg.cost >= x.cr.Camlburg.cost then
            x
        else
            { x with cr = (nt) }
and update_crl =
    fun nt x ->
        if nt.Camlburg.cost >= x.crl.Camlburg.cost then
            x
        else
            { x with crl = (nt) }
and update_ha16 =
    fun nt x ->
        if nt.Camlburg.cost >= x.ha16.Camlburg.cost then
            x
        else
            { x with ha16 = (nt) }
and update_inst =
    fun nt x ->
        if nt.Camlburg.cost >= x.inst.Camlburg.cost then
            x
        else
            { x with inst = (nt) }
and update_k15 =
    fun nt x ->
        if nt.Camlburg.cost >= x.k15.Camlburg.cost then
            x
        else
            { x with k15 = (nt) }
and update_k16 =
    fun nt x ->
        if nt.Camlburg.cost >= x.k16.Camlburg.cost then
            x
        else
            { x with k16 = (nt) }
and update_k4 =
    fun nt x ->
        if nt.Camlburg.cost >= x.k4.Camlburg.cost then
            x
        else
            { x with k4 = (nt) }
and update_lconst =
    fun nt x ->
        if nt.Camlburg.cost >= x.lconst.Camlburg.cost then
            x
        else
            { x with lconst = (nt) }
and update_lr =
    fun nt x ->
        if nt.Camlburg.cost >= x.lr.Camlburg.cost then
            x
        else
            { x with lr = (nt) }
and update_lrl =
    fun nt x ->
        if nt.Camlburg.cost >= x.lrl.Camlburg.cost then
            x
        else
            { x with lrl = (nt) }
and update_ndx_addr =
    fun nt x ->
        if nt.Camlburg.cost >= x.ndx_addr.Camlburg.cost then
            x
        else
            { x with ndx_addr = (nt) }
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
and update_pic =
    fun nt x ->
        if nt.Camlburg.cost >= x.pic.Camlburg.cost then
            x
        else
            { x with pic = (nt) }
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
                                
# 203 "ppcrec.mlb"
                                ( s  "0(%s)" reg         )
                                
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
and update_xerl =
    fun nt x ->
        if nt.Camlburg.cost >= x.xerl.Camlburg.cost then
            x
        else
            { x with xerl = (nt) }
and update__Add21 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add21.Camlburg.cost then
            x
        else
            { x with _Add21 = (nt) }
and update__Add22 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add22.Camlburg.cost then
            x
        else
            { x with _Add22 = (nt) }
and update__Add30 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add30.Camlburg.cost then
            x
        else
            { x with _Add30 = (nt) }
and update__Add31 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add31.Camlburg.cost then
            x
        else
            { x with _Add31 = (nt) }
and update__Binop33 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Binop33.Camlburg.cost then
            x
        else
            { x with _Binop33 = (nt) }
and update__Binop34 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Binop34.Camlburg.cost then
            x
        else
            { x with _Binop34 = (nt) }
and update__Diff2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Diff2.Camlburg.cost then
            x
        else
            { x with _Diff2 = (nt) }
and update__Fetch10 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch10.Camlburg.cost then
            x
        else
            { x with _Fetch10 = (nt) }
and update__Fetch12 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch12.Camlburg.cost then
            x
        else
            { x with _Fetch12 = (nt) }
and update__Fetch14 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch14.Camlburg.cost then
            x
        else
            { x with _Fetch14 = (nt) }
and update__Fetch29 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch29.Camlburg.cost then
            x
        else
            { x with _Fetch29 = (nt) }
and update__Fetch3 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch3.Camlburg.cost then
            x
        else
            { x with _Fetch3 = (nt) }
and update__Fetch5 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch5.Camlburg.cost then
            x
        else
            { x with _Fetch5 = (nt) }
and update__Fetch8 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Fetch8.Camlburg.cost then
            x
        else
            { x with _Fetch8 = (nt) }
and update__Goto24 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto24.Camlburg.cost then
            x
        else
            { x with _Goto24 = (nt) }
and update__Goto27 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Goto27.Camlburg.cost then
            x
        else
            { x with _Goto27 = (nt) }
and update__Lobits19 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Lobits19.Camlburg.cost then
            x
        else
            { x with _Lobits19 = (nt) }
and update__Lobits20 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Lobits20.Camlburg.cost then
            x
        else
            { x with _Lobits20 = (nt) }
and update__Mem1 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mem1.Camlburg.cost then
            x
        else
            { x with _Mem1 = (nt) }
and update__Mem4 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mem4.Camlburg.cost then
            x
        else
            { x with _Mem4 = (nt) }
and update__Mem6 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Mem6.Camlburg.cost then
            x
        else
            { x with _Mem6 = (nt) }
and update__OvSet28 =
    fun nt x ->
        if nt.Camlburg.cost >= x._OvSet28.Camlburg.cost then
            x
        else
            { x with _OvSet28 = (nt) }
and update__Store25 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store25.Camlburg.cost then
            x
        else
            { x with _Store25 = (nt) }
and update__Store26 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Store26.Camlburg.cost then
            x
        else
            { x with _Store26 = (nt) }
and update__Sx17 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sx17.Camlburg.cost then
            x
        else
            { x with _Sx17 = (nt) }
and update__Sx18 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sx18.Camlburg.cost then
            x
        else
            { x with _Sx18 = (nt) }
and update__Sxlo15 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sxlo15.Camlburg.cost then
            x
        else
            { x with _Sxlo15 = (nt) }
and update__Sxlo16 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sxlo16.Camlburg.cost then
            x
        else
            { x with _Sxlo16 = (nt) }
and update__Sxlo23 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Sxlo23.Camlburg.cost then
            x
        else
            { x with _Sxlo23 = (nt) }
and update__Unop32 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Unop32.Camlburg.cost then
            x
        else
            { x with _Unop32 = (nt) }
and update__Zx11 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zx11.Camlburg.cost then
            x
        else
            { x with _Zx11 = (nt) }
and update__Zx13 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zx13.Camlburg.cost then
            x
        else
            { x with _Zx13 = (nt) }
and update__Zx7 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zx7.Camlburg.cost then
            x
        else
            { x with _Zx7 = (nt) }
and update__Zx9 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Zx9.Camlburg.cost then
            x
        else
            { x with _Zx9 = (nt) }


let rec
conZxlo =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    and w = arg2
                    in
                        
# 280 "ppcrec.mlb"
                        ( s "Zxlo(%s,%d)" any w )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conZx =
    fun arg1 ->
        (update__Zx11
            {Camlburg.cost = (arg1._Fetch12.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch12.Camlburg.action ()
                    in
                        let addr = _v1 in addr)
            })
            ((update__Zx13
                {Camlburg.cost = (arg1._Fetch14.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch14.Camlburg.action ()
                        in
                            let ndx_addr = _v1 in ndx_addr)
                })
                ((update__Zx7
                    {Camlburg.cost = (arg1._Fetch8.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Fetch8.Camlburg.action ()
                            in
                                let addr = _v1 in addr)
                    })
                    ((update__Zx9
                        {Camlburg.cost = (arg1._Fetch10.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let _v1 = arg1._Fetch10.Camlburg.action ()
                                in
                                    let ndx_addr = _v1 in ndx_addr)
                        })
                        ((update_any
                            {Camlburg.cost = (arg1.any.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let any = arg1.any.Camlburg.action ()
                                    in
                                        
# 278 "ppcrec.mlb"
                                        ( s "Zx(%s)" any  )
                                        
# 000 "/dev/stdout"
)
                            })
                            inf))))
and conUnop =
    fun arg1 arg2 ->
        (update__Unop32
            {Camlburg.cost = (arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let opr = arg1
                    and x = arg2.reg.Camlburg.action ()
                    in
                        (opr ,x))
            })
            ((update_any
                {Camlburg.cost = (arg2.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let op = arg1
                        and x = arg2.any.Camlburg.action ()
                        in
                            
# 286 "ppcrec.mlb"
                            ( s "Unop(%s,%s)" op x  )
                            
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
                    
# 269 "ppcrec.mlb"
                    ( "True"  )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conSxlo =
    fun arg1 arg2 ->
        (update__Sxlo15
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + (Camlburg.matches 8) arg2)
            ;Camlburg.action =
                (fun () -> let reg = arg1.reg.Camlburg.action () in reg)
            })
            ((update__Sxlo16
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + (Camlburg.matches 16) arg2)
                ;Camlburg.action =
                    (fun () -> let reg = arg1.reg.Camlburg.action () in reg)
                })
                ((update__Sxlo23
                    {Camlburg.cost =
                        (arg1.pic.Camlburg.cost + (Camlburg.matches 16) arg2)
                    ;Camlburg.action =
                        (fun () ->
                            let pic = arg1.pic.Camlburg.action () in pic)
                    })
                    ((update_any
                        {Camlburg.cost = (arg1.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let any = arg1.any.Camlburg.action ()
                                and w = arg2
                                in
                                    
# 279 "ppcrec.mlb"
                                    ( s "Sxlo(%s,%d)" any w )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conSx =
    fun arg1 ->
        (update__Sx17
            {Camlburg.cost = (arg1._Fetch12.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch12.Camlburg.action ()
                    in
                        let addr = _v1 in addr)
            })
            ((update__Sx18
                {Camlburg.cost = (arg1._Fetch14.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Fetch14.Camlburg.action ()
                        in
                            let ndx_addr = _v1 in ndx_addr)
                })
                ((update_any
                    {Camlburg.cost = (arg1.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let any = arg1.any.Camlburg.action ()
                            in
                                
# 277 "ppcrec.mlb"
                                ( s "Sx(%s)" any  )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conStore =
    fun arg1 arg2 arg3 ->
        (update__Store25
            {Camlburg.cost =
                (arg1.regl.Camlburg.cost + arg2.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let regl = arg1.regl.Camlburg.action ()
                    and reg = arg2.reg.Camlburg.action ()
                    and w = arg3
                    in
                        (regl ,reg ,w))
            })
            ((update__Store26
                {Camlburg.cost =
                    (arg1.lrl.Camlburg.cost + arg2.next.Camlburg.cost
                    +
                    (Camlburg.matches 32) arg3)
                ;Camlburg.action =
                    (fun () ->
                        let lrl = arg1.lrl.Camlburg.action ()
                        and next = arg2.next.Camlburg.action ()
                        in
                            (lrl ,next))
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
                                
# 299 "ppcrec.mlb"
                                ( s "Store(%s,%s,%d)" dst src w )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_inst
                        (Camlburg.choice
                            [{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    and w = arg3
                                    in
                                        
# 208 "ppcrec.mlb"
                                        ( s "mr %s,%s" regl reg )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.lr.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and lr = arg2.lr.Camlburg.action ()
                                    and w = arg3
                                    in
                                        
# 209 "ppcrec.mlb"
                                        ( s "mflr %s" regl )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.lrl.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let lrl = arg1.lrl.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    and w = arg3
                                    in
                                        
# 210 "ppcrec.mlb"
                                        ( s "mtlr %s" reg  )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.cr.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and cr = arg2.cr.Camlburg.action ()
                                    and w = arg3
                                    in
                                        
# 211 "ppcrec.mlb"
                                        ( s "mfcr %s" regl )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.const16.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and
                                        const16 =
                                        arg2.const16.Camlburg.action ()
                                    in
                                        
# 213 "ppcrec.mlb"
                                        ( s "addi %s,0,%s" regl const16 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Fetch3.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Fetch3.Camlburg.action ()
                                    in
                                        let addr = _v1
                                        in
                                            
# 215 "ppcrec.mlb"
                                            ( s "lwz %s,%s" regl addr  )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Fetch5.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Fetch5.Camlburg.action ()
                                    in
                                        let ndx_addr = _v1
                                        in
                                            
# 216 "ppcrec.mlb"
                                            ( s "lwzx %s,%s" regl ndx_addr )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Zx7.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Zx7.Camlburg.action ()
                                    in
                                        let addr = _v1
                                        in
                                            
# 217 "ppcrec.mlb"
                                            ( s "lbz %s,%s" regl addr  )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Zx9.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Zx9.Camlburg.action ()
                                    in
                                        let ndx_addr = _v1
                                        in
                                            
# 218 "ppcrec.mlb"
                                            ( s "lbzx %s,%s" regl ndx_addr  )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Zx11.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Zx11.Camlburg.action ()
                                    in
                                        let addr = _v1
                                        in
                                            
# 219 "ppcrec.mlb"
                                            ( s "lhz %s,%s" regl addr  )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Zx13.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Zx13.Camlburg.action ()
                                    in
                                        let ndx_addr = _v1
                                        in
                                            
# 220 "ppcrec.mlb"
                                            ( s "lhzx %s,%s" regl ndx_addr  )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sxlo15.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sxlo15.Camlburg.action ()
                                    in
                                        let reg = _v1
                                        in
                                            
# 221 "ppcrec.mlb"
                                            ( s "extsb %s,%s" regl reg )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sxlo16.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sxlo16.Camlburg.action ()
                                    in
                                        let reg = _v1
                                        in
                                            
# 222 "ppcrec.mlb"
                                            ( s "extsh %s,%s" regl reg )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sx17.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sx17.Camlburg.action ()
                                    in
                                        let addr = _v1
                                        in
                                            
# 223 "ppcrec.mlb"
                                            ( s "lha %s,%s" regl addr  )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sx18.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sx18.Camlburg.action ()
                                    in
                                        let ndx_addr = _v1
                                        in
                                            
# 224 "ppcrec.mlb"
                                            ( s "lhax %s,%s" regl ndx_addr  )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1._Mem4.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let _v1 = arg1._Mem4.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    in
                                        let addr = _v1
                                        in
                                            
# 226 "ppcrec.mlb"
                                            ( s "stw %s,%s" reg addr  )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1._Mem6.Camlburg.cost
                                +
                                arg2.reg.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let _v1 = arg1._Mem6.Camlburg.action ()
                                    and reg = arg2.reg.Camlburg.action ()
                                    in
                                        let ndx_addr = _v1
                                        in
                                            
# 227 "ppcrec.mlb"
                                            ( s "stwx %s,%s" reg ndx_addr )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1._Mem4.Camlburg.cost
                                +
                                arg2._Lobits19.Camlburg.cost
                                +
                                (Camlburg.matches 8) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let _v1 = arg1._Mem4.Camlburg.action ()
                                    and
                                        _v2 =
                                        arg2._Lobits19.Camlburg.action ()
                                    in
                                        let reg = _v2
                                        in
                                            let addr = _v1
                                            in
                                                
# 228 "ppcrec.mlb"
                                                ( s "stb %s,%s" reg addr  )
                                                
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1._Mem6.Camlburg.cost
                                +
                                arg2._Lobits19.Camlburg.cost
                                +
                                (Camlburg.matches 8) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let _v1 = arg1._Mem6.Camlburg.action ()
                                    and
                                        _v2 =
                                        arg2._Lobits19.Camlburg.action ()
                                    in
                                        let reg = _v2
                                        in
                                            let ndx_addr = _v1
                                            in
                                                
# 229 "ppcrec.mlb"
                                                ( s "stbx %s,%s" reg ndx_addr  )
                                                
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1._Mem4.Camlburg.cost
                                +
                                arg2._Lobits20.Camlburg.cost
                                +
                                (Camlburg.matches 16) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let _v1 = arg1._Mem4.Camlburg.action ()
                                    and
                                        _v2 =
                                        arg2._Lobits20.Camlburg.action ()
                                    in
                                        let reg = _v2
                                        in
                                            let addr = _v1
                                            in
                                                
# 230 "ppcrec.mlb"
                                                ( s "sth %s,%s" reg addr  )
                                                
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1._Mem6.Camlburg.cost
                                +
                                arg2._Lobits20.Camlburg.cost
                                +
                                (Camlburg.matches 16) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let _v1 = arg1._Mem6.Camlburg.action ()
                                    and
                                        _v2 =
                                        arg2._Lobits20.Camlburg.action ()
                                    in
                                        let reg = _v2
                                        in
                                            let ndx_addr = _v1
                                            in
                                                
# 231 "ppcrec.mlb"
                                                ( s "sthx %s,%s" reg ndx_addr  )
                                                
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add21.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add21.Camlburg.action ()
                                    in
                                        let (reg, ha16) = _v1
                                        in
                                            
# 233 "ppcrec.mlb"
                                            ( s "addis %s,%s,%s" regl reg ha16 )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2.ha16.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and ha16 = arg2.ha16.Camlburg.action ()
                                    in
                                        
# 234 "ppcrec.mlb"
                                        ( s "addis %s,0,%s" regl ha16 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add22.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add22.Camlburg.action ()
                                    in
                                        let (reg, pic) = _v1
                                        in
                                            
# 236 "ppcrec.mlb"
                                            ( s "addi %s,%s,lo16(%s)" regl reg pic )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Sxlo23.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Sxlo23.Camlburg.action ()
                                    in
                                        let pic = _v1
                                        in
                                            
# 237 "ppcrec.mlb"
                                            ( s "addi %s,0,lo16(%s)" regl pic )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add30.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add30.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 259 "ppcrec.mlb"
                                            ( s "add %s,%s,%s" regl x y )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Add31.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Add31.Camlburg.action ()
                                    in
                                        let (x, y) = _v1
                                        in
                                            
# 260 "ppcrec.mlb"
                                            ( s "addi %s,%s,%s" regl x y )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Unop32.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and _v1 = arg2._Unop32.Camlburg.action ()
                                    in
                                        let (opr, x) = _v1
                                        in
                                            
# 262 "ppcrec.mlb"
                                            ( s "%s  %s,%s"    opr regl x )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Binop33.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and
                                        _v1 =
                                        arg2._Binop33.Camlburg.action ()
                                    in
                                        let (opr, x, y) = _v1
                                        in
                                            
# 263 "ppcrec.mlb"
                                            ( s "%s  %s,%s,%s" opr regl x y )
                                            
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                (arg1.regl.Camlburg.cost
                                +
                                arg2._Binop34.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg3)
                            ;Camlburg.action =
                                (fun () ->
                                    let regl = arg1.regl.Camlburg.action ()
                                    and
                                        _v1 =
                                        arg2._Binop34.Camlburg.action ()
                                    in
                                        let (opr, x, y) = _v1
                                        in
                                            
# 265 "ppcrec.mlb"
                                            ( s "%si %s,%s,%s" opr regl x y )
                                            
# 000 "/dev/stdout"
)
                            }]))
                        inf)))
and conSlice =
    fun arg1 arg2 arg3 ->
        (update_any
            {Camlburg.cost = (arg3.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let w = arg1
                    and n = arg2
                    and y = arg3.any.Camlburg.action ()
                    in
                        
# 294 "ppcrec.mlb"
                        ( sprintf "Slice(%d, %d, %s)" w n y )
                        
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
                        
# 297 "ppcrec.mlb"
                        ( sprintf "Reg(%s, %d)" (Char.escaped char) n )
                        
# 000 "/dev/stdout"
)
            })
            ((update_cial
                {Camlburg.cost =
                    ((Camlburg.matches 'c') arg1 + (Camlburg.matches 1) arg2)
                ;Camlburg.action =
                    (fun () ->
                        
# 187 "ppcrec.mlb"
                        ( () )
                        
# 000 "/dev/stdout"
)
                })
                ((update_crl
                    {Camlburg.cost =
                        ((Camlburg.matches 'c') arg1
                        +
                        (Camlburg.matches 2) arg2)
                    ;Camlburg.action =
                        (fun () ->
                            
# 188 "ppcrec.mlb"
                            ( () )
                            
# 000 "/dev/stdout"
)
                    })
                    ((update_lrl
                        {Camlburg.cost =
                            ((Camlburg.matches 'c') arg1
                            +
                            (Camlburg.matches 5) arg2)
                        ;Camlburg.action =
                            (fun () ->
                                
# 190 "ppcrec.mlb"
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
                                    
# 186 "ppcrec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                            })
                            ((update_regl
                                {Camlburg.cost =
                                    (let n = arg2
                                    in
                                        
# 199 "ppcrec.mlb"
                                        ( guard (n<>0) )
                                        
# 000 "/dev/stdout"

                                    +
                                    (Camlburg.matches 'r') arg1)
                                ;Camlburg.action =
                                    (fun () ->
                                        let n = arg2
                                        in
                                            
# 199 "ppcrec.mlb"
                                            ( s "r%d" n )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_spl
                                    {Camlburg.cost =
                                        ((Camlburg.matches 'r') arg1
                                        +
                                        (Camlburg.matches 14) arg2)
                                    ;Camlburg.action =
                                        (fun () ->
                                            
# 191 "ppcrec.mlb"
                                            ( () )
                                            
# 000 "/dev/stdout"
)
                                    })
                                    ((update_xerl
                                        {Camlburg.cost =
                                            ((Camlburg.matches 'c') arg1
                                            +
                                            (Camlburg.matches 4) arg2)
                                        ;Camlburg.action =
                                            (fun () ->
                                                
# 189 "ppcrec.mlb"
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
                        
# 304 "ppcrec.mlb"
                        ( s "Par(%s,%s)" l r )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1._Goto24.Camlburg.cost
                        +
                        arg2._Store25.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto24.Camlburg.action ()
                            and _v2 = arg2._Store25.Camlburg.action ()
                            in
                                let (regl, reg, w) = _v2
                                in
                                    let lr = _v1
                                    in
                                        
# 244 "ppcrec.mlb"
                                        ( sprintf "mr %s, %s; blr" regl reg )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto24.Camlburg.cost
                        +
                        arg2._Store26.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto24.Camlburg.action ()
                            and _v2 = arg2._Store26.Camlburg.action ()
                            in
                                let (lrl, next) = _v2
                                in
                                    let lr = _v1
                                    in
                                        
# 247 "ppcrec.mlb"
                                        ( "blrl" )
                                        
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Goto27.Camlburg.cost
                        +
                        arg2._Store26.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Goto27.Camlburg.action ()
                            and _v2 = arg2._Store26.Camlburg.action ()
                            in
                                let (lrl, next) = _v2
                                in
                                    let lconst = _v1
                                    in
                                        
# 248 "ppcrec.mlb"
                                        ( s "bl %s" (ind_addr lconst) )
                                        
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conOvSet =
    fun arg1 ->
        (update__OvSet28
            {Camlburg.cost = (arg1._Fetch29.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Fetch29.Camlburg.action ()
                    in
                        let xerl = _v1 in xerl)
            })
            inf
and conNop =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 289 "ppcrec.mlb"
                    ( "nop" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conMem =
    fun arg1 ->
        (update__Mem1
            {Camlburg.cost = (arg1._Diff2.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Diff2.Camlburg.action ()
                    in
                        let (c1, c2) = _v1 in (c1 ,c2))
            })
            ((update__Mem4
                {Camlburg.cost = (arg1.addr.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let addr = arg1.addr.Camlburg.action () in addr)
                })
                ((update__Mem6
                    {Camlburg.cost = (arg1.ndx_addr.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let ndx_addr = arg1.ndx_addr.Camlburg.action ()
                            in
                                ndx_addr)
                    })
                    ((update_any
                        {Camlburg.cost = (arg1.any.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let any = arg1.any.Camlburg.action ()
                                in
                                    
# 296 "ppcrec.mlb"
                                    ( s "Mem(%s)" any )
                                    
# 000 "/dev/stdout"
)
                        })
                        inf)))
and conLobits =
    fun arg1 arg2 ->
        (update__Lobits19
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + (Camlburg.matches 8) arg2)
            ;Camlburg.action =
                (fun () -> let reg = arg1.reg.Camlburg.action () in reg)
            })
            ((update__Lobits20
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + (Camlburg.matches 16) arg2)
                ;Camlburg.action =
                    (fun () -> let reg = arg1.reg.Camlburg.action () in reg)
                })
                ((update_any
                    {Camlburg.cost = (arg1.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let any = arg1.any.Camlburg.action ()
                            and w = arg2
                            in
                                
# 291 "ppcrec.mlb"
                                ( s "Lobits(%s, %d)" any w )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conLink =
    fun arg1 arg2 ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let symbol = arg1
                    and w = arg2
                    in
                        
# 271 "ppcrec.mlb"
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
                            
# 182 "ppcrec.mlb"
                            ( symbol#mangled_text )
                            
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
                        
# 300 "ppcrec.mlb"
                        ( s "Kill(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conHa16 =
    fun arg1 ->
        (update_any
            {Camlburg.cost = (arg1.any.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let any = arg1.any.Camlburg.action ()
                    in
                        
# 283 "ppcrec.mlb"
                        ( s "Ha16(%s)" any )
                        
# 000 "/dev/stdout"
)
            })
            ((update_ha16
                {Camlburg.cost = (arg1.pic.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let pic = arg1.pic.Camlburg.action ()
                        in
                            
# 239 "ppcrec.mlb"
                            ( s "ha16(%s)" pic )
                            
# 000 "/dev/stdout"
)
                })
                inf)
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
                        
# 302 "ppcrec.mlb"
                        ( s "Guarded(%s,%s)" guard any )
                        
# 000 "/dev/stdout"
)
            })
            ((update_inst
                (Camlburg.choice
                    [{Camlburg.cost =
                        (arg1.cmp.Camlburg.cost + arg2._Goto27.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let cmp = arg1.cmp.Camlburg.action ()
                            and _v1 = arg2._Goto27.Camlburg.action ()
                            in
                                let lconst = _v1
                                in
                                    
# 253 "ppcrec.mlb"
                                    ( let (i_, (l_, op), x, y) = cmp in
          s "cmp%sw%s cr0,%s,%s\n\tb%s %s"  l_ i_ x y op lconst
       )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._OvSet28.Camlburg.cost
                        +
                        arg2._Goto27.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._OvSet28.Camlburg.action ()
                            and _v2 = arg2._Goto27.Camlburg.action ()
                            in
                                let lconst = _v2
                                in
                                    let xerl = _v1
                                    in
                                        
# 257 "ppcrec.mlb"
                                        ( s "bo %s" lconst )
                                        
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conGoto =
    fun arg1 ->
        (update__Goto24
            {Camlburg.cost = (arg1.lr.Camlburg.cost)
            ;Camlburg.action =
                (fun () -> let lr = arg1.lr.Camlburg.action () in lr)
            })
            ((update__Goto27
                {Camlburg.cost = (arg1.lconst.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let lconst = arg1.lconst.Camlburg.action ()
                        in
                            lconst)
                })
                ((update_any
                    {Camlburg.cost = (arg1.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let any = arg1.any.Camlburg.action ()
                            in
                                
# 305 "ppcrec.mlb"
                                ( s "Goto(%s)" any )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_inst
                        (Camlburg.choice
                            [{Camlburg.cost = (arg1.lconst.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let
                                        lconst =
                                        arg1.lconst.Camlburg.action ()
                                    in
                                        
# 241 "ppcrec.mlb"
                                        ( s "b %s" (ind_addr lconst) )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost = (arg1.lr.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let lr = arg1.lr.Camlburg.action ()
                                    in
                                        
# 242 "ppcrec.mlb"
                                        ( "blr"                      )
                                        
# 000 "/dev/stdout"
)
                            }]))
                        inf)))
and conFetch =
    fun arg1 arg2 ->
        (update__Fetch10
            {Camlburg.cost =
                (arg1._Mem6.Camlburg.cost + (Camlburg.matches 8) arg2)
            ;Camlburg.action =
                (fun () ->
                    let _v1 = arg1._Mem6.Camlburg.action ()
                    in
                        let ndx_addr = _v1 in ndx_addr)
            })
            ((update__Fetch12
                {Camlburg.cost =
                    (arg1._Mem4.Camlburg.cost + (Camlburg.matches 16) arg2)
                ;Camlburg.action =
                    (fun () ->
                        let _v1 = arg1._Mem4.Camlburg.action ()
                        in
                            let addr = _v1 in addr)
                })
                ((update__Fetch14
                    {Camlburg.cost =
                        (arg1._Mem6.Camlburg.cost
                        +
                        (Camlburg.matches 16) arg2)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Mem6.Camlburg.action ()
                            in
                                let ndx_addr = _v1 in ndx_addr)
                    })
                    ((update__Fetch29
                        {Camlburg.cost =
                            (arg1.xerl.Camlburg.cost
                            +
                            (Camlburg.matches 32) arg2)
                        ;Camlburg.action =
                            (fun () ->
                                let xerl = arg1.xerl.Camlburg.action ()
                                in
                                    xerl)
                        })
                        ((update__Fetch3
                            {Camlburg.cost =
                                (arg1._Mem4.Camlburg.cost
                                +
                                (Camlburg.matches 32) arg2)
                            ;Camlburg.action =
                                (fun () ->
                                    let _v1 = arg1._Mem4.Camlburg.action ()
                                    in
                                        let addr = _v1 in addr)
                            })
                            ((update__Fetch5
                                {Camlburg.cost =
                                    (arg1._Mem6.Camlburg.cost
                                    +
                                    (Camlburg.matches 32) arg2)
                                ;Camlburg.action =
                                    (fun () ->
                                        let
                                            _v1 =
                                            arg1._Mem6.Camlburg.action ()
                                        in
                                            let ndx_addr = _v1 in ndx_addr)
                                })
                                ((update__Fetch8
                                    {Camlburg.cost =
                                        (arg1._Mem4.Camlburg.cost
                                        +
                                        (Camlburg.matches 8) arg2)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                _v1 =
                                                arg1._Mem4.Camlburg.action ()
                                            in
                                                let addr = _v1 in addr)
                                    })
                                    ((update_any
                                        {Camlburg.cost =
                                            (arg1.any.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    any =
                                                    arg1.any.Camlburg.action
                                                        ()
                                                and w = arg2
                                                in
                                                    
# 275 "ppcrec.mlb"
                                                    ( s "Fetch(%s,%d)" any w )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        ((update_cia
                                            {Camlburg.cost =
                                                (arg1.cial.Camlburg.cost
                                                +
                                                (Camlburg.matches 32) arg2)
                                            ;Camlburg.action =
                                                (fun () ->
                                                    let
                                                        cial =
                                                        arg1.cial.Camlburg.action
                                                            ()
                                                    in
                                                        
# 194 "ppcrec.mlb"
                                                        ( () )
                                                        
# 000 "/dev/stdout"
)
                                            })
                                            ((update_cr
                                                {Camlburg.cost =
                                                    (arg1.crl.Camlburg.cost
                                                    +
                                                    (Camlburg.matches 32)
                                                        arg2)
                                                ;Camlburg.action =
                                                    (fun () ->
                                                        let
                                                            crl =
                                                            arg1.crl.Camlburg.action
                                                                ()
                                                        in
                                                            
# 195 "ppcrec.mlb"
                                                            ( () )
                                                            
# 000 "/dev/stdout"
)
                                                })
                                                ((update_lr
                                                    {Camlburg.cost =
                                                        (arg1.lrl.Camlburg.cost
                                                        +
                                                        (Camlburg.matches 32)
                                                            arg2)
                                                    ;Camlburg.action =
                                                        (fun () ->
                                                            let
                                                                lrl =
                                                                arg1.lrl.Camlburg.action
                                                                    ()
                                                            in
                                                                
# 196 "ppcrec.mlb"
                                                                ( () )
                                                                
# 000 "/dev/stdout"
)
                                                    })
                                                    ((update_pc
                                                        {Camlburg.cost =
                                                            (arg1.pcl.Camlburg.cost
                                                            +
                                                            (Camlburg.matches
                                                                32)
                                                                arg2)
                                                        ;Camlburg.action =
                                                            (fun () ->
                                                                let
                                                                    pcl =
                                                                    arg1.pcl.Camlburg.action
                                                                        ()
                                                                in
                                                                    
# 193 "ppcrec.mlb"
                                                                    ( () )
                                                                    
# 000 "/dev/stdout"
)
                                                        })
                                                        ((update_pic
                                                            {Camlburg.cost =
                                                                (arg1._Mem1.Camlburg.cost)
                                                            ;Camlburg.action =
                                                                (fun () ->
                                                                    let
                                                                        _v1 =
                                                                        arg1._Mem1.Camlburg.action
                                                                            ()
                                                                    and
                                                                        w2 =
                                                                        arg2
                                                                    in
                                                                        let
                                                                            (c1,
                                                                            c2) =
                                                                            _v1
                                                                        in
                                                                            
# 184 "ppcrec.mlb"
                                                                            ( s "%s-%s" c1 c2 )
                                                                            
# 000 "/dev/stdout"
)
                                                            })
                                                            ((update_reg
                                                                {Camlburg.cost =
                                                                    (arg1.regl.Camlburg.cost
                                                                    +
                                                                    (Camlburg.matches
                                                                        32)
                                                                        arg2)
                                                                ;Camlburg.action =
                                                                    (fun
                                                                    ()
                                                                    ->
                                                                        let
                                                                            regl =
                                                                            arg1.regl.Camlburg.action
                                                                                ()
                                                                        in
                                                                            
# 200 "ppcrec.mlb"
                                                                            ( regl )
                                                                            
# 000 "/dev/stdout"
)
                                                                })
                                                                ((update_sp
                                                                    {Camlburg.cost =
                                                                        (arg1.spl.Camlburg.cost
                                                                        +
                                                                        (Camlburg.matches
                                                                            32)
                                                                            arg2)
                                                                    ;Camlburg.action =
                                                                        (fun
                                                                        ()
                                                                        ->
                                                                            let
                                                                                spl =
                                                                                arg1.spl.Camlburg.action
                                                                                    ()
                                                                            in
                                                                                
# 197 "ppcrec.mlb"
                                                                                ( () )
                                                                                
# 000 "/dev/stdout"
)
                                                                    })
                                                                    inf))))))))))))))
and conFalse =
    fun () ->
        (update_any
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 270 "ppcrec.mlb"
                    ( "False" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conDiff =
    fun arg1 arg2 ->
        (update__Diff2
            {Camlburg.cost =
                (arg1.lconst.Camlburg.cost + arg2.lconst.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let c1 = arg1.lconst.Camlburg.action ()
                    and c2 = arg2.lconst.Camlburg.action ()
                    in
                        (c1 ,c2))
            })
            ((update_any
                {Camlburg.cost =
                    (arg1.any.Camlburg.cost + arg2.any.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let c1 = arg1.any.Camlburg.action ()
                        and c2 = arg2.any.Camlburg.action ()
                        in
                            
# 272 "ppcrec.mlb"
                            ( s "Diff(%s, %s)" c1 c2 )
                            
# 000 "/dev/stdout"
)
                })
                ((update_pic
                    {Camlburg.cost =
                        (arg1.lconst.Camlburg.cost
                        +
                        arg2.lconst.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let c1 = arg1.lconst.Camlburg.action ()
                            and c2 = arg2.lconst.Camlburg.action ()
                            in
                                
# 183 "ppcrec.mlb"
                                ( s "%s-%s" c1 c2 )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
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
                        
# 303 "ppcrec.mlb"
                        ( s "Cmp(%s,%s,%s)" op x y )
                        
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
                                
# 250 "ppcrec.mlb"
                                ( ("",  ppc_op op,x,y) )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg2.reg.Camlburg.cost + arg3.const16.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let op = arg1
                            and x = arg2.reg.Camlburg.action ()
                            and y = arg3.const16.Camlburg.action ()
                            in
                                
# 251 "ppcrec.mlb"
                                ( ("i", ppc_op op,x,y) )
                                
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
                        
# 273 "ppcrec.mlb"
                        ( sprintf "Bits(%s)" (Bits.to_string bits) )
                        
# 000 "/dev/stdout"
)
            })
            ((update_const16
                {Camlburg.cost =
                    (let bits = arg1
                    in
                        
# 164 "ppcrec.mlb"
                        ( guard (Bits.S.fits 16 bits) )
                        
# 000 "/dev/stdout"
)
                ;Camlburg.action =
                    (fun () ->
                        let bits = arg1
                        in
                            
# 165 "ppcrec.mlb"
                            ( Bits.to_decimal_string bits )
                            
# 000 "/dev/stdout"
)
                })
                ((update_k15
                    {Camlburg.cost =
                        (let bits = arg1
                        in
                            
# 168 "ppcrec.mlb"
                            ( guard (Bits.width bits > 5 &&
                   Bits.eq bits (Bits.U.of_int 15 (Bits.width bits))) )
                            
# 000 "/dev/stdout"
)
                    ;Camlburg.action =
                        (fun () ->
                            let bits = arg1
                            in
                                
# 170 "ppcrec.mlb"
                                ( () )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_k16
                        {Camlburg.cost =
                            (let bits = arg1
                            in
                                
# 173 "ppcrec.mlb"
                                ( guard (Bits.width bits > 5 &&
                   Bits.eq bits (Bits.U.of_int 16 (Bits.width bits))) )
                                
# 000 "/dev/stdout"
)
                        ;Camlburg.action =
                            (fun () ->
                                let bits = arg1
                                in
                                    
# 175 "ppcrec.mlb"
                                    ( () )
                                    
# 000 "/dev/stdout"
)
                        })
                        ((update_k4
                            {Camlburg.cost =
                                (let bits = arg1
                                in
                                    
# 178 "ppcrec.mlb"
                                    ( guard (Bits.width bits > 5 &&
                   Bits.eq bits (Bits.U.of_int 4 (Bits.width bits))) )
                                    
# 000 "/dev/stdout"
)
                            ;Camlburg.action =
                                (fun () ->
                                    let bits = arg1
                                    in
                                        
# 180 "ppcrec.mlb"
                                        ( () )
                                        
# 000 "/dev/stdout"
)
                            })
                            inf))))
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
                        
# 292 "ppcrec.mlb"
                        ( sprintf "BitExtract(%s, %s, %d)" lsb y n )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conBinop =
    fun arg1 arg2 arg3 ->
        (update__Binop33
            {Camlburg.cost =
                (arg2.reg.Camlburg.cost + arg3.reg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let opr = arg1
                    and x = arg2.reg.Camlburg.action ()
                    and y = arg3.reg.Camlburg.action ()
                    in
                        (opr ,x ,y))
            })
            ((update__Binop34
                {Camlburg.cost =
                    (arg2.reg.Camlburg.cost + arg3.const16.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let opr = arg1
                        and x = arg2.reg.Camlburg.action ()
                        and y = arg3.const16.Camlburg.action ()
                        in
                            (opr ,x ,y))
                })
                ((update_any
                    {Camlburg.cost =
                        (arg2.any.Camlburg.cost + arg3.any.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let op = arg1
                            and x = arg2.any.Camlburg.action ()
                            and y = arg3.any.Camlburg.action ()
                            in
                                
# 287 "ppcrec.mlb"
                                ( s "Binop(%s,%s,%s)" op x y  )
                                
# 000 "/dev/stdout"
)
                    })
                    inf))
and conAdd =
    fun arg1 arg2 ->
        (update__Add21
            {Camlburg.cost =
                (arg1.reg.Camlburg.cost + arg2.ha16.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let reg = arg1.reg.Camlburg.action ()
                    and ha16 = arg2.ha16.Camlburg.action ()
                    in
                        (reg ,ha16))
            })
            ((update__Add22
                {Camlburg.cost =
                    (arg1.reg.Camlburg.cost + arg2._Sxlo23.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let reg = arg1.reg.Camlburg.action ()
                        and _v1 = arg2._Sxlo23.Camlburg.action ()
                        in
                            let pic = _v1 in (reg ,pic))
                })
                ((update__Add30
                    {Camlburg.cost =
                        (arg1.reg.Camlburg.cost + arg2.reg.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.reg.Camlburg.action ()
                            and y = arg2.reg.Camlburg.action ()
                            in
                                (x ,y))
                    })
                    ((update__Add31
                        {Camlburg.cost =
                            (arg1.reg.Camlburg.cost
                            +
                            arg2.const16.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let x = arg1.reg.Camlburg.action ()
                                and y = arg2.const16.Camlburg.action ()
                                in
                                    (x ,y))
                        })
                        ((update_addr
                            {Camlburg.cost =
                                (arg1.reg.Camlburg.cost
                                +
                                arg2.const16.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let reg = arg1.reg.Camlburg.action ()
                                    and
                                        const16 =
                                        arg2.const16.Camlburg.action ()
                                    in
                                        
# 204 "ppcrec.mlb"
                                        ( s "%s(%s)" const16 reg )
                                        
# 000 "/dev/stdout"
)
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
                                            
# 281 "ppcrec.mlb"
                                            ( s "Add(%s, %s)" x y )
                                            
# 000 "/dev/stdout"
)
                                })
                                ((update_ndx_addr
                                    {Camlburg.cost =
                                        (arg1.reg.Camlburg.cost
                                        +
                                        arg2.reg.Camlburg.cost)
                                    ;Camlburg.action =
                                        (fun () ->
                                            let
                                                reg1 =
                                                arg1.reg.Camlburg.action ()
                                            and
                                                reg2 =
                                                arg2.reg.Camlburg.action ()
                                            in
                                                
# 206 "ppcrec.mlb"
                                                ( s "%s,%s" reg1 reg2 )
                                                
# 000 "/dev/stdout"
)
                                    })
                                    ((update_next
                                        {Camlburg.cost =
                                            (arg1.cia.Camlburg.cost
                                            +
                                            arg2.k4.Camlburg.cost)
                                        ;Camlburg.action =
                                            (fun () ->
                                                let
                                                    cia =
                                                    arg1.cia.Camlburg.action
                                                        ()
                                                and
                                                    k4 =
                                                    arg2.k4.Camlburg.action
                                                        ()
                                                in
                                                    
# 246 "ppcrec.mlb"
                                                    ( () )
                                                    
# 000 "/dev/stdout"
)
                                        })
                                        inf)))))))



# 36 "ppcrec.mlb"
   
    (*s: code to follow the labeler *)
    let rec const = function
      | RP.Bool(true)             -> conTrue  ()
      | RP.Bool(false)            -> conFalse ()
      | RP.Link(s,_,w)            -> conLink s w
      | RP.Diff(c1,c2)            -> conDiff (const c1) (const c2)
      | RP.Late(s,w)              -> Impossible.impossible "Late constant in recognizer"
      | RP.Bits(b)                -> conBits(b)
    (*x: code to follow the labeler *)
    let is_cmp (opr,ws) =
      let cmp = ["eq";"ge";"geu";"gt";"gtu";"le";"leu";"lt";"ltu";"ne"] in
      if not (List.mem opr cmp) then false
      else match ws with
        [32]  -> true
      | _     -> error "comparison not at 32 bits in PPC recognizer"

    let rtl2ppc = function
      | "and"  -> "and"
      | "divu" -> "divwu"
      | "quot" -> "divw"
      | "mul"  -> "mullw"
      | "neg"  -> "neg"
      | "or"   -> "or"
      | "shl"  -> "slw"
      | "shrl" -> "srw"
      | "sub"  -> "sub"
      | "xor"  -> "xor"
      | opr    -> error (sprintf "Unsupported RTL operator \"%s\"" opr)

    let rec exp = function
      | RP.Const(k)                  -> const (k)
      | RP.Fetch(l,w)                -> conFetch (loc l) w
      (*s: case for [[ha16(e)]] *)
      | RP.App (("shl", [32]), [
         (*s: pic_hi16 + pic_15 *)
         RP.App(("add", [32]), [
           (*s: pic_hi16 *)
           RP.App(("shrl", [32]), [e; RP.Const (RP.Bits k16')])
           (*e: pic_hi16 *)
           ; 
           (*s: pic15 *)
           RP.App(("zx", [1;32]),
                  [RP.App (("lobits", [32;1]),
                           [RP.App(("shrl", [32]), [e'; RP.Const (RP.Bits k15)])])])
           (*e: pic15 *)
           ])
         (*e: pic_hi16 + pic_15 *)
         ; RP.Const (RP.Bits k16)]) 
         when Bits.eq k16 (Bits.U.of_int 16 32) && Bits.eq k16' (Bits.U.of_int 16 32)
           && Bits.eq k15 (Bits.U.of_int 15 32) && RU.Eq.exp e e' ->
           conHa16 (exp e)
      (*e: case for [[ha16(e)]] *)
      | RP.App(("sx", [n; _]), [RP.App (("lobits", [_;_]), [x])]) -> conSxlo (exp x) n
      | RP.App(("zx", [n; _]), [RP.App (("lobits", [_;_]), [x])]) -> conZxlo (exp x) n
      | RP.App(("sx", [8 ;32]), [x]) -> conSx (exp x)
      | RP.App(("sx", [16;32]), [x]) -> conSx (exp x)
      | RP.App(("zx", [8 ;32]), [x]) -> conZx (exp x)
      | RP.App(("zx", [16;32]), [x]) -> conZx (exp x)
      | RP.App(("add",[16]), [x; y]) -> conAdd  (exp x) (exp y)
      | RP.App(("add",[32]), [x; y]) -> conAdd  (exp x) (exp y)


      | RP.App(("ppc_xer_ov_set", []), [x])        -> conOvSet (exp x)
      | RP.App(("bitExtract", [_; n]), [lsb; src]) -> conBitExtract (exp lsb) (exp src) n

      | RP.App(("lobits", [32;w]), [x]) -> conLobits (exp x) w

      | RP.App((opr, ws), [x])          -> conUnop (rtl2ppc opr) (exp x)
      | RP.App((opr, ws), [x;y])        -> if is_cmp(opr,ws) then conCmp opr (exp x) (exp y)
                                           else conBinop (rtl2ppc opr) (exp x) (exp y)

      | RP.App((o,_),_) -> error (sprintf "unknown operator %s" o)
    (*x: code to follow the labeler *)
    and loc l = match l with
      | RP.Mem(('m',_,_), Rtl.C c, e, ass) -> conMem (exp e)
      | RP.Reg((sp, _,_), i, w)            -> conReg sp i 
      | RP.Mem(_, _, _, _)                 -> error "non-mem, non-reg cell"
      | RP.Var _ | RP.Global _             -> error "var found"
      | RP.Slice(w,i,l)                    -> conSlice w i (loc l)

    and effect = function
      | RP.Store(RP.Reg(('c',_,_),i,_),r,_)
        when i = SS.indices.SS.pc          -> conGoto (exp r)
      | RP.Store(l,e,w)                    -> conStore (loc l) (exp e) w
      | RP.Kill(l)                         -> conKill (loc l)

    and guarded g eff =
      match g with
      | RP.Const(RP.Bool b) -> if b then effect eff else conNop()
      | _                   -> conGuarded (exp g) (effect eff)

    and geffects = function
        | []          -> conNop()
        | [g, s]      -> guarded g s
        | (g, s) :: t -> conPar (guarded g s) (geffects t)
    and rtl (RP.Rtl es) = geffects es
    (*x: code to follow the labeler *)
    let errmsg r msg =
      List.iter prerr_string
        [ "recognizer error: "; msg; " on "; RU.ToString.rtl r; "\n" ]

    let to_asm r i =
      try
        let _ = imports := i in
        let plan = rtl (Down.rtl r) in
        plan.inst.Camlburg.action ()
      with 
      | Camlburg.Uncovered -> " not an instruction: " ^ RU.ToString.rtl r
      | Error msg -> (errmsg r msg; " error in recognizer: " ^ msg)

    let is_instruction r =
      try
        let plan = rtl (Down.rtl r) in
        plan.inst.Camlburg.cost < 100
      with
      | Camlburg.Uncovered -> false
      | Error msg -> (errmsg r msg; false)
    (*e: code to follow the labeler *)
         end (* of M *) 
      

# 000 "/dev/stdout"
