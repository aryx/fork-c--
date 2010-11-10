
# 2 "dummyexpander.mlb"
 
 (*s: utilities *)
 module R  = Rtl
 module RP = Rtl.Private
 module RU = Rtlutil
 module S  = Space
 module T  = Target
 module C  = Camlburg
 module D  = Rtl.Dn      (* Convert Down to private repr. *)
 module U  = Rtl.Up       (* Convert Up    to abstract repr. *)
 module W  = Rtlutil.Width 
 module G  = Cfg             (* for expanding all RTLs in a CFG *)
 module GU = Cfgutil
 (*x: utilities *)
 exception Error of string
 let error msg = raise (Error msg)
 (*x: utilities *)
 type state = R.rtl list * Talloc.Multiple.t
 type 'a m  = state -> ('a * state)
 (*x: utilities *)
 let exec  rtl  = fun (rtls,tmp) -> ((), (rtl :: rtls,tmp)) 
 (*x: utilities *)
 let return a  = fun s -> (a, s)
 let (>>=) (m: 'a m) (f: 'a -> 'b m) = fun s -> let (a,s) = m s in f a s 
 (*x: utilities *)
 let dtmp = function
     | 2 -> fun(rtls, tmp as state) -> (R.reg ('d', 1, 2), state)
     | w  -> Impossible.impossible 
                 (Printf.sprintf "unsupported width for d-space register: %d" w)

 let itmp = function
     | 32 -> fun (rtls,tmp as state) -> (Talloc.Multiple.loc tmp 't' 32, state)
     (*
     | 8  -> fun (rtls,tmp as state) -> (Talloc.Multiple.loc tmp 'v'  8, state)
     *)
     | 8  -> fun (rtls,tmp as state) -> (Talloc.Multiple.loc tmp 't'  32, state)
     | w  -> Impossible.impossible 
                 (Printf.sprintf "unsupported width for int register: %d" w)
 let ftmp = function
     | 64 -> fun (rtls,tmp as state) -> (Talloc.Multiple.loc tmp 'u' 32, state)
     | 32 -> fun (rtls,tmp as state) -> (Talloc.Multiple.loc tmp 'u' 32, state)
     | 8  -> fun (rtls,tmp as state) -> (Talloc.Multiple.loc tmp 'u' 32, state)
     | w  -> Impossible.impossible 
                 (Printf.sprintf "unsupported width for float register: %d" w)
 (*x: utilities *)
 module StringSet = Set.Make(struct type t = string let compare = compare end)
 (*x: utilities *)
 let bool_ops2 = (* boolean valued primitive with non-boolean args *)
     List.fold_right StringSet.add
         ["eq"; "ge"; "geu"; "gt"; "gtu"; "le"; "leu"; "lt"; "ltu"; "ne"]
          StringSet.empty

 let is_bool_op2  op     = StringSet.mem op bool_ops2
 let is_bool_op1  op     = op = "bool"
 (*x: utilities *)
 let fetch t = R.fetch t (W.loc t)
 (*x: utilities *)
 let bits2 n = R.bits (Bits.U.of_int n 2) 2

 let app0 tmp s x =
     let opr   = R.opr s x in
     let exp   = R.app opr [] in
     let width = W.exp exp in
     tmp width >>= fun t -> 
     (*
     exec (R.store t exp width) >>= fun () ->
     *)
     return t

 let app1 tmp s x arg1 =
     arg1 >>= fun a1 ->
     let opr   = R.opr s x in
     let exp   = R.app opr [fetch a1] in
     let width = W.exp exp in
     tmp width >>= fun t ->
     exec (R.store t exp width) >>= fun () ->
     return t

 let app2 tmp s x arg1 arg2 =
     arg1 >>= fun a1 -> arg2 >>= fun a2 ->
     let opr   = R.opr s x in
     let exp   = R.app opr [fetch a1; fetch a2] in
     let width = W.exp exp in
     tmp width >>= fun t ->
     exec (R.store t exp width) >>= fun () ->
     return t

 let app3 tmp s x arg1 arg2 arg3 =
     arg1 >>= fun a1 -> arg2 >>= fun a2 -> arg3 >>= fun a3 ->
     let opr   = R.opr s x in
     let exp   = R.app opr [fetch a1; fetch a2; fetch a3] in
     let width = W.exp exp in
     tmp width >>= fun t ->
     exec (R.store t exp width) >>= fun () ->
     return t
 (*x: utilities *)
 let mem sp agg width addr ass=
     addr >>= fun a -> return (R.mem (U.assertion ass) sp agg width a)

 let reg sp i width = return (R.reg (sp, i, width))
 (*x: utilities *)
 let store left right width =
     left >>= fun l -> right >>= fun r -> 
     return (R.store l (R.fetch r width) width)

 let store' left right width =   (* right is a value, not a location *)
     left >>= fun l -> right >>= fun r -> 
     return (R.store l r width)
 (*e: utilities *)
 

# 000 "/dev/stdout"


type
    (
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
        stmt: ( 't15 ) Camlburg.nt;
        rtl: ( 't14 ) Camlburg.nt;
        mem: ( 't13 ) Camlburg.nt;
        ireg: ( 't12 ) Camlburg.nt;
        icell: ( 't11 ) Camlburg.nt;
        guard: ( 't10 ) Camlburg.nt;
        gstmts: ( 't9 ) Camlburg.nt;
        gstmt: ( 't8 ) Camlburg.nt;
        freg: ( 't7 ) Camlburg.nt;
        fcell: ( 't6 ) Camlburg.nt;
        error: ( 't5 ) Camlburg.nt;
        dreg: ( 't4 ) Camlburg.nt;
        dcell: ( 't3 ) Camlburg.nt;
        const: ( 't2 ) Camlburg.nt;
        ccell: ( 't1 ) Camlburg.nt;
        addr: ( 't0 ) Camlburg.nt
    }

let rec
inf =
    {addr = (Camlburg.infinity)
    ;ccell = (Camlburg.infinity)
    ;const = (Camlburg.infinity)
    ;dcell = (Camlburg.infinity)
    ;dreg = (Camlburg.infinity)
    ;error = (Camlburg.infinity)
    ;fcell = (Camlburg.infinity)
    ;freg = (Camlburg.infinity)
    ;gstmt = (Camlburg.infinity)
    ;gstmts = (Camlburg.infinity)
    ;guard = (Camlburg.infinity)
    ;icell = (Camlburg.infinity)
    ;ireg = (Camlburg.infinity)
    ;mem = (Camlburg.infinity)
    ;rtl = (Camlburg.infinity)
    ;stmt = (Camlburg.infinity)
    }


let rec
update_addr =
    fun nt x ->
        if nt.Camlburg.cost >= x.addr.Camlburg.cost then
            x
        else
            { x with addr = (nt) }
and update_ccell =
    fun nt x ->
        if nt.Camlburg.cost >= x.ccell.Camlburg.cost then
            x
        else
            { x with ccell = (nt) }
and update_const =
    fun nt x ->
        if nt.Camlburg.cost >= x.const.Camlburg.cost then
            x
        else
            (fun x ->
                (update_ireg
                    {Camlburg.cost = (nt.Camlburg.cost + 1)
                    ;Camlburg.action =
                        (fun () ->
                            let const = x.const.Camlburg.action ()
                            in
                                
# 205 "dummyexpander.mlb"
                                (  const                                >>= fun k ->
                itmp (W.exp k)                       >>= fun t  ->
                exec (R.store t k (W.exp k))         >>= fun () ->
                return t
            )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with const = (nt) }
and update_dcell =
    fun nt x ->
        if nt.Camlburg.cost >= x.dcell.Camlburg.cost then
            x
        else
            { x with dcell = (nt) }
and update_dreg =
    fun nt x ->
        if nt.Camlburg.cost >= x.dreg.Camlburg.cost then
            x
        else
            { x with dreg = (nt) }
and update_error =
    fun nt x ->
        if nt.Camlburg.cost >= x.error.Camlburg.cost then
            x
        else
            { x with error = (nt) }
and update_fcell =
    fun nt x ->
        if nt.Camlburg.cost >= x.fcell.Camlburg.cost then
            x
        else
            { x with fcell = (nt) }
and update_freg =
    fun nt x ->
        if nt.Camlburg.cost >= x.freg.Camlburg.cost then
            x
        else
            (fun x ->
                (update_ireg
                    {Camlburg.cost = (nt.Camlburg.cost + 10)
                    ;Camlburg.action =
                        (fun () ->
                            let freg = x.freg.Camlburg.action ()
                            in
                                
# 316 "dummyexpander.mlb"
                                (  
                freg                                >>= fun u  ->
                let w = W.loc u in 
                itmp w                              >>= fun t  ->
                exec (R.store t (R.fetch u w) w)    >>= fun () ->
                return t
            )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with freg = (nt) }
and update_gstmt =
    fun nt x ->
        if nt.Camlburg.cost >= x.gstmt.Camlburg.cost then
            x
        else
            { x with gstmt = (nt) }
and update_gstmts =
    fun nt x ->
        if nt.Camlburg.cost >= x.gstmts.Camlburg.cost then
            x
        else
            { x with gstmts = (nt) }
and update_guard =
    fun nt x ->
        if nt.Camlburg.cost >= x.guard.Camlburg.cost then
            x
        else
            { x with guard = (nt) }
and update_icell =
    fun nt x ->
        if nt.Camlburg.cost >= x.icell.Camlburg.cost then
            x
        else
            { x with icell = (nt) }
and update_ireg =
    fun nt x ->
        if nt.Camlburg.cost >= x.ireg.Camlburg.cost then
            x
        else
            (fun x ->
                (update_freg
                    {Camlburg.cost = (nt.Camlburg.cost + 10)
                    ;Camlburg.action =
                        (fun () ->
                            let ireg = x.ireg.Camlburg.action ()
                            in
                                
# 326 "dummyexpander.mlb"
                                (  
                ireg                                >>= fun t ->
                let w = W.loc t in
                ftmp w                              >>= fun u  ->
                exec (R.store u (R.fetch t w) w)    >>= fun () ->
                return u
            )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                ((fun x ->
                    (update_addr
                        {Camlburg.cost = (nt.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let ireg = x.ireg.Camlburg.action ()
                                in
                                    
# 294 "dummyexpander.mlb"
                                    ( ireg >>= fun r -> return (fetch r) )
                                    
# 000 "/dev/stdout"
)
                        })
                        x)
                    { x with ireg = (nt) })
and update_mem =
    fun nt x ->
        if nt.Camlburg.cost >= x.mem.Camlburg.cost then
            x
        else
            { x with mem = (nt) }
and update_rtl =
    fun nt x ->
        if nt.Camlburg.cost >= x.rtl.Camlburg.cost then
            x
        else
            { x with rtl = (nt) }
and update_stmt =
    fun nt x ->
        if nt.Camlburg.cost >= x.stmt.Camlburg.cost then
            x
        else
            { x with stmt = (nt) }


let rec
conVar =
    fun () ->
        (update_error
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 392 "dummyexpander.mlb"
                    ( error "Var constructor" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conTrue =
    fun () ->
        (update_guard
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 343 "dummyexpander.mlb"
                    ( return (R.bool true)  )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conStore =
    fun arg1 arg2 arg3 ->
        (update_stmt
            (Camlburg.choice
                [{Camlburg.cost =
                    (arg1.mem.Camlburg.cost + arg2.ireg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let mem = arg1.mem.Camlburg.action ()
                        and ireg = arg2.ireg.Camlburg.action ()
                        and width = arg3
                        in
                            
# 334 "dummyexpander.mlb"
                            ( store mem   ireg  width )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.mem.Camlburg.cost + arg2.freg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let mem = arg1.mem.Camlburg.action ()
                        and freg = arg2.freg.Camlburg.action ()
                        and width = arg3
                        in
                            
# 335 "dummyexpander.mlb"
                            ( store mem   freg  width )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.icell.Camlburg.cost + arg2.ireg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let icell = arg1.icell.Camlburg.action ()
                        and ireg = arg2.ireg.Camlburg.action ()
                        and width = arg3
                        in
                            
# 336 "dummyexpander.mlb"
                            ( store icell ireg  width )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.fcell.Camlburg.cost + arg2.freg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let fcell = arg1.fcell.Camlburg.action ()
                        and freg = arg2.freg.Camlburg.action ()
                        and width = arg3
                        in
                            
# 337 "dummyexpander.mlb"
                            ( store fcell freg  width )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (1 + arg1.ccell.Camlburg.cost + arg2.ireg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let ccell = arg1.ccell.Camlburg.action ()
                        and ireg = arg2.ireg.Camlburg.action ()
                        and width = arg3
                        in
                            
# 338 "dummyexpander.mlb"
                            ( store ccell ireg  width )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.dcell.Camlburg.cost + arg2.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let dcell = arg1.dcell.Camlburg.action ()
                        and dreg = arg2.dreg.Camlburg.action ()
                        and width = arg3
                        in
                            
# 339 "dummyexpander.mlb"
                            ( store dcell dreg  width )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.dcell.Camlburg.cost + arg2.const.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let dcell = arg1.dcell.Camlburg.action ()
                        and const = arg2.const.Camlburg.action ()
                        and width = arg3
                        in
                            
# 340 "dummyexpander.mlb"
                            ( store' dcell const width )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    (arg1.ccell.Camlburg.cost + arg2.const.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let ccell = arg1.ccell.Camlburg.action ()
                        and const = arg2.const.Camlburg.action ()
                        and width = arg3
                        in
                            
# 341 "dummyexpander.mlb"
                            ( store' ccell const width )
                            
# 000 "/dev/stdout"
)
                }]))
            inf
and conSlice =
    fun () ->
        (update_error
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 393 "dummyexpander.mlb"
                    ( error "Slice constrictor" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conRtl =
    fun arg1 ->
        (update_rtl
            {Camlburg.cost = (1 + arg1.gstmts.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let gstmts = arg1.gstmts.Camlburg.action ()
                    in
                        
# 385 "dummyexpander.mlb"
                        (
                gstmts            >>= fun rtls ->
                exec (R.par rtls) >>= fun ()  ->
                return ()
            )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conReg =
    fun arg1 arg2 arg3 ->
        (update_ccell
            {Camlburg.cost = ((Camlburg.matches 'c') arg1)
            ;Camlburg.action =
                (fun () ->
                    let index = arg2
                    and width = arg3
                    in
                        
# 306 "dummyexpander.mlb"
                        ( reg 'c' index width )
                        
# 000 "/dev/stdout"
)
            })
            ((update_dcell
                {Camlburg.cost = ((Camlburg.matches 'd') arg1)
                ;Camlburg.action =
                    (fun () ->
                        let index = arg2
                        and width = arg3
                        in
                            
# 305 "dummyexpander.mlb"
                            ( reg 'd' index width )
                            
# 000 "/dev/stdout"
)
                })
                ((update_error
                    {Camlburg.cost = (0)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1
                            and index = arg2
                            and width = arg3
                            in
                                
# 309 "dummyexpander.mlb"
                                ( error (Printf.sprintf "register in space '%c'" x) )
                                
# 000 "/dev/stdout"
)
                    })
                    ((update_fcell
                        (Camlburg.choice
                            [{Camlburg.cost = ((Camlburg.matches 'f') arg1)
                            ;Camlburg.action =
                                (fun () ->
                                    let index = arg2
                                    and width = arg3
                                    in
                                        
# 303 "dummyexpander.mlb"
                                        ( reg 'f' index width )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost = ((Camlburg.matches 'u') arg1)
                            ;Camlburg.action =
                                (fun () ->
                                    let index = arg2
                                    and width = arg3
                                    in
                                        
# 304 "dummyexpander.mlb"
                                        ( reg 'u' index width )
                                        
# 000 "/dev/stdout"
)
                            }]))
                        ((update_icell
                            (Camlburg.choice
                                [{Camlburg.cost =
                                    ((Camlburg.matches 'r') arg1)
                                ;Camlburg.action =
                                    (fun () ->
                                        let index = arg2
                                        and width = arg3
                                        in
                                            
# 299 "dummyexpander.mlb"
                                            ( reg 'r' index width )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost =
                                    ((Camlburg.matches 't') arg1)
                                ;Camlburg.action =
                                    (fun () ->
                                        let index = arg2
                                        and width = arg3
                                        in
                                            
# 300 "dummyexpander.mlb"
                                            ( reg 't' index width )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost =
                                    ((Camlburg.matches 'g') arg1)
                                ;Camlburg.action =
                                    (fun () ->
                                        let index = arg2
                                        and width = arg3
                                        in
                                            
# 301 "dummyexpander.mlb"
                                            ( reg 'g' index width )
                                            
# 000 "/dev/stdout"
)
                                }
                                ;{Camlburg.cost =
                                    ((Camlburg.matches 'v') arg1)
                                ;Camlburg.action =
                                    (fun () ->
                                        let index = arg2
                                        and width = arg3
                                        in
                                            
# 302 "dummyexpander.mlb"
                                            ( reg 'v' index width )
                                            
# 000 "/dev/stdout"
)
                                }]))
                            inf))))
and conNil =
    fun () ->
        (update_gstmts
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 380 "dummyexpander.mlb"
                    (  return [] )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conMem =
    fun arg1 arg2 arg3 arg4 arg5 ->
        (update_error
            {Camlburg.cost = (10 + arg4.addr.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1
                    and agg = arg2
                    and width = arg3
                    and addr = arg4.addr.Camlburg.action ()
                    and ass = arg5
                    in
                        
# 312 "dummyexpander.mlb"
                        ( error (Printf.sprintf "cell in space '%c'" x) )
                        
# 000 "/dev/stdout"
)
            })
            ((update_mem
                {Camlburg.cost =
                    ((Camlburg.matches 'm') arg1 + arg4.addr.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let agg = arg2
                        and width = arg3
                        and addr = arg4.addr.Camlburg.action ()
                        and ass = arg5
                        in
                            
# 297 "dummyexpander.mlb"
                            ( mem 'm' agg width addr ass )
                            
# 000 "/dev/stdout"
)
                })
                inf)
and conKill =
    fun () ->
        (update_error
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 391 "dummyexpander.mlb"
                    ( error "cannot handle Kill" )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conGStmt =
    fun arg1 arg2 ->
        (update_gstmt
            {Camlburg.cost =
                (arg1.guard.Camlburg.cost + arg2.stmt.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let guard = arg1.guard.Camlburg.action ()
                    and stmt = arg2.stmt.Camlburg.action ()
                    in
                        
# 374 "dummyexpander.mlb"
                        ( 
                guard >>= fun g ->
                stmt  >>= fun s -> 
                return (R.guard g s) 
            )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conFetch =
    fun arg1 arg2 ->
        (update_dreg
            {Camlburg.cost = (arg1.dcell.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let dcell = arg1.dcell.Camlburg.action ()
                    and width = arg2
                    in
                        
# 229 "dummyexpander.mlb"
                        ( dcell )
                        
# 000 "/dev/stdout"
)
            })
            ((update_freg
                (Camlburg.choice
                    [{Camlburg.cost = (1 + arg1.mem.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let mem = arg1.mem.Camlburg.action ()
                            and width = arg2
                            in
                                
# 221 "dummyexpander.mlb"
                                (  mem           >>= fun m ->
                ftmp width    >>= fun u -> 
                exec (R.store u (R.fetch m width) width) >>= fun () ->
                return u
            )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost = (arg1.fcell.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let fcell = arg1.fcell.Camlburg.action ()
                            and width = arg2
                            in
                                
# 228 "dummyexpander.mlb"
                                ( fcell )
                                
# 000 "/dev/stdout"
)
                    }]))
                ((update_ireg
                    (Camlburg.choice
                        [{Camlburg.cost = (1 + arg1.mem.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let mem = arg1.mem.Camlburg.action ()
                                and width = arg2
                                in
                                    
# 213 "dummyexpander.mlb"
                                    (  mem           >>= fun m ->
                itmp width    >>= fun t -> 
                exec (R.store t (R.fetch m width) width) >>= fun () ->
                return t
            )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost = (arg1.icell.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let icell = arg1.icell.Camlburg.action ()
                                and width = arg2
                                in
                                    
# 227 "dummyexpander.mlb"
                                    ( icell )
                                    
# 000 "/dev/stdout"
)
                        }]))
                    inf))
and conFalse =
    fun () ->
        (update_guard
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    
# 344 "dummyexpander.mlb"
                    ( return (R.bool false) )
                    
# 000 "/dev/stdout"
)
            })
            inf
and conConst =
    fun arg1 ->
        (update_const
            {Camlburg.cost = (0)
            ;Camlburg.action =
                (fun () ->
                    let k = arg1
                    in
                        
# 201 "dummyexpander.mlb"
                        ( return (U.exp (RP.Const(k))) )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conCons =
    fun arg1 arg2 ->
        (update_gstmts
            {Camlburg.cost =
                (arg1.gstmt.Camlburg.cost + arg2.gstmts.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let gstmt = arg1.gstmt.Camlburg.action ()
                    and gstmts = arg2.gstmts.Camlburg.action ()
                    in
                        
# 382 "dummyexpander.mlb"
                        ( gstmt  >>= fun s  -> gstmts >>= fun ss -> return (s::ss) )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conApp3 =
    fun arg1 arg2 arg3 arg4 arg5 ->
        (update_freg
            (Camlburg.choice
                [{Camlburg.cost =
                    ((Camlburg.matches "fadd") arg1 + arg3.freg.Camlburg.cost
                    +
                    arg4.freg.Camlburg.cost
                    +
                    arg5.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg2
                        and r1 = arg3.freg.Camlburg.action ()
                        and r2 = arg4.freg.Camlburg.action ()
                        and c = arg5.dreg.Camlburg.action ()
                        in
                            
# 273 "dummyexpander.mlb"
                            (app3 ftmp "fsub" x r1 r2 c)
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    ((Camlburg.matches "fsub") arg1 + arg3.freg.Camlburg.cost
                    +
                    arg4.freg.Camlburg.cost
                    +
                    arg5.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg2
                        and r1 = arg3.freg.Camlburg.action ()
                        and r2 = arg4.freg.Camlburg.action ()
                        and c = arg5.dreg.Camlburg.action ()
                        in
                            
# 274 "dummyexpander.mlb"
                            (app3 ftmp "fadd" x r1 r2 c)
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    ((Camlburg.matches "fdiv") arg1 + arg3.freg.Camlburg.cost
                    +
                    arg4.freg.Camlburg.cost
                    +
                    arg5.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg2
                        and r1 = arg3.freg.Camlburg.action ()
                        and r2 = arg4.freg.Camlburg.action ()
                        and c = arg5.dreg.Camlburg.action ()
                        in
                            
# 275 "dummyexpander.mlb"
                            (app3 ftmp "fdiv" x r1 r2 c)
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost =
                    ((Camlburg.matches "fmul") arg1 + arg3.freg.Camlburg.cost
                    +
                    arg4.freg.Camlburg.cost
                    +
                    arg5.dreg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg2
                        and r1 = arg3.freg.Camlburg.action ()
                        and r2 = arg4.freg.Camlburg.action ()
                        and c = arg5.dreg.Camlburg.action ()
                        in
                            
# 276 "dummyexpander.mlb"
                            (app3 ftmp "fmul" x r1 r2 c)
                            
# 000 "/dev/stdout"
)
                }]))
            inf
and conApp2 =
    fun arg1 arg2 arg3 arg4 ->
        (update_dreg
            {Camlburg.cost =
                (1 + (Camlburg.matches "fcmp") arg1 + arg3.freg.Camlburg.cost
                +
                arg4.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg2
                    and f1 = arg3.freg.Camlburg.action ()
                    and f2 = arg4.freg.Camlburg.action ()
                    in
                        
# 284 "dummyexpander.mlb"
                        (
                f1 >>= fun f1 -> f2 >>= fun f2 ->
                let o        = R.opr "fcmp" x in
                let exp      = R.app o [fetch f1; fetch f2] in
                let fpcond n = R.reg ('d', n, 2) in
                let r        = fpcond 1 in
                exec (R.store r exp 2) >>= fun () ->
                return r
            )
                        
# 000 "/dev/stdout"
)
            })
            ((update_freg
                (Camlburg.choice
                    [{Camlburg.cost =
                        ((Camlburg.matches "f2i") arg1
                        +
                        arg3.freg.Camlburg.cost
                        +
                        arg4.dreg.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg2
                            and r1 = arg3.freg.Camlburg.action ()
                            and r2 = arg4.dreg.Camlburg.action ()
                            in
                                
# 268 "dummyexpander.mlb"
                                ( app2 itmp "f2i"  x r1 r2 )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        ((Camlburg.matches "i2f") arg1
                        +
                        arg3.ireg.Camlburg.cost
                        +
                        arg4.dreg.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg2
                            and r1 = arg3.ireg.Camlburg.action ()
                            and r2 = arg4.dreg.Camlburg.action ()
                            in
                                
# 269 "dummyexpander.mlb"
                                ( app2 ftmp "i2f"  x r1 r2 )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        ((Camlburg.matches "f2f") arg1
                        +
                        arg3.freg.Camlburg.cost
                        +
                        arg4.dreg.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg2
                            and r1 = arg3.freg.Camlburg.action ()
                            and r2 = arg4.dreg.Camlburg.action ()
                            in
                                
# 270 "dummyexpander.mlb"
                                ( app2 ftmp "f2f"  x r1 r2 )
                                
# 000 "/dev/stdout"
)
                    }]))
                ((update_guard
                    (Camlburg.choice
                        [{Camlburg.cost =
                            (let o = arg1
                            and x = arg2
                            in
                                
# 347 "dummyexpander.mlb"
                                ( if is_bool_op2 o then 0 else C.inf_cost  )
                                
# 000 "/dev/stdout"

                            +
                            arg3.ireg.Camlburg.cost
                            +
                            arg4.ireg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let o = arg1
                                and x = arg2
                                and r1 = arg3.ireg.Camlburg.action ()
                                and r2 = arg4.ireg.Camlburg.action ()
                                in
                                    
# 348 "dummyexpander.mlb"
                                    (  
                r1 >>= fun r1 -> r2 >>= fun r2 ->
                let opr = R.opr o x in
                return (R.app opr [fetch r1; fetch r2])
            )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            (let o = arg1
                            and x = arg2
                            in
                                
# 358 "dummyexpander.mlb"
                                ( if is_bool_op2 o then 0 else C.inf_cost  )
                                
# 000 "/dev/stdout"

                            +
                            arg3.dreg.Camlburg.cost
                            +
                            arg4.dreg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let o = arg1
                                and x = arg2
                                and r1 = arg3.dreg.Camlburg.action ()
                                and r2 = arg4.dreg.Camlburg.action ()
                                in
                                    
# 359 "dummyexpander.mlb"
                                    (  
                r1 >>= fun r1 -> r2 >>= fun r2 ->
                let opr = R.opr o x in
                return (R.app opr [fetch r1; fetch r2])
            )
                                    
# 000 "/dev/stdout"
)
                        }]))
                    ((update_ireg
                        (Camlburg.choice
                            [{Camlburg.cost =
                                ((Camlburg.matches "add") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 248 "dummyexpander.mlb"
                                        ( app2 itmp "add"  x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "and") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 249 "dummyexpander.mlb"
                                        ( app2 itmp "and"  x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "div") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 250 "dummyexpander.mlb"
                                        ( app2 itmp "div"  x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "divu") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 251 "dummyexpander.mlb"
                                        ( app2 itmp "divu" x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "mod") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 252 "dummyexpander.mlb"
                                        ( app2 itmp "mod"  x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "modu") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 253 "dummyexpander.mlb"
                                        ( app2 itmp "modu" x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "mul") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 254 "dummyexpander.mlb"
                                        ( app2 itmp "mul"  x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "or") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 255 "dummyexpander.mlb"
                                        ( app2 itmp "or"   x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "quot") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 256 "dummyexpander.mlb"
                                        ( app2 itmp "quot" x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "rem") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 257 "dummyexpander.mlb"
                                        ( app2 itmp "rem"  x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "rotl") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 258 "dummyexpander.mlb"
                                        ( app2 itmp "rotl" x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "rotr") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 259 "dummyexpander.mlb"
                                        ( app2 itmp "rotr" x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "shl") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 260 "dummyexpander.mlb"
                                        ( app2 itmp "shl"  x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "shra") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 261 "dummyexpander.mlb"
                                        ( app2 itmp "shra" x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "shrl") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 262 "dummyexpander.mlb"
                                        ( app2 itmp "shrl" x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "sub") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 263 "dummyexpander.mlb"
                                        ( app2 itmp "sub"  x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }
                            ;{Camlburg.cost =
                                ((Camlburg.matches "xor") arg1
                                +
                                arg3.ireg.Camlburg.cost
                                +
                                arg4.ireg.Camlburg.cost)
                            ;Camlburg.action =
                                (fun () ->
                                    let x = arg2
                                    and r1 = arg3.ireg.Camlburg.action ()
                                    and r2 = arg4.ireg.Camlburg.action ()
                                    in
                                        
# 264 "dummyexpander.mlb"
                                        ( app2 itmp "xor"  x r1 r2 )
                                        
# 000 "/dev/stdout"
)
                            }]))
                        inf)))
and conApp1 =
    fun arg1 arg2 arg3 ->
        (update_freg
            {Camlburg.cost =
                ((Camlburg.matches "fneg") arg1 + arg3.freg.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let x = arg2
                    and r1 = arg3.freg.Camlburg.action ()
                    in
                        
# 266 "dummyexpander.mlb"
                        ( app1 ftmp "fneg" x r1 )
                        
# 000 "/dev/stdout"
)
            })
            ((update_guard
                {Camlburg.cost =
                    (let o = arg1
                    and x = arg2
                    in
                        
# 366 "dummyexpander.mlb"
                        ( if is_bool_op1 o then 0 else C.inf_cost  )
                        
# 000 "/dev/stdout"

                    +
                    arg3.ireg.Camlburg.cost)
                ;Camlburg.action =
                    (fun () ->
                        let o = arg1
                        and x = arg2
                        and r1 = arg3.ireg.Camlburg.action ()
                        in
                            
# 367 "dummyexpander.mlb"
                            (  
                r1 >>= fun r1 -> 
                let opr = R.opr o x in
                return (R.app opr [fetch r1])
            )
                            
# 000 "/dev/stdout"
)
                })
                ((update_ireg
                    (Camlburg.choice
                        [{Camlburg.cost =
                            ((Camlburg.matches "com") arg1
                            +
                            arg3.ireg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let x = arg2
                                and r1 = arg3.ireg.Camlburg.action ()
                                in
                                    
# 242 "dummyexpander.mlb"
                                    ( app1 itmp "com"  x r1 )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            ((Camlburg.matches "lobits") arg1
                            +
                            arg3.ireg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let x = arg2
                                and r1 = arg3.ireg.Camlburg.action ()
                                in
                                    
# 243 "dummyexpander.mlb"
                                    ( app1 itmp "lobits" x r1 )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            ((Camlburg.matches "neg") arg1
                            +
                            arg3.ireg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let x = arg2
                                and r1 = arg3.ireg.Camlburg.action ()
                                in
                                    
# 244 "dummyexpander.mlb"
                                    ( app1 itmp "neg"  x r1 )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            ((Camlburg.matches "sx") arg1
                            +
                            arg3.ireg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let x = arg2
                                and r1 = arg3.ireg.Camlburg.action ()
                                in
                                    
# 245 "dummyexpander.mlb"
                                    ( app1 itmp "sx"   x r1 )
                                    
# 000 "/dev/stdout"
)
                        }
                        ;{Camlburg.cost =
                            ((Camlburg.matches "zx") arg1
                            +
                            arg3.ireg.Camlburg.cost)
                        ;Camlburg.action =
                            (fun () ->
                                let x = arg2
                                and r1 = arg3.ireg.Camlburg.action ()
                                in
                                    
# 246 "dummyexpander.mlb"
                                    ( app1 itmp "zx"   x r1 )
                                    
# 000 "/dev/stdout"
)
                        }]))
                    inf))
and conApp0 =
    fun arg1 arg2 ->
        (update_const
            (Camlburg.choice
                [{Camlburg.cost = ((Camlburg.matches "round_down") arg1)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg2
                        in
                            
# 236 "dummyexpander.mlb"
                            ( return (bits2 3) )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost = ((Camlburg.matches "round_nearest") arg1)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg2
                        in
                            
# 237 "dummyexpander.mlb"
                            ( return (bits2 1) )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost = ((Camlburg.matches "round_up") arg1)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg2
                        in
                            
# 238 "dummyexpander.mlb"
                            ( return (bits2 2) )
                            
# 000 "/dev/stdout"
)
                }
                ;{Camlburg.cost = ((Camlburg.matches "round_zero") arg1)
                ;Camlburg.action =
                    (fun () ->
                        let x = arg2
                        in
                            
# 239 "dummyexpander.mlb"
                            ( return (bits2 0) )
                            
# 000 "/dev/stdout"
)
                }]))
            ((update_dreg
                (Camlburg.choice
                    [{Camlburg.cost = ((Camlburg.matches "float_eq") arg1)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg2
                            in
                                
# 231 "dummyexpander.mlb"
                                ( app0 dtmp "float_eq" x )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost = ((Camlburg.matches "float_gt") arg1)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg2
                            in
                                
# 232 "dummyexpander.mlb"
                                ( app0 dtmp "float_gt" x )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost = ((Camlburg.matches "float_lt") arg1)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg2
                            in
                                
# 233 "dummyexpander.mlb"
                                ( app0 dtmp "float_lt" x )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost = ((Camlburg.matches "unordered") arg1)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg2
                            in
                                
# 234 "dummyexpander.mlb"
                                ( app0 dtmp "unordered" x )
                                
# 000 "/dev/stdout"
)
                    }]))
                inf)



# 115 "dummyexpander.mlb"
 
 (*s: tail *)
 let rec map f = function
     | []                        -> conNil ()
     | x::xs                     -> conCons (f x) (map f xs)
 (*x: tail *)
 let rec exp = function
     | RP.Const(RP.Bool(true))   -> conTrue()
     | RP.Const(RP.Bool(false))  -> conFalse()
     | RP.Const(k)               -> conConst(k)
     | RP.Fetch(l,w)             -> conFetch (loc l) w
     | RP.App((o,x),[])          -> conApp0 o x
     | RP.App((o,x),[e1])        -> conApp1 o x (exp e1)
     | RP.App((o,x),[e1;e2])     -> conApp2 o x (exp e1) (exp e2)
     | RP.App((o,x),[e1;e2;e3])  -> conApp3 o x (exp e1) (exp e2) (exp e3)
     | _                         -> error "application with too many args"

 and loc = function
     | RP.Mem(char, aff, w, e, ass) -> conMem char aff w (exp e) ass 
     | RP.Reg(sp, i, w)          -> conReg sp i w
     | RP.Var(s, i, w)           -> conVar ()
     | RP.Slice(w,i,loc)         -> conSlice ()
    
 and stmt = function
     | RP.Store(l,e,w)           -> conStore (loc l) (exp e) w
     | RP.Kill(loc)              -> conKill ()

 and guarded (e,eff)             =  conGStmt (exp e) (stmt eff)

 and rtl = function
     | RP.Rtl(gs)                -> conRtl (map guarded gs)
 (*x: tail *)
 let expand tmps (r: R.rtl) = 
     let plan = rtl (D.rtl r) in
     try
         match plan.rtl.Camlburg.action () ([],tmps) with
             | (), (rtls,_) -> rtls
     with 
         Camlburg.Uncovered -> 
             ( List.iter prerr_endline
                 [ "cannot expand this RTL:"
                 ; RU.ToUnreadableString.rtl r
                 ]
             ; assert false
             )
 (*x: tail *)
 let cfg proc =
     let nodes       = GU.fold_bwd proc.Proc.cfg (fun n ns -> n::ns) []
     and tmps        = proc.Proc.temps   (* source for temporaries *)
     and insert n i  = G.gm_insert_assign_before i n
     and modified    = ref false in
     let expnd node =
         match expand tmps (G.instr node) with
         | last :: preds ->
             ( ignore (List.fold_left insert node preds)
             ; G.upd_instr node (fun _ -> last)
             ; modified := (preds <> [])
             )
         | [] -> assert false 
     in    
         ( List.iter expnd nodes
         ; !modified
         )
 (*x: tail *)
 module Make (ProcT: Lua.Lib.TYPEVIEW with type 'a t = Proc.t): 
     Lua.Lib.USERCODE with type 'a userdata' = 'a ProcT.combined =
 struct
     type 'a userdata' = 'a ProcT.combined
     module M (C : Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') =
     struct
       module V  = C.V

       let ( **-> ) = V.( **-> )
       let proc     = ProcT.makemap V.userdata V.projection
       let init =
         C.register_module "Expander" 
         [ "dummy", V.efunc (V.value **-> proc **-> V.result V.bool) (fun _ -> cfg)
         ] 
  (* FIX -- want proper stage *)
     end (*M*)    
 end (*Make*)            
 (*e: tail *)


# 000 "/dev/stdout"
