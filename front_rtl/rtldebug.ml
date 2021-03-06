(*s: front_rtl/rtldebug.ml *)
(*s: rtldebug.ml *)
open Nopoly

module RP = Rtl.Private
exception TypeCheck of Rtl.rtl
let fail rtl = raise (TypeCheck rtl)

let typecheck (r:Rtl.rtl) =
    let width n ty = match ty with
        | Types.Bits k when n = k  -> ty
        | _                        -> fail r in
    let bits = function
        | Types.Bits n             -> n
        | _                        -> fail r in
    let bool = function
        | Types.Bool               -> ()
        | _                        -> fail r in 
        
    let rec exp = function
        | RP.Fetch (l, w)          -> width w (loc l)
        | RP.Const c               -> const c  
        | RP.App(opr, args)        -> 
            let argtys       = List.map exp args in
            let opty, result = Rtlop.mono (Rtl.Up.opr opr) in
                if opty =*= argtys then  (* comparing lists of types! *)
                    result
                else
                    fail r
    
    and const = function
        | RP.Bool _                -> Types.Bool 
        | RP.Bits b                -> Types.Bits (Bits.width b)
        | RP.Link (_,_,w)          -> Types.Bits w
 | RP.Diff (c,c')	   -> let t = const c and t' = const c' in
                                      if t =*= t' then t else fail r
        | RP.Late (_,w)            -> Types.Bits w
    
    and loc = function
        | RP.Mem ((_, _, ms),c,e,ass) ->
     let _ = bits (exp e) in Types.Bits (Cell.to_width ms c)
        | RP.Reg ((_, _, ms),i,c)     -> Types.Bits (Cell.to_width ms c)
        | RP.Slice (w,i,l)         -> let _ = bits (loc l) in Types.Bits w
        | RP.Var    (_,_,w)        -> Types.Bits w
        | RP.Global (_,_,w)        -> Types.Bits w
        
    and effect = function
        | RP.Store (l,e,w)         -> let _  = width w (loc l) in
                                      let _  = width w (exp e) in
                                      ()  
        | RP.Kill  (l)             -> let _ = bits (loc l) in ()  
    
    and guarded (e,eff)            =  ( bool (exp e)
                                      ; effect eff
                                      )
    and rtl (RP.Rtl rtl)           = List.iter guarded rtl in 
    rtl (Rtl.Dn.rtl r)
(*e: rtldebug.ml *)
(*e: front_rtl/rtldebug.ml *)
