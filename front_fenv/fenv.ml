(*s: fenv.ml *)
module E            = Error     (* handy abbreviations *)
module T            = Types
module Asm          = Asm    
(*x: fenv.ml *)
module type Arg = sig
    type 'a partial
    type 'a info
    
    val good    : 'a   -> 'a info
    val asgood  : 'a info -> 'a option
    val bad     : unit -> 'a info
end
(*x: fenv.ml *)
(*s: exposed types shared by clean and dirty environments *)
type regkind       = RReg  of string   (* hardware reg *)
                   | RKind of string   (* calling convention kind *)
                   | RNone             (* none of above *)
(*x: exposed types shared by clean and dirty environments *)
type variable =    { index:        int
                   ; rkind:        regkind
                   ; loc:          Rtl.loc
                   ; variance:     Ast.variance
                   }
(*x: exposed types shared by clean and dirty environments *)
type symclass      = Proc          of Symbol.t
                   | Code          of Symbol.t    
                   | Data          of Symbol.t
                   | Stack         of Rtl.exp  (* address of slot *)

type denotation    = Constant      of Bits.bits
                   | Label         of symclass
                   | Import        of string * Symbol.t
                                             (* external name, assembly symbol *)
                   | Variable      of variable
                   | Continuation  of continuation
and continuation = { base       : Block.t;   (* always empty; used only for address *)
                     convention : string;
                     formals    : (string * variable * aligned) list;
                                                  (* kinded, aligned formals *)
                     mutable escapes     : bool;  (* used as rvalue *)
                     mutable cut_to      : bool;  (* mentioned in annotation *)
                     mutable unwound_to  : bool;  (* mentioned in annotation *)
                     mutable returned_to : convention list;
                        (* list every convention cc such that there exists
                           a call site with convention cc and that call site
                           `also returns to' the continuation *)
                   } (* might need a convention here *)
and convention   = string
and aligned      = int
(*e: exposed types shared by clean and dirty environments *)
(*s: private types shared by clean and dirty environments *)
type stackdata =      { soffset    :    int    (* current offset *)
                      ; smaxalign  :    int    (* max stackdata align constr*)
                      ; sname      :    string (* label for offset *)
                      }
(*x: private types shared by clean and dirty environments *)
type extern         = { imported:     Strutil.Set.t
                      ; exported:     Strutil.Set.t
                      ; nam2sym:      Symbol.t Strutil.Map.t
                      }
(*e: private types shared by clean and dirty environments *)
(*s: exported signature [[Env]] *)
module type Env = sig
    type 'a info
    type 'a partial
    val  bad: unit -> 'a info

    (*s: bindings appearing in signature [[Env]] *)
    (* pad: was previously an abstract type *)
    type 'proc env' = { scopes          :    scope list (* top = hd scopes *)
                       ; srcmap          :    Srcmap.map
                       ; asm             :    'proc Asm.assembler
                       ; error           :    bool
                       ; metrics         :    Metrics.t
                       ; extern          :    extern
                       ; globals         :    string  list(* global registers *)
                       ; stackdata       :    stackdata
                       }
    (* type env = Proc.t env' *)
    and scope           = { mutable venv:   ventry Strutil.Map.t
                       ; tenv:   tentry Strutil.Map.t
                       ; rindex: int   (* getIndex, nextIndex *)
                       }

    (*s: types exposed in signature [[Env]] *)
    and  ventry        = Srcmap.rgn * (denotation * Types.ty) info
    (*x: types exposed in signature [[Env]] *)
    and  tentry         = Srcmap.rgn * Types.ty info
    (*e: types exposed in signature [[Env]] *)
    val map : ('b -> 'a) -> 'a env' -> 'b env'

    val empty   : Srcmap.map -> Metrics.t -> 'proc Asm.assembler -> 'proc env'
                           (* empty scope stack *)
    val srcmap  : 'proc env' -> Srcmap.map
    val asm     : 'proc env' -> 'proc Asm.assembler
    val metrics : 'proc env' -> Metrics.t
    (*x: bindings appearing in signature [[Env]] *)
    val emptyscope: scope   
    val top:        'p env' -> scope            (* top empty = assert false *)
    val pop:        'p env' -> 'p env'              (* pop empty = assert false *)
    val push:       'p env' -> scope -> 'p env'
    (*x: bindings appearing in signature [[Env]] *)
    val foldv:      (string -> ventry -> 'a -> 'a) -> scope -> 'a -> 'a
    (*x: bindings appearing in signature [[Env]] *)
    val bindv           : string -> ventry  -> 'p env' -> 'p env'
    val rebindv         : string -> ventry  -> 'p env' -> 'p env'
    val rebindv'        : string -> ventry  -> 'p env' -> unit  (* mutates *)
    val bindt           : string -> tentry  -> 'p env' -> 'p env'
    val findv           : string -> 'p env' -> ventry   (* Error.ErrorExn *)
    val findt           : string -> 'p env' -> tentry   (* Error.ErrorExn *)
    (*x: bindings appearing in signature [[Env]] *)
    val is_localv       : string -> 'p env' -> bool     (* *)
    (*x: bindings appearing in signature [[Env]] *)
    val flagError       : 'p env' -> 'p env'
    val errorFlag       : 'p env' -> bool
    (*x: bindings appearing in signature [[Env]] *)
    val import: Srcmap.rgn -> string -> string -> 'p env' -> 'p env' (* import g as f *)
    val export: Srcmap.rgn -> string -> string -> 'p env' -> 'p env' (* export f as g *)
    (*x: bindings appearing in signature [[Env]] *)
    val symbol:   'p env' -> string -> Symbol.t
    (*e: bindings appearing in signature [[Env]] *)
end
(*e: exported signature [[Env]] *)
module Env (Arg: Arg) = struct 
 (*s: body of functor [[Env]] *)
 type 'a partial     = 'a Arg.partial
 type 'a info        = 'a Arg.info
 let  bad            = Arg.bad

 (* pad: *)
 type  ventry        = Srcmap.rgn * (denotation * Types.ty) info
 type  tentry         = Srcmap.rgn * Types.ty info

 (*x: body of functor [[Env]] *)
 type 'proc env'    =  { scopes          :    scope list (* top = hd scopes *)
                       ; srcmap          :    Srcmap.map
                       ; asm             :    'proc Asm.assembler
                       ; error           :    bool
                       ; metrics         :    Metrics.t
                       ; extern          :    extern
                       ; globals         :    string  list(* global registers *)
                       ; stackdata       :    stackdata
                       }
 (*x: body of functor [[Env]] *)
 and scope           = { mutable venv:   ventry Strutil.Map.t
                       ; tenv:   tentry Strutil.Map.t
                       ; rindex: int   (* getIndex, nextIndex *)
                       }
 (*x: body of functor [[Env]] *)
 let error r map msg = E.errorRegionPrt (map,r) msg 
 (*x: body of functor [[Env]] *)
 let map f { scopes      = scopes     
           ; srcmap      = srcmap     
           ; asm         = asm        
           ; error       = error      
           ; metrics     = metrics     
           ; extern      = extern     
           ; globals     = globals    
           ; stackdata   = stackdata  
           } =
           { scopes      = scopes     
           ; srcmap      = srcmap     
           ; asm         = Asm.map f asm        
           ; error       = error      
           ; metrics     = metrics     
           ; extern      = extern     
           ; globals     = globals    
           ; stackdata   = stackdata  
           }
 (*x: body of functor [[Env]] *)
 let empty map metrics asm =  
     { scopes    = []
     ; srcmap    = map
     ; asm       = asm
     ; error     = false
     ; globals   = []
     ; stackdata = { smaxalign  = 1
                   ; soffset    = 0
                   ; sname      = "can't happen"
                   }
     ; metrics   = metrics
     ; extern    = { imported   = Strutil.Set.empty
                   ; exported   = Strutil.Set.empty
                   ; nam2sym    = Strutil.Map.empty
                   }
     } 
 (*x: body of functor [[Env]] *)
 let emptyscope          = { venv   = Strutil.Map.empty
                           ; tenv   = Strutil.Map.empty 
                           ; rindex = 0
                           }

 let srcmap  {srcmap=m}  = m
 let asm     {asm=a}     = a 
 let metrics {metrics=m} = m 

 let top env = match env.scopes with
     | []    -> assert false
     | s::ss -> s

 let pop env = match env.scopes with
     | []     -> assert false
     | s::ss  -> { env with scopes = ss }

 let push env scope = { env with scopes = scope :: env.scopes } 
 (*x: body of functor [[Env]] *)
 let foldv f {venv=v} z = Strutil.Map.fold f v z
 (*x: body of functor [[Env]] *)
 type typedefn = (Ast.ty * string list) * Ast.region
 let addTypedefn t env = assert false
 let typedefns env = assert false
 let setTypedefns ts env = assert false
 (*x: body of functor [[Env]] *)
 type constdefn = (Ast.ty option * string * Ast.expr) * Ast.region
 let addConstdefn t env = assert false
 let constdefns env = assert false
 let setConstdefns ts env = assert false
 (*x: body of functor [[Env]] *)
 let bindv name (rgn,x as ventry) env = 
     let scope = ( match env.scopes with
                 | []    -> assert false
                 | s::ss -> s
                 ) in
     try let (rgn',x) = Strutil.Map.find name scope.venv in
         ( error rgn  env.srcmap ("re-declaration of value "^name)
         ; error rgn' env.srcmap ("previously declared here")
         ; let scope = 
             { scope with venv = Strutil.Map.add name (rgn',Arg.bad()) scope.venv} 
           in
             { env with 
               scopes = scope :: List.tl env.scopes  
             ; error  = true  
             }
         )              
     with Not_found -> 
         let scope = { scope with venv = Strutil.Map.add name ventry scope.venv} in
         let env   = { env with scopes = scope :: List.tl env.scopes } in
             ( match Arg.asgood x with
             | Some(Variable _,_) when List.length env.scopes = 1 ->
                   (* this is a global register declaration *)
                   { env with globals = name :: env.globals }
             | _ -> env
             ) 
 (*x: body of functor [[Env]] *)
 let bindt name (rgn,x as tentry) env = 
     let scope = ( match env.scopes with
                 | []    -> assert false
                 | s::ss -> s
                 ) in
     try let (rgn',x) = Strutil.Map.find name scope.tenv in
         ( error rgn  env.srcmap ("re-declaration of type "^name)
         ; error rgn' env.srcmap ("previously declared here")
         ; let scope = 
             { scope with tenv = Strutil.Map.add name (rgn',Arg.bad()) scope.tenv} 
           in
             { env with 
               scopes = scope :: List.tl env.scopes  
             ; error  = true
             }
         )              
     with Not_found -> 
         let scope = { scope with tenv = Strutil.Map.add name tentry scope.tenv} in
         let env   = { env with scopes = scope :: List.tl env.scopes } in
             env
 (*x: body of functor [[Env]] *)
 let findv name env =
   let rec loop = function
     | []    -> E.error ("unknown value: "^name)
     | s::ss -> (try Strutil.Map.find name s.venv with Not_found -> loop ss) in
   loop env.scopes
 (*x: body of functor [[Env]] *)
 let findt name env =
   let rec loop = function
     | []    -> E.error ("unknown type: "^name)
     | s::ss -> ( try Strutil.Map.find name s.tenv with Not_found -> loop ss ) in
   loop env.scopes
 (*x: body of functor [[Env]] *)
 let rebindv name x env =
     let rec loop = function
         | []    -> Impossible.impossible ("can't rebind "^name)
         | s::ss -> if Strutil.Map.mem name s.venv 
                    then { s with venv = Strutil.Map.add name x s.venv } :: ss
                    else s :: loop ss in
     { env with scopes = loop env.scopes }

 let rebindv' name x env =
     let rec loop = function
         | []    -> Impossible.impossible ("can't rebind "^name)
         | s::ss -> if Strutil.Map.mem name s.venv 
                    then s.venv <- Strutil.Map.add name x s.venv 
                    else loop ss in
     loop env.scopes 
 (*x: body of functor [[Env]] *)
 let is_localv name env = 
     ( match env.scopes with
     | []    -> assert false
     | [s]   -> false
     | s::ss -> Strutil.Map.mem name s.venv
     )
 (*x: body of functor [[Env]] *)
 let flagError env = if env.error then env else { env with error = true }
 let errorFlag env = env.error
 (*x: body of functor [[Env]] *)
 let import r sym name env =
     if Strutil.Set.mem sym env.extern.exported 
     then 
         ( error r env.srcmap ("import of an exported name: "^name)
         ; flagError env
         )
     else 
         let sym'   = env.asm#import sym in
         let extern = { exported = env.extern.exported
                      ; imported = Strutil.Set.add sym  env.extern.imported
                      ; nam2sym  = Strutil.Map.add name sym' env.extern.nam2sym
                      }
          in
             { env with extern = extern }

 let export r name sym env =
     if Strutil.Set.mem sym env.extern.imported 
     then 
         ( error r env.srcmap ("export of an imported name: "^name)
         ; flagError env
         )
     else 
         let sym'   = env.asm#export sym in
         let extern = { imported = env.extern.imported
                       ; exported = Strutil.Set.add sym env.extern.exported
                       ; nam2sym  = Strutil.Map.add name sym' env.extern.nam2sym
                       }
          in
             { env with extern = extern }
 (*x: body of functor [[Env]] *)
 let symbol env n =
     try Strutil.Map.find n env.extern.nam2sym
     with Not_found ->
         let rec avoid_collision n =
             if Strutil.Set.mem n env.extern.exported ||
                Strutil.Set.mem n env.extern.imported
             then
                 avoid_collision (n ^ "@")
             else
                 n
         in
             env.asm#local (avoid_collision n)
 (*x: body of functor [[Env]] *)
 let nextIndex  env   = match env.scopes with
                        | top::t -> { env with scopes = 
                                         {top with rindex = top.rindex + 1}::t
                                    } 
                        | _      -> assert false (* no scope *)

 let getIndex   env   = match env.scopes with
                        | top::_ -> top.rindex
                        | _      -> assert false (* no scope *)
                       
 (*x: body of functor [[Env]] *)
 let globals env = List.rev env.globals
 (*e: body of functor [[Env]] *)
end
(*x: fenv.ml *)
module Dirty = Env (struct
    type 'a partial = 'a option
    type 'a info    = 'a Error.error
    
    let good    x   = Error.Ok(x)
    let bad     x   = Error.Error
    let asgood = function
        | Error.Ok(x) -> Some x
        | Error.Error -> None
end)
(*x: fenv.ml *)
module Clean = Env (struct
    type 'a partial             = 'a 
    type 'a info                = 'a

    let good x                  = x
    let asgood x                = Some x
    let bad  x                  = assert false
end)
(*x: fenv.ml *)
let denotation's_category = function
  | Label (Proc  _) -> "procedure"
  | Label (Code  _) -> "code label"
  | Label (Data  _) -> "data label"
  | Label (Stack _) -> "stackdata label"
  | Constant _      -> "constant"
  | Continuation _  -> "continuation"
  | Import (_, _)   -> "imported symbol"
  | Variable _      -> "register variable"
(*x: fenv.ml *)
let clean_env map =
    let copy key data map = match data with
    | pos, E.Ok den -> Strutil.Map.add key (pos, den) map
    | _,   E.Error  -> assert false in
    Strutil.Map.fold copy map Strutil.Map.empty      
(*x: fenv.ml *)
let clean_scope s =
    { Clean.tenv   = clean_env s.Dirty.tenv
    ; Clean.venv   = clean_env s.Dirty.venv
    ; Clean.rindex = s.Dirty.rindex
    }
(*x: fenv.ml *)
let clean env =
    { Clean.scopes    = List.map clean_scope env.Dirty.scopes
    ; Clean.srcmap    = env.Dirty.srcmap 
    ; Clean.asm       = env.Dirty.asm
    ; Clean.error     = env.Dirty.error
    ; Clean.metrics   = env.Dirty.metrics
    ; Clean.stackdata = env.Dirty.stackdata
    ; Clean.globals   = env.Dirty.globals
    ; Clean.extern    = env.Dirty.extern
    }
(*e: fenv.ml *)
