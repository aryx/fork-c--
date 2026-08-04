(*s: assembler/mangle.ml *)
(*s: mangle.ml *)
type strset         = Strutil.Set.t
type strmap         = (string,string) Hashtbl.t

type t              = string -> string
type spec           = { preprocess:  string -> string
                      ; replace:     char -> char
                      ; reserved:    string list
                      ; avoid:       string -> string
                      }
(*x: mangle.ml *)
type state          = { mutable used: strset
                      ; map:          strmap (* is also mutable *)
                      }
(*x: mangle.ml *)
let mk spec =
    let state    = { used = Strutil.Set.empty
                   ; map  = Hashtbl.create 997 (* initial size *)
                   } 
    in
    
    let rec avoid s  =
            if   Strutil.Set.mem s state.used 
            then avoid (spec.avoid s)
            else s 
    in

    let newMangle s =
        let s'   = Bytes.of_string (spec.preprocess s) in
        let _    = for i = 0 to (Bytes.length s') - 1 do
                     Bytes.set s' i (spec.replace (Bytes.get s' i))
                   done in
        let s'   = avoid (Bytes.to_string s') in
            ( state.used <- Strutil.Set.add s'   state.used
            ; Hashtbl.add state.map s s' 
            ; s'
            )
    in         
    let mangle s = 
        try  Hashtbl.find state.map s
        with Not_found -> newMangle s
    in
        mangle
(*e: mangle.ml *)
(*e: assembler/mangle.ml *)
