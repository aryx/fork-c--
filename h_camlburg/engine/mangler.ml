(*s: mangler.ml *)
(*s: types *)
type spec =
    { preprocess:     string -> string
    ; replace:        char   -> char
    ; reserved:       string list
    ; avoid:          string -> string
    }
(*x: types *)
type t = string -> string
(*e: types *)

module StrCmp = struct type t = string let compare = compare end
module StrSet = Set.Make(StrCmp)

type state =
    { mutable used:     StrSet.t
    ; mangled:          (string, string) Hashtbl.t
    }

let unique state avoid str =
    let rec loop str =
        if   StrSet.mem str state.used
        then loop (avoid str)
        else str
    in
        loop str

let map (f: char->char) str =
    let str' = String.copy str in
    let ()   = for i = 0 to String.length str - 1 do
                str'.[i] <- f str'.[i]
               done
    in
        str'

let ($$) x f = f x (* x $$ f1 $$ f2 == f2 (f1 x) *)
    
let mk (spec: spec) = 
    let state = 
        { used     = List.fold_right StrSet.add spec.reserved StrSet.empty 
        ; mangled  = Hashtbl.create 29 (* initial size - make parameter? *)
        } in
    fun str ->
        try Hashtbl.find state.mangled str      (* if mangled before *)
        with Not_found ->
            let str' = str 
                     $$ spec.preprocess         (*  1st transformation *)
                     $$ map spec.replace        (*  2nd transformation *)
                     $$ unique state spec.avoid (*  .. *)
            in
                ( state.used <- StrSet.add str' state.used
                ; Hashtbl.add state.mangled str str'
                ; str'
                )
(*e: mangler.ml *)
