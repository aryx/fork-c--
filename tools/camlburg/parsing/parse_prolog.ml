(*s: parse_prolog.ml *)
(*s: prolog *)
module S = Spec
(*x: prolog *)
let error msg = raise (Parseerror.Error msg)
(*x: prolog *)
let p  ()  = (Parsing.symbol_start (), Parsing.symbol_end())
let px x   = (Parsing.rhs_start x    , Parsing.rhs_end x)
let rev    = List.rev   
(*x: prolog *)
type decls = 
    { terms     : S.StringSet.t
    ; heads     : Code.exp list     (* in reverse order *)
    ; tails     : Code.exp list     (* in reverse order *)
    ; types     : string S.StringMap.t
    }

let empty =
    { terms     = List.fold_right 
                    S.StringSet.add 
                    ["int"; "string"; "char"] 
                    S.StringSet.empty
    ; heads     = []
    ; tails     = []
    ; types     = S.StringMap.empty
    }
(*e: prolog *)
(*e: parse_prolog.ml *)
