(*s: front_cfg/ep.ml *)
(*s: ep.ml *)
type ('em, 'pr) pre_map =
    { embed   : 'em
    ; project : 'pr
    }
type ('lo, 'hi) map = ('lo -> 'hi, 'hi -> 'lo) pre_map
(*e: ep.ml *)
(*e: front_cfg/ep.ml *)
