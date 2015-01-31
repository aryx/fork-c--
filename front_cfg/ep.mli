(*s: front_cfg/ep.mli *)
(*s: ep.mli *)
type ('em, 'pr) pre_map =
    { embed   : 'em
    ; project : 'pr
    }
type ('lo, 'hi) map = ('lo -> 'hi, 'hi -> 'lo) pre_map
(*e: ep.mli *)
(*e: front_cfg/ep.mli *)
