(*s: front_zipcfg/varmap.mli *)
(*s: varmap.mli *)
type reg  = Register.t
type rset = Register.Set.t
type temp = Register.t
type loc_pair  = { reg  : Register.t option; mem  : Rtlutil.aloc option }
type loc_pair' = { reg' : Register.t option; mem' : bool }
(*s: types for tracking defs and uses *)
type use_dist = Use of (int * int)
(*x: types for tracking defs and uses *)
type hwdef_num = (int * int option) (* (n, m):  nth def in basic block m *)
type hwdef = Copy    of (hwdef_num * int * Register.t * hwdef)
           | NonCopy of int
(*x: types for tracking defs and uses *)
type def_dist = HW of hwdef
              | Temp of (int * int)

(*e: types for tracking defs and uses *)
(*x: varmap.mli *)
type t
val empty            : t
val add_reg          : temp -> Register.t   -> t -> t
val add_mem          : temp -> Rtlutil.aloc -> t -> t
val spill            : temp -> Rtlutil.aloc -> t -> t
val remove_reg       : temp -> Register.t   -> t -> t
val remove_mem       : temp -> Rtlutil.aloc -> t -> t

type y
val emptyy           : y
val is_empty'        : y -> bool
val add_reg'         : temp -> Register.t   -> y -> y
val add_mem'         : temp -> y -> y
val remove_mem'      : temp -> y -> y
val remove_reg'      : temp -> y -> y
val spill'           : temp -> y -> y
val join             : y -> y -> y
val eq'              : old:y -> new':y -> bool
(*x: varmap.mli *)
val fold             : (temp -> Register.t   -> 'a -> 'a) ->
                       (temp -> Rtlutil.aloc -> 'a -> 'a) -> t -> 'a -> 'a
val fold'            : (temp -> Register.t   -> 'a -> 'a) ->
                       (temp -> 'a -> 'a) -> y -> 'a -> 'a
val filter           : (temp -> bool) -> t -> t
val var_locs'        : t -> temp -> loc_pair
val var_locs''       : y -> temp -> loc_pair'
val temp_loc'        : y -> temp -> reg  option
val reg_contents     : t -> reg  -> temp option
val reg_contents'    : y -> reg  -> temp list
val print            : string -> t -> unit
val print'           : string -> y -> unit
(*x: varmap.mli *)
val free_reg_inregs  : rset -> rset -> rset -> t -> rset -> bool -> reg -> bool
val free_reg_outregs : rset -> rset -> t -> rset -> bool -> reg -> bool
val alloc_inreg  : reg list -> rset -> rset -> rset -> t -> rset -> bool ->
                   temp -> reg
val alloc_outreg : reg list -> rset -> rset ->         t -> rset -> bool ->
                   temp -> reg
(*x: varmap.mli *)
val sync_maps : t -> t -> t * t
(*e: varmap.mli *)
(*e: front_zipcfg/varmap.mli *)
