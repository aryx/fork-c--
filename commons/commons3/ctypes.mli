(*s: commons3/ctypes.mli *)
(*s: ctypes.mli *)
(*s: types *)
type 'a ctypes = { char               : 'a
                 ; double             : 'a
                 ; float              : 'a
                 ; int                : 'a
                 ; long_double        : 'a
                 ; long_int           : 'a
                 ; long_long_int      : 'a
                 ; short              : 'a
                 ; signed_char        : 'a
                 ; unsigned_char      : 'a
                 ; unsigned_long      : 'a
                 ; unsigned_short     : 'a
                 ; unsigned_int       : 'a
                 ; unsigned_long_long : 'a
                 ; address            : 'a
                 }
(*x: types *)
type width   = int
type metrics = { w : width; va_w : width}
(*e: types *)
val enum_int : string -> int
(*
val ctypes_vararg_enum : string
val ctypes_vararg_str  : string
*)
(*e: ctypes.mli *)
(*e: commons3/ctypes.mli *)
