(*s: front_rtl/rtldebug.mli *)
(*s: rtldebug.mli *)
exception TypeCheck of Rtl.rtl
val typecheck:     Rtl.rtl -> unit      (* raises TypeCheck *)
(*e: rtldebug.mli *)
(*e: front_rtl/rtldebug.mli *)
