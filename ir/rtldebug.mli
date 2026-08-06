(*s: front_rtl/rtldebug.mli *)
(*s: rtldebug.mli content *)
exception TypeCheck of Rtl.rtl
val typecheck:     Rtl.rtl -> unit      (* raises TypeCheck *)
(*e: rtldebug.mli content *)
(*e: front_rtl/rtldebug.mli *)
