(*s: rtldebug.mli *)
exception TypeCheck of Rtl.rtl
val typecheck:     Rtl.rtl -> unit      (* raises TypeCheck *)
(*e: rtldebug.mli *)
