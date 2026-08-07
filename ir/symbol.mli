(*s: front_rtl/symbol.mli *)
(*s: symbol.mli content *)
(*s: class type t *)
class type t = object
    method mangled_text:    string
    method original_text:   string
end 
val pp : Format.formatter -> t -> unit

(*e: class type t *)
val unmangled    : string -> t
val with_mangler : (string -> string) -> string -> t
(*e: symbol.mli content *)
(*e: front_rtl/symbol.mli *)
