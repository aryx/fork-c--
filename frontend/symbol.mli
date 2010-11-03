(*s: symbol.mli *)
(*s: class type t *)
class type t = object
    method mangled_text:    string
    method original_text:   string
end 
(*e: class type t *)
val unmangled    : string -> t
val with_mangler : (string -> string) -> string -> t
(*e: symbol.mli *)
