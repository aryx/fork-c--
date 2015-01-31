(*s: front_rtl/symbol.ml *)
(*s: symbol.ml *)
(*s: class type t *)
class type t = object
    method mangled_text:    string
    method original_text:   string
end 
(*e: class type t *)
class unmangled (n:string) : t = 
object(this)
    method original_text = n
    method mangled_text  = n
end
class mangled (mangle:string->string) (n:string) : t = object
    method mangled_text    = mangle n
    method original_text   = n
end
let unmangled n = new unmangled n
let with_mangler m n = new mangled m n
(*e: symbol.ml *)
(*e: front_rtl/symbol.ml *)
