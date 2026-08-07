(*s: front_rtl/symbol.ml *)
(*s: symbol.ml content *)
(*s: class type t *)
class type t = object
    method mangled_text:    string
    method original_text:   string
end 
(*e: class type t *)

(* does not work: [@@deriving show] *)
(* manual *)
let pp fmt (x : t) =
  Format.fprintf fmt
    "{ mangled_text = %S; original_text = %S }"
    x#mangled_text
    x#original_text

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
(*e: symbol.ml content *)
(*e: front_rtl/symbol.ml *)
