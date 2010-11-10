(*s: metrics.mli *)
(*s: exposed types *)
type t = {
  byteorder   : Rtl.aggregation ; (* big/little endian, id *)
  wordsize    : int             ; (* bits *)
  pointersize : int             ; (* bits *)
  memsize     : int             ; (* smallest addressable unit, typically 8 *)
  float       : string          ; (* name of float representation (def "ieee754") *)
  charset     : string          ; (* "latin1"  character encoding      *)
}
(*e: exposed types *)
val default : t
val of_ast  : 
  swap:bool -> Srcmap.map -> (Ast.region * Ast.arch) list -> 
  t Error.error
(*e: metrics.mli *)
