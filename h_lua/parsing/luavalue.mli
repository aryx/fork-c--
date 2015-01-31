(*s: luavalue.mli *)
(*s: signatures *)
type ('a, 'b, 'c) ep = { embed : 'a -> 'b; project : 'b -> 'a; is : 'c -> bool }
type ('a, 'b, 'c) synonym_for_ep = ('a, 'b, 'c) ep 
  = { embed : 'a -> 'b; project : 'b -> 'a; is : 'c -> bool }
(*x: signatures *)
module type S = sig
  type 'a userdata'
  type srcloc
  type initstate
  type value
    = Nil
    | Number   of float
    | String   of string
    | Function of srcloc * func
    | Userdata of userdata
    | Table    of table
  and func  = value list -> value list (* can also side-effect state *)
  and table = (value, value) Luahash.t
  and userdata  = value userdata'
  and state = { globals : table
              ; fallbacks : (string, value) Hashtbl.t
              ; mutable callstack : activation list
              ; mutable currentloc : Srcmap.location option (* supersedes top of stack *)
              ; startup : initstate
              }
  and activation = srcloc * Srcmap.location option

  val caml_func : func -> value (* each result unique *)
  val lua_func  : file:string -> linedefined:int -> func -> value
  val srcloc    : file:string -> linedefined:int -> srcloc (* must NOT be reused *)
  val eq        : value -> value -> bool
  val to_string : value -> string
  val activation_strings : state -> activation -> string list
  type objname = Fallback of string | Global of string | Element of string * value
  val objname : state -> value -> objname option
     (* 'fallback', 'global', or 'element', name *)

  val state : unit -> state (* empty state, without even fallbacks *)
  val at_init : state -> string list -> unit  (* run code at startup time *)
  val initcode : state -> (string -> unit) -> unit (* for the implementation only *)
(*x: signatures *)
  module Table : sig
    val create : int -> table
    val find   : table -> key:value -> value   (* returns Nil if not found *)
    val bind   : table -> key:value -> data:value -> unit
    val of_list : (string * value) list -> table
  end
(*x: signatures *)
  exception Projection of value * string
  val projection : value -> string -> 'a
  type ('a, 'b, 'c) ep = ('a, 'b, 'c) synonym_for_ep 
    = { embed : 'a -> 'b; project : 'b -> 'a; is : 'c -> bool }
  type 'a map  = ('a, value, value) ep
  type 'a mapf  (* used to build function maps that curry/uncurry *)
(*x: signatures *)
  val float    : float  map
  val int      : int    map
  val bool     : bool   map
  val string   : string map
  val userdata : userdata map
  val unit     : unit   map
(*x: signatures *)
  val option : 'a map -> 'a option map
(*x: signatures *)
  val default : 'a -> 'a map -> 'a map
(*x: signatures *)
  val list    : 'a map -> 'a list map   (* does not project nil *)
  val optlist : 'a map -> 'a list map   (* projects nil to empty list *)
(*x: signatures *)
  val value  : value map
  val table  : table map
(*x: signatures *)
  val record : 'a map -> (string * 'a) list map
(*x: signatures *)
  val enum   : string -> (string * 'a) list -> 'a map
(*x: signatures *)
  val ( -->  ) : 'a map  -> 'b map  -> ('a -> 'b) map
  val ( **-> ) : 'a map  -> 'b mapf -> ('a -> 'b) mapf
  val result   : 'a map  -> 'a mapf
  val resultvs : value list mapf                   (* functions returning value lists*)
  val resultpair:'a map  -> 'b map  -> ('a * 'b)       mapf
  val dots_arrow:'a map  -> 'b map  -> ('a list -> 'b) mapf     (* varargs functions *)
  val results  : ('a -> value list) -> (value list -> 'a) -> 'a mapf  
                                    (* 'a represents multiple results (general case) *)
  val func     : 'a mapf -> 'a map                 (* function *)
  val closure  : 'a mapf -> 'a map                 (* function or table+apply method *)
  val efunc    : 'a mapf -> 'a -> value            (* efunc f = (closure f).embed *)
(*x: signatures *)
  type alt                              (* an alternative *)
  val alt    : 'a mapf -> 'a -> alt     (* create an alternative *)
  val choose : alt list -> value        (* dispatch on type/number of args *)
(*x: signatures *)
  val ( <|> ) : 'a map -> 'a map -> 'a map 
  val ( <@ ) : 'a map -> ('a -> 'b) -> 'b map   (* apply continuation after project *)
end
(*x: signatures *)
module type USERDATA = sig
  type 'a t                             (* type parameter will be Lua value *)
  val tname : string  (* name of this type, for projection errors *)
  val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val to_string : ('a -> string) -> 'a t -> string
end
(*e: signatures *)
module Make (U : USERDATA) : S with type 'a userdata'  = 'a U.t 
(*e: luavalue.mli *)
