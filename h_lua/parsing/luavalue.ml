(*s: luavalue.ml *)
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
= struct
  (*s: value toplevel *)
  type 'a userdata'  = 'a U.t
  type srcloc = int * string * int (* unique id, filename, linedefined *)
  type value
    = Nil
    | Number   of float
    | String   of string
    | Function of srcloc * func
    | Userdata of userdata
    | Table    of table
  and func  = value list -> value list
  and table = (value, value) Luahash.t
  and userdata  = value userdata'
  and state = { globals : table
              ; fallbacks : (string, value) Hashtbl.t
              ; mutable callstack : activation list
              ; mutable currentloc : Srcmap.location option (* supersedes top of stack *)
              ; startup : initstate
              }
  and initstate =
    { mutable init_strings : (string -> unit) -> unit; mutable initialized : bool }
  and activation = srcloc * Srcmap.location option
  (*x: value toplevel *)
  let rec eq x y = match x, y with
  | Nil,             Nil             -> true
  | Number x,        Number y        -> x = y
  | String x,        String y        -> x = y
  | Userdata x,      Userdata y      -> U.eq eq x y
  | Table x,         Table y         -> x == y
  | Function ((x, _, _), _), 
             Function ((y, _, _), _) -> x = y
  | _,               _               -> false
  (*x: value toplevel *)
  (*s: table definition *)
  module Table = struct
    let create = Luahash.create eq
    let find t ~key:k = try Luahash.find t k with Not_found -> Nil
    let bind t ~key:k ~data:v =
      match v with
      | Nil -> Luahash.remove t k
      | _ -> Luahash.replace t k v
    let of_list l =
      let t = create (List.length l) in
      let _ = List.iter (fun (k, v) -> bind t (String k) v) l in
      t
  end
  (*e: table definition *)
  (*x: value toplevel *)
  let srcloc =
    let n = ref 0 in
    fun ~file ~linedefined:line -> (n := !n + 1; (!n, file, line))
  let lua_func ~file ~linedefined:line f = Function (srcloc file line, f)
  let caml_func = lua_func ~file:"(OCaml)" ~linedefined:(-1)
  (*x: value toplevel *)
  let luastring_of_float x =
    let s = string_of_float x in
    if String.get s (String.length s - 1) = '.' then
      String.sub s 0 (String.length s - 1)
    else
      s

  let rec to_string = function
    | Nil             -> "nil"
    | Number x        -> luastring_of_float x
    | String s        -> s
    | Function (_, _) -> "function"
    | Userdata u      -> U.to_string to_string u
    | Table t         -> "table"
  (*x: value toplevel *)
  type objname = Fallback of string | Global of string | Element of string * value
  let key_matching iter t needle =
    let r = ref None in
    iter (fun k v -> if eq needle v then r := Some k else ()) t;
    !r
  let objname g needle =
    match key_matching Hashtbl.iter g.fallbacks needle with
    | Some s -> Some (Fallback s)
    | None -> match key_matching Luahash.iter g.globals needle with
      | Some (String s) -> Some (Global s)
      | _ ->
          let r = ref None in
          Luahash.iter (fun k v ->
            match !r with
            | None -> (match k, v with
              | String n, Table t ->
                  (match key_matching Luahash.iter t needle with
                  | Some v -> r := Some (Element (n, v))
                  | None -> ())
              | k, v -> ())
            | Some _ -> ()) g.globals;
          !r
  (*x: value toplevel *)
  let activation_strings g ((uid, file, line) as srcloc, current) =
    let first tail = match objname g (Function (srcloc, fun _ -> assert false)) with
    | Some (Fallback n) -> "`" :: n :: "' fallback" :: tail
    | Some (Global n)   -> "function " :: n :: tail
    | Some (Element (t, String n)) -> "function " :: t :: "." :: n :: tail
    | Some (Element (t, v))    -> "function " :: t :: "[" :: to_string v :: "]" :: tail
    | None -> "unknown function" :: tail
    in let last = match current with
    | None -> " defined in file " :: file ::
               (if line > 0 then [" at line "; string_of_int line ] else [])
  (*  | Some (f, l, c) when f = file ->
        [" at line "; string_of_int l; " column "; string_of_int c]
  *)
    | Some (f, l, c) ->
        [" in file "; f; ", line "; string_of_int l; " column "; string_of_int c]
    in match line with
    | 0  -> "main of " :: file :: last
    | -1 -> first [" "; file]
    | _  -> first last
  (*x: value toplevel *)
  exception Projection of value * string
  let projection v s = raise (Projection(v, s))
  type ('a, 'b, 'c) ep = ('a, 'b, 'c) synonym_for_ep 
    = { embed : 'a -> 'b; project : 'b -> 'a; is : 'c -> bool }
  type 'a map  = ('a, value, value) ep
  type 'a mapf = ('a, value list -> value list, value list) ep
  (*x: value toplevel *)
  let userdata = { embed = (fun x -> Userdata x)
                 ; project = (function Userdata x -> x
                                    | v -> raise (Projection (v, U.tname)))
                 ; is = (function Userdata _ -> true | _ -> false)
                 } 

  let string = { embed = (fun s -> String s)
               ; project = (function String s -> s 
                                   | Number x -> luastring_of_float x
                                   | v -> raise (Projection (v, "string")))
               ; is = (function String _ | Number _ -> true | _ -> false)
               } 

  let is_float_literal s = 
    try Luafloat.length (Lexing.from_string s) = String.length s
    with Failure _ -> false
  let pervasive_float = float
  let float = 
    { embed = (fun x -> Number x)
    ; project = (function Number x -> x
                        | String s when is_float_literal s -> float_of_string s
                        | v -> raise (Projection (v, "float")))
    ; is = (function Number _ -> true | String s -> is_float_literal s | _ -> false)
    } 
  (*x: value toplevel *)
  let to_int x = 
    let n = truncate x in
    if pervasive_float n = x then n else raise (Projection (Number x, "int"))

  let int   = { embed = (fun n -> Number (pervasive_float n))
              ; project = (function Number x -> to_int x 
                                  | v -> raise (Projection (v, "int")))
              ; is = (function Number x -> pervasive_float (truncate x) = x | _ -> false)
              } 

  let bool  = { embed = (fun b -> if b then String "t" else Nil)
              ; project = (function Nil -> false | _ -> true)
              ; is = (fun _ -> true)
              } 
  let unit =  { embed = (fun () -> Nil)
              ; project = (function Nil -> () | v -> raise (Projection (v, "unit")))
              ; is = (function Nil -> true | _ -> false)
              } 
  (*x: value toplevel *)
  let enum typename pairs = 
    { embed = (fun v' -> try String (fst (List.find (fun (k, v) -> v = v') pairs))
                         with Not_found -> assert false)
    ; project = (function String k ->
                   (try List.assoc k pairs
                    with Not_found -> raise (Projection (String k, typename)))
                | v -> raise (Projection (v, typename)))
    ; is = (function String k -> List.mem_assoc k pairs | _ -> false)
    } 
  (*x: value toplevel *)
  let option t = { embed = (function None -> Nil | Some x -> t.embed x)
                 ; project = (function Nil -> None | v -> Some (t.project v))
                 ; is = (function Nil -> true | v -> t.is v)
                 }
  let default d t = 
    { embed = t.embed
    ; project = (function Nil -> d | v -> t.project v)
    ; is = (function Nil -> true | v -> t.is v)
    }
  (*x: value toplevel *)
  let list (ty : 'a map) = 
    let table l =
      let n = List.length l in
      let t = Table.create n in
      let rec set_elems next = function
        | [] -> Table.bind t (String "n") (Number (pervasive_float n))
        | e :: es -> ( Table.bind t (Number next) (ty.embed e)
                     ; set_elems (next +. 1.0) es)
      in  (set_elems 1.0 l; Table t)
    in
    let untable (t:table) =
      let n = match Table.find t (String "n") with
      | Number x -> to_int x
      | _ -> Luahash.population t  in
      let rec elems i =
        if i > n then []
        else ty.project (Table.find t (Number (pervasive_float i))) :: elems (i + 1) in
      elems 1
    in { embed = table; project = (function Table t -> untable t
                                          | v -> raise (Projection (v, "list"))); 
         is = (function Table t -> true | _ -> false) }
  let optlist ty = default [] (list ty)
  (*x: value toplevel *)
  let value = { embed = (fun x -> x); project = (fun x -> x); is = (fun _ -> true) }
  let table = { embed = (fun x -> Table x)
              ; project = (function Table t -> t | v -> raise (Projection (v, "table")))
              ; is = (function Table t -> true | _ -> false)
              }
  (*x: value toplevel *)
  let projectRecord ty v = match v with
  | Table t ->
      let rec addpairs (k, v) =
        (string.project v, ty.project v) ::
        try addpairs (Luahash.next t k) with Not_found -> [] in
      (try addpairs (Luahash.first t) with Not_found -> [])
  | _ -> raise (Projection (v, "table (as record)"))
  
  let record ty =
    { embed = (fun pairs ->
               Table (Table.of_list (List.map (fun (k, v) -> (k, ty.embed v)) pairs)))
    ; project = projectRecord ty
    ; is = table.is
    } 
  (*x: value toplevel *)
  let take1 = function  (* take one value from a list of arguments *)
    | [] -> Nil
    | h::t -> h

  let take2 = function [] -> Nil, Nil | v :: vs -> v, take1 vs

  let const f s = f
  let (-->) arg result =
    { embed =   (fun f -> 
                 caml_func (fun args -> [result.embed (f (arg.project (take1 args)))]))
    ; project = (function Function (_, f) -> 
                             fun x -> result.project (take1 (f [arg.embed x]))
                        | v -> raise (Projection (v, "function")))
    ; is = (function Function (_, _) -> true | _ -> false)
    } 
  (*x: value toplevel *)
  let ( **-> ) (firstarg : 'a map) (lastargs : 'b mapf) : ('a -> 'b) mapf =
    let apply (f : 'a -> 'b) args = 
      let h, t = match args with [] -> Nil, [] | h :: t -> h, t in
      let f = f (firstarg.project h) in
      lastargs.embed f t
    in
    let unapp f' =
      fun (x : 'a) -> lastargs.project (function t -> f' (firstarg.embed x :: t)) in
    (* function can match even if args are defaulted, but not if too many args *)
    let is args = 
      let h, t = match args with [] -> Nil, [] | h :: t -> h, t in
      firstarg.is h && lastargs.is t in
    { embed = apply; project = unapp; is = is }

  let results (a_to_values : 'a -> value list) (a_of_values : value list -> 'a) = 
    { embed   = (fun (a:'a) -> fun lua_args -> a_to_values a);
      project = (fun f_lua -> (a_of_values (f_lua []) : 'a));
      is = (function [] -> true | _ :: _ -> false)
    } 

  let (<<) f g = fun x -> f (g x)

  let result r = results (fun v -> [r.embed v]) (r.project << take1)
  let resultvs = results (fun l -> l) (fun l -> l)
  let resultpair a b =
    let em (x, y) = [a.embed x; b.embed y] in
    let pr vs =
      let x, y = match vs with
      | [] -> Nil, Nil
      | [x] -> x, Nil
      | x :: y :: _ -> x, y in
      (a.project x, b.project y) in
    results em pr

  (* other possibilities not exposed in interface *)
  let result2 r1 r2 = results (fun (v1, v2) -> [r1.embed v1; r2.embed v2])
                              ((fun (l1, l2) -> r1.project l1, r2.project l2) << take2)
  let runit =
    results (fun () -> []) 
            (function [] -> () | h :: _ -> raise (Projection (h, "unit result")))
  (*x: value toplevel *)
  let dots_arrow (varargs : 'a map) (result : 'b map) : ('a list -> 'b) mapf =
    let apply (f : 'a list -> 'b) = 
      fun (args : value list) ->
        [result.embed (f (List.map varargs.project args))] in
    let unapp (f' : value list -> value list) =
      fun (args : 'a list) ->
        result.project (take1 (f' (List.map varargs.embed args))) in
    { embed = apply; project = unapp; is = List.for_all varargs.is }


  let func (arrow : 'a mapf) : ('a map) =
    { embed   = (fun (f : 'a) -> caml_func (arrow.embed f))
    ; project = (function Function (_, f) -> (arrow.project f : 'a)
                        | v -> raise (Projection (v, "function")))
    ; is = (function Function(_, _) -> true | _ -> false)
    } 

  let closure (arrow : 'a mapf) : ('a map) =
    { embed   = (fun (f : 'a) -> caml_func (arrow.embed f))
    ; project = (function Function (_, f) -> (arrow.project f : 'a)
                        | Table t as v -> (
      (*s: project table [[t]] into a function *)
      let f = try Table.find t (String "apply")
              with Not_found -> raise (Projection (v, "function"))  in
      match f with
      | Function (_, f) -> arrow.project (fun vs -> f (v :: vs))
      | v -> raise (Projection (v, "'apply' element of table as function"))
      (*e: project table [[t]] into a function *)
      )
                        | v -> raise (Projection (v, "function")))
    ; is = (function Function(_, _) -> true | Table t -> 
       (*s: table [[t]] is a closure *)
       (try
         match Table.find t (String "apply") with
         | Function (_, _) -> true
         | _ -> false
       with Not_found -> false)
       (*e: table [[t]] is a closure *)
                   | _ -> false)
    } 

  let efunc t f = (closure t).embed f
  (*x: value toplevel *)
  type alt = (value list -> value list) * (value list -> bool)
  let alt t f = (t.embed f, t.is)
  let choose alts =
    let run args =
      let f = try fst (List.find (fun (_, is) -> is args) alts)
              with Not_found ->
                let args = (list value).embed args in
                raise (Projection (args, "arguments matching alternatives")) in
      f args in
    caml_func run

  let lf = efunc (list value **-> result (list value)) List.rev
  (*x: value toplevel *)
  let ( <|> ) t t' =
    { project = (fun v -> if t.is v then t.project v else t'.project v)
    ; embed   = t'.embed
    ; is      = (fun v -> t.is v || t'.is v)
    }

  let ( <@ ) t k =
    { project = (fun v -> k (t.project v))
    ; embed   = (fun _ -> assert false)
    ; is      = t.is
    }
  (*x: value toplevel *)
  module StringList = struct
    type t = (string -> unit) -> unit
    let empty f = ()
    let of_list l f = List.iter f l
    let append l1 l2 f = l1 f; l2 f
  end

  let state () = 
   { globals = Table.create 50; fallbacks = Hashtbl.create 10; callstack = [];
     currentloc = None;
     startup = { init_strings = StringList.empty; initialized = false; }
   }

  let at_init g ss =
    if g.startup.initialized then
      (prerr_endline
         "Internal Lua-ML error: called at_init after initialiation was complete";
       exit(1))
    else
      g.startup.init_strings <-
        StringList.append g.startup.init_strings (StringList.of_list ss)

  let initcode g =
    if g.startup.initialized then
      (prerr_endline "Internal Lua-ML error: a naughty client called initcode";
       exit(1))
    else
      let code = g.startup.init_strings in
      begin
        g.startup.initialized <- true;
        g.startup.init_strings <- StringList.empty;
        code
      end
  (*e: value toplevel *)
end
(*e: luavalue.ml *)
