(*s: colorgraph.ml *)
(*s: colorgraph.ml  *)
module AU  = Automatonutil
module Dn  = Rtl.Dn
module RTD = Runtimedata
module P   = Proc
module PA  = Preast2ir
module R   = Register
module RP  = Rtl.Private
module RC  = Registerclass.RegisterClass
module RM  = Register.Map
module RS  = Register.Set
module RSX = Register.SetX
module RU  = Rtlutil

(* claude: Nopoly.(=*=) below needs this open; every other .nw-tangled
 * .ml in this tree gets it prepended automatically by the (now-dropped)
 * generator, so it has to be added by hand here. *)
open Nopoly
let ( ++ ) = RS.union
let ( -- ) = RS.diff
let imposs = Impossible.impossible
let impossf fmt = Printf.kprintf Impossible.impossible fmt

let () = Debug.register "colorgraph" "generalized graph coloring allocator"
(*s: imperative lists *)
type ('a,'b) impListItem = { v : 'b
                           ; mutable list : 'a
                           ; mutable next : ('a,'b) impListItem option
                           ; mutable prev : ('a,'b) impListItem option
                           }

type ('a,'b) impListGroup = 'a -> ('a,'b) impListItem option ref
(*x: imperative lists *)
let rec eq_item a b i i' =
  let eq_here i i' = a i.list i'.list && b i.v i'.v in
  let opt eq o o' = match o, o' with
  | None,   None    -> true
  | Some i, Some i' -> eq i i'
  | None,   Some _  -> false
  | Some _, None    -> false in
  let rec eq_left  i i' = eq_here i i' && opt eq_left  i.prev i'.prev in
  let rec eq_right i i' = eq_here i i' && opt eq_right i.next i'.next in
  eq_here i i' && opt eq_right i.next i'.next && opt eq_left i.prev i'.prev
let eq_item a b i i' =
  b i.v i'.v
(*x: imperative lists *)
let ilg_add group list value =
    let item = { v = value
               ; list = list
               ; next = !(group list)
               ; prev = None
               } in
    let () = match item.next with
             | Some n -> n.prev <- Some item
             | None   -> () in
    let () = group list := Some item in
    item

let ilg_switch group list item =
    begin
          (match item.prev with
          | Some p -> p.next <- item.next
          | None   -> group item.list := item.next)
        ; (match item.next with
          | Some n -> n.prev <- item.prev
          | None   -> ())
        ; item.list <- list
        ; item.prev <- None
        ; item.next <- !(group list)
        ; group list := Some item
        ; (match item.next with
          | Some n -> n.prev <- Some item
          | None   -> ())
    end

let rec ilg_iter fn lst =
    match lst with
    | Some n ->
        let next = n.next in    
        let () = fn n     in
        ilg_iter fn next
    | None   -> ()

let rec ilg_map fn lst =
    match lst with
    | Some n -> fn n :: ilg_map fn n.next
    | None   -> []

let rec ilg_fold fn lst rst =
    match lst with
    | Some n -> fn n (ilg_fold fn n.next rst)
    | None   -> rst

let rec ilg_filter fn lst =
    match lst with
    | Some n when fn n -> n :: ilg_filter fn n.next
    | Some n -> ilg_filter fn n.next
    | None   -> []
(*x: imperative lists *)
type tempList = Precolored
              | Initial
              | SimplifyWorklist
              | FreezeWorklist
              | SpillWorklist
              | Spilled
              | Coalesced
              | Colored
              | SelectStack
let str_of_list = function
  | Precolored       -> "Precolored"
  | Initial          -> "Initial"
  | SimplifyWorklist -> "Simplify"
  | FreezeWorklist   -> "Freeze"
  | SpillWorklist    -> "Spill Wlist"
  | Spilled          -> "Spilled Temp"
  | Coalesced        -> "Coalesced"
  | Colored          -> "Colored"
  | SelectStack      -> "Select"

type moveList = CoalescedMove
              | ConstrainedMove
              | FrozenMove
              | WorklistMove
              | ActiveMove
(*e: imperative lists *)
(*x: colorgraph.ml  *)
module G  = Zipcfg
module GR = Zipcfg.Rep
module UM = Unique.Map

let irwk = Rtlutil.ReadWriteKill.sets_promote
let defsm middle =
  let uses, defs, kills = irwk (GR.mid_instr middle) in
  defs ++ kills

let defsl last =
  let uses, defs, kills = irwk (GR.last_instr last) in
  let defs = defs ++ kills in
  let p = R.promote_rxset in
  p (G.union_over_outedges last (fun _ -> R.rset_to_rxset defs)
        (fun {G.node = n'; G.defs = d; G.kills = k} ->
           (R.rset_to_rxset (defs ++ p d ++ p k))))

let usesm middle =
  let uses, _, _ = irwk (GR.mid_instr middle) in
  R.promote_rxset (R.rset_to_rxset uses)
let usesl last =
  let uses, _, _ = irwk (GR.last_instr last) in
  R.promote_rxset (G.add_inedge_uses last (R.rset_to_rxset uses))
(*x: colorgraph.ml  *)
let spill_slot_for proc ((_,_,ms), _, c)  =
  Automaton.allocate proc.Proc.priv (Cell.to_width ms c) "" 1
module type COLOR = sig
  type t 
  type set
  val reg : t -> Rtl.Private.loc 
  val of_reg  : Register.t -> t
  val fits   : ('a, 'b, 'c) Target.t -> Rtl.space -> t -> bool
  val to_string : t -> string
  val subst : (Register.t -> t) -> Rtl.rtl -> Rtl.rtl
  val may_alias : t -> t -> bool  (* share hardware resources *)
  module Set : sig
    val empty : set
    val singleton : t -> set
    val of_list : t list -> set
    val of_regs : Register.Set.t -> set

    val remove : t -> set -> set
    val filter : (t -> bool) -> set -> set
    val exists : (t -> bool) -> set -> bool

    val is_empty : set -> bool
    val choose   : set -> t
    val cardinal : set -> int  (* can't work with infinite sets; must eliminate *)
    val overlap : set -> set -> bool  (* true iff intersection is not empty *)
  end
end

module RegColor : COLOR with type t = Register.t and type set = RS.t = struct
  type t = Register.t
  type set = RS.t
  let reg r = RP.Reg r
  let of_reg r = r
  let fits = Target.fits
  let to_string = RU.ToString.reg
  let subst f = Rtlutil.Subst.reg ~map:f
  let may_alias = RU.MayAlias.regs
  module Set = struct
    include RS
    let of_list l = List.fold_left (fun set reg -> RS.add reg set) RS.empty l
    let of_regs s = s
    let overlap s1 s2 = RS.is_empty (RS.inter s1 s2)
  end
end

module PreMake (Color : COLOR) = struct
  module CS = Color.Set
(*s: graph coloring types *)
module T = Target
(*x: graph coloring types *)
module CompareSpace = struct
    type t = Rtl.space
    let compare = Rtlutil.Compare.space
end
module SpaceMap = Map.Make(CompareSpace)

module CompareEdge = struct
    type t = Register.t * Register.t
    let compare (x, y) (x', y') =
      match Register.compare x x' with
      | 0 -> Register.compare y y'
      | d -> d
end
module EdgeSet = Set.Make(CompareEdge)

let is_tmp target (s,_,_) = Target.is_tmp target s 
(*x: graph coloring types *)
type move  = Register.t * Register.t
module CompareMoves = struct
  type t = move
  let compare (r1, r2) (r1', r2') =
    match Register.compare r1 r1' with
    | 0 -> Register.compare r2 r2'
    | n -> n
end
module MoveSet = Set.Make (CompareMoves)
module MoveMap = Map.Make (CompareMoves)
type color = Color.t
type igNode = (tempList,Register.t) impListItem
type moveWorklist = (moveList, move) impListGroup
type tempWorklist = (tempList, Register.t) impListGroup
type 'a colorGraph = { mutable adjSet : EdgeSet.t
                     ; mutable adjListMap : igNode list RM.t
                     ; mutable degreeMap : int RM.t
                     ; mutable moveListMap : (moveList, move) impListItem list RM.t
                     ; mutable aliasMap : Register.t RM.t
                     ; mutable colorMap : color RM.t
                     ; mutable spills : RS.t
                     ; mutable spillMap : (Automaton.loc * Register.t list) RM.t

                     ; mutable regToItem  : igNode RM.t
                     ; mutable regToClass : RC.t RM.t
                     ; mutable tempLG : tempWorklist
                     ; mutable moveLG : moveWorklist

                     ; mutable allColors : Color.set
                     ; mutable colors    : Color.set SpaceMap.t
                     ; mutable numUses   : int RM.t
                     ; mutable liveRange : int RM.t
                     ; mutable hi : int MoveMap.t
                     ; mutable k : int
                     }
(*x: graph coloring types *)
let possibleColors target cg ((s,_,_) as tmp) =
    try SpaceMap.find s cg.colors
    with Not_found ->
        if is_tmp target tmp then
            let colSet = CS.filter (Color.fits target s) cg.allColors in
            let () = cg.colors <- SpaceMap.add s colSet cg.colors in
            colSet
        else
          let _ = impossf "asked for possible colors of a h/w register" in
          CS.singleton (Color.of_reg tmp)
(*x: graph coloring types *)
let degree_for_temp target cg temp =
  if is_tmp target temp then
    cg.k - CS.cardinal (possibleColors target cg temp)
  else
    0
(*e: graph coloring types *)
(* claude: this used to be wrapped in a Lua/Backplane binding shell,
 * mirroring x86backend.ml's decision not to port Backplane - the real
 * algorithm below never referenced Lua/Interp/V/register itself (only
 * the deleted stageFn table at the very end of this chunk did), so it
 * moves out unchanged into a plain PreMake, and [[ralloc]] at the end of
 * this functor is the new straight-line OCaml driver replacing whatever
 * Lua policy script used to sequence build/makeWorklist/simplify/coalesce/
 * freeze/selectSpill/assignColors/resetProgram/applyColors (that script
 * itself, referenced as Ralloc.color = ColorGraph.color in
 * LUA/lua-cmm-driver/luacompile.nw:667, is not present anywhere in this
 * fork or the upstream checkout - reconstructed from the stage names/
 * signatures here, which match Appel's iterated-register-coalescing
 * algorithm exactly). Old shell, kept for comparison against the port:
 *
 * module GCT : Lua.Lib.USERTYPE with type 'a t = 'a colorGraph = struct
 *     type 'a t = 'a colorGraph
 *     let tname = "Graph Coloring"
 *     let eq _ x y = x =*= y  (* may be dangerous *)
 *     let to_string vs _ = "<cgInfo>"
 * end
 * module Make (BackplaneT : Lua.Lib.TYPEVIEW with type 'a t = 'a Backplane.M.action)
 *             (GCT : Lua.Lib.TYPEVIEW with type 'a t = 'a GCT.t
 *                                   and type 'a combined = 'a BackplaneT.combined)
 *             (ProcT : Lua.Lib.TYPEVIEW with type 'a t = Ast2ir.proc
 *                                       and  type 'a combined = 'a BackplaneT.combined)
 *       : Lua.Lib.USERCODE with type 'a userdata' = 'a BackplaneT.combined =
 *   struct
 *     type 'a userdata' = 'a BackplaneT.combined
 *     module M (Interp : Lua.Lib.CORE
 *                 with type 'a V.userdata' = 'a BackplaneT.combined) = struct
 *         module V = Interp.V
 *         let cgmap = GCT.makemap V.userdata V.projection
 *
 *                 module RT = Register.RT (Interp)
 *                 let register = RT.map
 *)
        (*s: graph coloring builtins *)
            let verb   = 18
            let cfgSay = Verbose.say verb
            let cgSay  = cfgSay

            (*s: list as set *)
            let empty lst = match lst with
              | [] -> true
              | _ -> false

            let listMapFind key listMap =
              try RM.find key listMap with Not_found -> []
            let listMapAdd value key listMap =
              RM.add key (value::(listMapFind key listMap)) listMap
            (*e: list as set *)
            (*s: node observers *)
            let source = fst
            let dest   = snd

            let is_some = function Some _ -> true | None -> false 

            (*
             * singleAssignment returns a (Register.t * Register.t) option
             * First get the assignment, then verify that the source and dest are
             * in the same reg. space
             *)
            let isMoveInstruction t mid =
              (* WE NEED TO DISCUSS HOW TO HANDLE THIS PROPERLY: V IS NOT A SPACE
                 IN THE TARGET, WHICH MEANS THE FUNCTIONS IN TARGET2.NW DON'T WORK *)
              let is_vfp r = Vfp.is_vfp (RP.Reg r) in
              match Rtlutil.RTLType.singleAssignment (GR.mid_instr mid) with
              | Some (((s1,_,_) as r1), ((s2,_,_) as r2)) ->
                (not (is_vfp r1 || is_vfp r2)) &&
                   (RU.Eq.space s1 s2 || (is_tmp t r1 && Target.fits t s1 r2)
                                      || (is_tmp t r2 && Target.fits t s2 r1))
              | _ -> false
            let getMove mid = Rtlutil.RTLType.singleAssignment (GR.mid_instr mid)
            (*e: node observers *)
            (*s: print CGInfo *)
            let regILists cgInfo =
                   [ ("Precolored", !(cgInfo.tempLG Precolored))
                   ; ("Initial", !(cgInfo.tempLG Initial))
                   ; ("SimplifyWorklist", !(cgInfo.tempLG SimplifyWorklist))
                   ; ("FreezeWorklist", !(cgInfo.tempLG FreezeWorklist))
                   ; ("SpillWorklist", !(cgInfo.tempLG SpillWorklist))
                   ; ("spilledTemps", !(cgInfo.tempLG Spilled))
                   ; ("coalescedTemps", !(cgInfo.tempLG Coalesced))
                   ; ("coloredTemps", !(cgInfo.tempLG Colored))
                   ; ("SelectStack", !(cgInfo.tempLG SelectStack))
                   ]
            let regLists cgInfo =
                   [ ("Spills", RS.elements cgInfo.spills)
                (* ; ("AllColors", cgInfo.allColors) *)
                   ]
            let moveILists cgInfo =
                            [ ("coalescedMoves", !(cgInfo.moveLG CoalescedMove))
                            ; ("ConstrainedMoves", !(cgInfo.moveLG ConstrainedMove))
                            ; ("FrozenMoves", !(cgInfo.moveLG FrozenMove))
                            ; ("WorklistMoves", !(cgInfo.moveLG WorklistMove))
                            ; ("ActiveMoves", !(cgInfo.moveLG ActiveMove))
                            ]
            (*x: print CGInfo *)
            let printReg ((s,_,_), i, w) = Printf.sprintf "%c%d" s i
            let printColor = Color.to_string
            let printMove move = printReg (source move)
                               ^ " -> "
                               ^ printReg (dest move)
            let printMap p1 p2 mapName sm =
                cgSay ["    " ^ mapName ^ ":\n"];
                RM.iter (fun tmp s -> cgSay [ "        "
                                              ; p1 tmp
                                              ; " -> "
                                              ; p2 s
                                              ; "\n"
                                              ]) sm
            let printStringMap mapName sm = printMap printReg (fun x -> x) mapName sm
            let printAliasMap  mapName am = printMap printReg printReg     mapName am
            let printColorMap  mapName am = printMap printReg printColor   mapName am
            (*x: print CGInfo *)
            let print_cgInfo cgInfo =
                let _ = cgSay ["\nCGINFO\n"] in
                let printList iter pr (name, itemlist) =
                    let indent = "        " in
                    cgSay ["    "; name; ":\n"]; 
                    iter (fun r -> cgSay [indent; pr r; "\n"])
                              itemlist in
                let printItemList pr (name, itemlist) =
                    match itemlist with
                    | [] -> ()
                    | i  -> printList List.iter pr (name, itemlist) in
                let printItemIList pr (name, itemlist) =
                    match itemlist with
                    | None -> ()
                    | Some _  -> printList (fun fn -> ilg_iter (fun i -> fn i.v))
                                           pr (name, itemlist) in
                let printReglist lst  = printItemList printReg lst in
                let printRegIlist lst  = printItemIList printReg lst in
                let printMoveIlist lst = printItemIList printMove lst in
                let printAdjListMember key lst =
                    cgSay ["        "; printReg key; ": "];
                    List.iter (fun r -> cgSay [", "; printReg r.v]) lst;
                    cgSay ["\n"]; in
                let printAdjListMap am =
                    let _ = cgSay ["    AdjListMap:\n"] in
                    RM.iter printAdjListMember am
                in
                    printAdjListMap cgInfo.adjListMap;
                    List.iter printReglist (regLists cgInfo);
                    List.iter printRegIlist (regILists cgInfo);
                    cgSay ["    K: "; string_of_int cgInfo.k; "\n"];
                    List.iter printMoveIlist (moveILists cgInfo);
                    printAliasMap "aliasMap" cgInfo.aliasMap;
                    printColorMap "colorMap" cgInfo.colorMap;
                    printStringMap "listMap"
                        (RM.fold (fun r i map -> RM.add r (str_of_list i.list) map)
                                 cgInfo.regToItem RM.empty);
                    cgSay ["    spills: "; RS.to_string cgInfo.spills; "\n"]

            let printCGInfo param proc = print_cgInfo param; (proc, false)
            (*e: print CGInfo *)
            (*s: init cgInfo *)
            let cgInfo = 
              let pre = ref None in
              let init = ref None in
              let simp = ref None in
              let freez = ref None in
              let spillw = ref None in
              let spilld = ref None in
              let coal = ref None in
              let colo = ref None in
              let sel = ref None in
              let tempFn = function
                  | Precolored -> pre
                  | Initial -> init
                  | SimplifyWorklist -> simp
                  | FreezeWorklist -> freez
                  | SpillWorklist -> spillw
                  | Spilled -> spilld
                  | Coalesced -> coal
                  | Colored -> colo
                  | SelectStack -> sel      in
              let coalm = ref None in
              let constm = ref None in
              let frzm = ref None in
              let workm = ref None in
              let actvm = ref None in
              let moveFn = function
                  | CoalescedMove   -> coalm
                  | ConstrainedMove -> constm
                  | FrozenMove      -> frzm
                  | WorklistMove    -> workm
                  | ActiveMove      -> actvm in
                 { adjSet = EdgeSet.empty
                 ; regToItem = RM.empty
                 ; regToClass = RM.empty
                 ; adjListMap = RM.empty
                 ; degreeMap = RM.empty
                 ; moveListMap = RM.empty
                 ; aliasMap = RM.empty
                 ; colorMap = RM.empty
                 ; tempLG = tempFn
                 ; spills = RS.empty
                 ; spillMap = RM.empty
                 ; moveLG = moveFn

                 ; colors = SpaceMap.empty
                 ; allColors = CS.empty
                 ; k = 0
                 ; numUses   = RM.empty
                 ; liveRange = RM.empty
                 ; hi = MoveMap.empty
                 }
            (*x: init cgInfo *)
            let allocatable cc = CS.of_regs (cc.Call.pre_nvregs ++ cc.Call.volregs)
            let clearCGInfo cg (cfg, ({Proc.cc = colors} as proc)) = 
              let pre = ref None in
              let init = ref None in
              let simp = ref None in
              let freez = ref None in
              let spillw = ref None in
              let spilld = ref None in
              let coal = ref None in
              let colo = ref None in
              let sel = ref None in
              let tempFn = function
                  | Precolored -> pre
                  | Initial -> init
                  | SimplifyWorklist -> simp
                  | FreezeWorklist -> freez
                  | SpillWorklist -> spillw
                  | Spilled -> spilld
                  | Coalesced -> coal
                  | Colored -> colo
                  | SelectStack -> sel      in
              let coalm = ref None in
              let constm = ref None in
              let frzm = ref None in
              let workm = ref None in
              let actvm = ref None in
              let moveFn = function
                  | CoalescedMove   -> coalm
                  | ConstrainedMove -> constm
                  | FrozenMove      -> frzm
                  | WorklistMove    -> workm
                  | ActiveMove      -> actvm in
              let () = cg.adjSet <- EdgeSet.empty in
              let () = cg.regToItem <- RM.empty in
              let () = cg.adjListMap <- RM.empty in
              let () = cg.degreeMap <- RM.empty in
              let () = cg.moveListMap <- RM.empty in
              let () = cg.aliasMap <- RM.empty in
              let () = cg.colorMap <- RM.empty in
              let () = cg.tempLG <- tempFn in
              let () = cg.spills <- RS.empty in
              let () = cg.spillMap <- RM.empty in
              let () = cg.moveLG <- moveFn in

              let () = cg.allColors <- allocatable colors in
              let () = cg.numUses   <- RM.empty in
              let () = cg.liveRange <- RM.empty in
              let () = cg.hi <- MoveMap.empty in
              let () = cg.k <- CS.cardinal cg.allColors in
              ((cfg, proc), false)
            (*e: init cgInfo *)
            (*s: build *)
            let resetDegree target cg temp =
              cg.degreeMap <- RM.add temp (degree_for_temp target cg temp) cg.degreeMap

            let compete_for_registers t1 t2 target cg =  (* code from KH not yet used *)
               let c1 = RM.find t1 cg.regToClass in
               let c2 = RM.find t2 cg.regToClass in
               let competes = RC.may_alias c1 c2 in
               Debug.eprintf "colorgraph" "Interferes? %b\n" competes;
               competes

            let compete_for_registers (s1,_,_ as t1) (s2,_,_ as t2) target cg =
              let aliases t cs = CS.exists (Color.may_alias (Color.of_reg t)) cs in
              if not (is_tmp target t1) then
                if not (is_tmp target t2) then
                  RU.MayAlias.regs t1 t2
                else (* t2 is temporary, t1 is not *)
                  aliases t1 (possibleColors target cg t2)
              else (* t1 is a temporary *)
                if not (is_tmp target t2) then
                  aliases t2 (possibleColors target cg t1)
                else (* both are temporaries *)
                  if RU.Eq.space s1 s2 then
                    true (* temporaries in the same space compete for registers *)
                  else
                    CS.overlap (possibleColors target cg t1) (possibleColors target cg t2)
            (*x: build *)
            let addEdge target cg u v =
                if (not ((EdgeSet.mem (u, v) cg.adjSet) || (Register.eq u v))) &&
                   compete_for_registers u v target cg
                then begin
                    let () = cg.adjSet <- List.fold_right EdgeSet.add [(u, v); (v, u)] cg.adjSet in
                    let addToAdjList t1 t2 =
                        if is_tmp target t1 (* t1 <> Precolored *)
                        then begin
                            cg.adjListMap <- listMapAdd (RM.find t2 cg.regToItem) t1 cg.adjListMap;
                            cg.degreeMap  <- RM.add t1 (RM.find t1 cg.degreeMap + 1) cg.degreeMap
                        end in
                    let () = addToAdjList u v in
                    let () = addToAdjList v u in
                    ()
              end
            (*x: build *)
            (* claude: [[last]] used to union in only
             * "add_live_spansl l RSX.empty" (defs/uses/spans local to this
             * node), but addInterference's own vis_last below computes its
             * live set as "add_live_spansl last (Live.live_out_last last)"
             * - the real backward-liveness fact, a strictly bigger set
             * whenever a register is live out of this block's terminator
             * without being defined or used anywhere in the whole
             * procedure (a pure pass-through, e.g. a convention-mandated
             * live register on a short, flat function with nothing else to
             * touch it). That gap meant regToItem/regToClass below could
             * be missing a register addInterference then calls addEdge on,
             * crashing addToAdjList's "RM.find t2 cg.regToItem" with
             * Not_found - reproduced on a small 7-argument function with no
             * branches; the existing native.tests corpus never hit it
             * because its multi-block programs happen to touch the same
             * registers elsewhere too. Fixed by mirroring vis_last's own
             * live expression exactly, so both passes agree on what a
             * "last" node's registers are. *)
            let get_regs cfg =
              let first  f rst = R.promote_rxset (G.add_live_spansf f RSX.empty) ++ rst in
              let middle m rst = defsm m ++ usesm m ++ rst in
              let last   l rst = defsl l ++ usesl l ++
                                 R.promote_rxset (G.add_live_spansl l (Live.live_out_last l)) ++
                                 rst in
              let block b rst = GR.fold_fwd_block first middle last b rst in
              G.fold_blocks block RS.empty cfg

            let build cg (cfg, ({Proc.target = PA.T target} as proc)) =
              Debug.eprintf "color" "color::build()\n"; flush stderr;
              let () = if is_some (!(cg.tempLG Initial)) then
                Impossible.impossible "Temps on initial list prematurely" in
              let regs = get_regs cfg in
              (*Debug.eprintf "color" "build regs: %s" (Register.Set.to_string regs);*)
              let regMap =
                RS.fold (fun r map -> resetDegree target cg r;
                                      cg.numUses <- RM.add r 0 cg.numUses;
                                      cg.liveRange <- RM.add r 0 cg.liveRange;
                                      let lst = if is_tmp target r then Initial else Precolored in
                                      RM.add r (ilg_add cg.tempLG lst r) map)
                        regs cg.regToItem in
              let () = cg.regToItem <- regMap in
              let classMap = 
                RS.fold (fun ((s,_,_) as r) map -> RM.add r (RC.mkClass s target) map)
                        regs cg.regToClass in
              let () = cg.regToClass <- classMap in
              if Debug.on "colorgraph" then begin
                Debug.eprintf "colorgraph" "registerclass map \n ";
                RM.iter (fun tmp rclass ->
                           Debug.eprintf "colorgraph" "%s -> %s\n"
                                         (RU.ToString.reg tmp) (RC.to_string rclass))
                        cg.regToClass;
                Debug.eprintf "colorgraph" "\n";
              end;
              (*s: addInterference *)
              let intMapAdd t v map = RM.add t ((try RM.find t map with Not_found -> 0) + v) map in
              let addInterference block =
                let (head, last) = GR.goto_end (GR.unzip block) in
                let addEdges live defs uses =
                  let live = live ++ defs in
                  let addAllEdges def =
                    RS.iter (fun liveNode -> addEdge target cg liveNode def) live in
                  RS.iter addAllEdges defs in
                let add_and_upd live defs uses =
                  ( RS.iter (fun t -> cg.numUses   <- intMapAdd t 1 cg.numUses)   uses
                  ; RS.iter (fun t -> cg.numUses   <- intMapAdd t 1 cg.numUses)   defs
                  ; RS.iter (fun t -> cg.liveRange <- intMapAdd t 1 cg.liveRange) live
                  ; addEdges live defs uses
                  ; uses ++ (live -- defs)
                  ) in
                let rec vis_head h live = match h with
                  | GR.First f -> ()
                  | GR.Head (h, m) ->
                    let defs = defsm m in
                    let uses = usesm m in
                    let live =
                      if isMoveInstruction target m then
                        match getMove m with
                        | Some move -> 
                          (let item = ilg_add cg.moveLG WorklistMove move in
                           cg.moveListMap <- RS.fold (listMapAdd item) (defs ++ uses) cg.moveListMap;
                           live -- uses)
                        | None -> impossf "non-move passed isMoveInstruction()"
                      else
                        live in
                    vis_head h (add_and_upd live defs uses) in
                let vis_last l =
                  let live = R.promote_rxset (G.add_live_spansl last (Live.live_out_last last)) in
                  let defs = defsl l in
                  let uses = usesl l in
                  add_and_upd live defs uses in
                vis_head head (vis_last last) in
              (*e: addInterference *)
              G.iter_blocks addInterference cfg;
              (cfg, proc), true
            (*e: build *)
            (*s: makeWorklist *)
            (*s: temp observers *)
            let adjacent temp cg =
              let (<>) = Pervasives.(<>) in
              List.filter (fun t -> t.list <> SelectStack && t.list <> Coalesced)
                          (listMapFind temp cg.adjListMap)

            let nodeMoves temp cg =
                List.filter (fun t -> t.list =*= ActiveMove || t.list =*= WorklistMove)
                            (listMapFind temp.v cg.moveListMap)

            let moveRelated temp cg =
                not (empty (nodeMoves temp cg))
            (*e: temp observers *)

            let makeWorklist cg proc = 
              let addToLists = function
            (*
                | temp when RM.find temp.v cg.degreeMap >=
                                             RC.cardinal (RM.find temp.v cg.regToClass) ->
            *)
            (* JD MERGE
            *)
                | temp when RM.find temp.v cg.degreeMap >= cg.k ->
            (*
            *)
                    ilg_switch cg.tempLG SpillWorklist temp
                | temp when moveRelated temp cg ->
                    ilg_switch cg.tempLG FreezeWorklist temp
                | temp ->
                   ilg_switch cg.tempLG SimplifyWorklist temp in
              let () = ilg_iter addToLists (!(cg.tempLG Initial)) in
              proc, true
            (*e: makeWorklist *)
            (*s: simplify *)
            (*s: remove temp *)
            let enableMoves temps cg =
              let enableMove move =
                if move.list =*= ActiveMove then
                  let hi_m = MoveMap.find move.v cg.hi - 1 in
                  ( cg.hi <- MoveMap.add move.v hi_m cg.hi
                  ; if hi_m <= 0 then ilg_switch cg.moveLG WorklistMove move
                  ) in
              List.iter (fun temp -> List.iter enableMove (nodeMoves temp cg)) temps
            (*x: remove temp *)
            let decrementDegree cg target temp =
                let d  = RM.find temp.v cg.degreeMap in
                let () = cg.degreeMap <- RM.add temp.v (d - 1) cg.degreeMap in
                if d = cg.k
                then begin
                    enableMoves (adjacent temp.v cg) cg;
                    if moveRelated temp cg
                    then ilg_switch cg.tempLG FreezeWorklist temp
                    else ilg_switch cg.tempLG SimplifyWorklist temp
                end
            (*e: remove temp *)

            let simplify cg (cfg, ({Proc.target = PA.T t} as proc)) =
              (*Debug.eprintf "color" "color::simplify()\n"; flush stderr;*)
              (match !(cg.tempLG SimplifyWorklist) with
              | Some temp ->
                let () = ilg_switch cg.tempLG SelectStack temp in
                let adj = adjacent temp.v cg in
                let () = if RM.find temp.v cg.degreeMap >= cg.k then enableMoves adj cg in
                let () = List.iter (decrementDegree cg t) (adjacent temp.v cg) in
                (cfg, proc), true
              | None -> (cfg, proc), false)
            (*e: simplify *)
            (*s: coalesce *)
            (*s: definition of [[uniq]] *)
            let uniq nodes =
              let map = List.fold_left (fun map i -> RM.add i.v i map) RM.empty nodes in
              RM.fold (fun _ v rst -> v :: rst) map []
            (*e: definition of [[uniq]] *)
            (*s: coalesce move *)
            let addWorklist temp cg =
                if not ((temp.list =*= Precolored) || (temp.list =*= Coalesced) ||
                        (moveRelated temp cg) || (RM.find temp.v cg.degreeMap >= cg.k))
                then ilg_switch cg.tempLG SimplifyWorklist temp
            (*x: coalesce move *)
            (*
            let ok tempt tempr cg =
                   (RM.find tempt.v cg.degreeMap < cg.k)
                || (tempt.list =*= Precolored)
                || (EdgeSet.mem (tempt.v, tempr.v) cg.adjSet)
            *)
            let george move u v cg =
              u.list =*= Precolored && not (v.list =*= Precolored) &&
              let k = List.fold_left (fun k w -> if RM.find w.v cg.degreeMap >= cg.k &&
                                                    not (EdgeSet.mem (u.v, w.v) cg.adjSet)
                                                 then k + 1 else k) 0 (adjacent v.v cg) in
              if k > 0 then (cg.hi <- MoveMap.add move.v k cg.hi; false) else true
            (*x: coalesce move *)
            let conservative neighbors cg =
                cg.k > (List.fold_left
                           (fun k temp ->
                                if RM.find temp.v cg.degreeMap >= cg.k then k + 1 else k)
                            0 neighbors)
            let briggs move u v cg =
              let k =
                List.fold_left (fun k n -> if RM.find n.v cg.degreeMap >= cg.k then k+1 else k)
                               0 (uniq ((adjacent u.v cg) @ (adjacent v.v cg))) in
              if k < cg.k then true else (cg.hi <- MoveMap.add move.v (k - (cg.k - 1)) cg.hi; false)
            (*x: coalesce move *)
            let rec getAlias node cg =
              try
                if node.list =*= Coalesced
                then getAlias (RM.find (RM.find node.v cg.aliasMap) cg.regToItem) cg
                else node.v
              with Not_found -> Impossible.impossible "Not_found in Colorgraph.getAlias"
            (*x: coalesce move *)
            let get m cg = RM.find (getAlias (RM.find m cg.regToItem) cg) cg.regToItem
            let combine target u v cg =
              let () = ilg_switch cg.tempLG Coalesced v in
              let () = cg.aliasMap <- RM.add v.v u.v cg.aliasMap in

              let () =
                if u.list =*= Precolored then
                  let upd move =
                    let x = get (source move.v) cg in
                    let y = get (dest   move.v) cg in
                    if not (x.list =*= Precolored || y.list =*= Precolored) then
                      ( cg.hi <- MoveMap.add move.v 0 cg.hi
                      ; cg.moveListMap <- RM.add u.v (move :: listMapFind u.v cg.moveListMap)
                                                 cg.moveListMap
                      ) in
                  List.iter upd (listMapFind v.v cg.moveListMap)
                else 
                  (* NOTE, UNION WOULD BE BETTER THAN @ HERE and above *)
                  cg.moveListMap <- RM.add u.v (listMapFind u.v cg.moveListMap @
                                                listMapFind v.v cg.moveListMap) cg.moveListMap in
              let losingHiDegNode = RM.find u.v cg.degreeMap >= cg.k &&
                                    RM.find v.v cg.degreeMap >= cg.k in
            (*
              let moves = listMapFind u.v cg.moveListMap @ listMapFind v.v cg.moveListMap in
              let () = cg.moveListMap <- RM.add u.v moves cg.moveListMap in
              let () = enableMoves [v] cg in
            *)
              let () = List.iter (fun temp -> addEdge target cg temp.v u.v
                                            ; decrementDegree cg target temp)
                                 (adjacent v.v cg) in
              ( if (RM.find u.v cg.degreeMap >= cg.k) && (u.list =*= FreezeWorklist)
                then ilg_switch cg.tempLG SpillWorklist u
              ; if losingHiDegNode then enableMoves (adjacent u.v cg) cg
              )
            (*e: coalesce move *)

            let coalesce cg (cfg, ({Proc.target = PA.T target} as proc)) =
              (*Debug.eprintf "color" "color::coalesce()\n"; flush stderr;*)
                (match !(cg.moveLG WorklistMove) with
                | Some move ->
            (*Debug.eprintf "color" "coalescees: %d\n" (ilg_fold (fun _ n -> n + 1)
                          (!(cg.moveLG WorklistMove)) 0); flush stderr;*)
                    let x = get (source move.v) cg in
                    let y = get (dest   move.v) cg in
                    let (u, v) = if y.list =*= Precolored then (y, x) else (x, y) in
                    let () =
                        if eq_item (=*=) Register.eq u v
                        then begin
                             ilg_switch cg.moveLG CoalescedMove move
                           ; addWorklist u cg
                        end else if (v.list =*= Precolored) || (EdgeSet.mem (u.v, v.v) cg.adjSet)
                            then begin
                                 ilg_switch cg.moveLG ConstrainedMove move
                               ; addWorklist u cg
                               ; addWorklist v cg
                        end else if george move u v cg || briggs move u v cg
            (*
                        end else if ((u.list =*= Precolored) &&
                                 (List.fold_left (fun rst t -> (ok t u cg) && rst)
                                                 true (adjacent v.v cg))
                                || ((not (u.list=*= Precolored)) &&
                                    (conservative (uniq ((adjacent u.v cg) @ (adjacent v.v cg)))
                                                  cg)))
            *)
                            then begin
                               let (keep,coal) = if RS.mem u.v cg.spills then (v,u) else (u,v) in
                                 ( ilg_switch cg.moveLG CoalescedMove move
                                 ; combine target keep coal cg
                                 ; addWorklist keep cg
                                 )
                        end else
                            ilg_switch cg.moveLG ActiveMove move in 
                    ((cfg, proc), true)
                | None -> ((cfg, proc),false))
            (*e: coalesce *)
            (*s: freeze *)
            (*s: freeze move *)
            let freezeMoves temp cg =
                let freezeMove move =
                    let x = RM.find (source move.v) cg.regToItem in
                    let y = RM.find (dest   move.v) cg.regToItem in
                    let v = if Register.eq (getAlias y cg) (getAlias temp cg)
                            then RM.find (getAlias x cg) cg.regToItem
                            else RM.find (getAlias y cg) cg.regToItem in
                    let () = ilg_switch cg.moveLG FrozenMove move in
                    if (Pervasives.(<>) v.list Precolored && empty (nodeMoves v cg)) &&
                       (RM.find v.v cg.degreeMap < cg.k)
                    then ilg_switch cg.tempLG SimplifyWorklist v in
                List.iter freezeMove (nodeMoves temp cg)
            (*e: freeze move *)

            let freeze cg proc =
              Debug.eprintf "color" "color::freeze()\n"; flush stderr;
               (match !(cg.tempLG FreezeWorklist) with
                    | Some temp ->
                        ( ilg_switch cg.tempLG SimplifyWorklist temp
                        ; freezeMoves temp cg
                        ; (proc, true)
                        )
                    | None -> (proc, false))
            (*e: freeze *)
            (*s: selectSpill *)
            let selectSpill cg (cfg, proc) =
              Debug.eprintf "color" "color::selectSpill()\n"; flush stderr;
              let lst = !(cg.tempLG SpillWorklist) in
              match lst with
              | None   -> ((cfg, proc), false)
              | Some t ->
                let cheaper ({v = v} as t) (cost, _ as best) =
                  let i2f = float_of_int in
                  let cost' = (i2f (RM.find v cg.numUses)) /.
                              (i2f (RM.find v cg.liveRange) *. i2f (RM.find v cg.degreeMap)) in
                  if cost' <. cost then (cost', t) else best in
                let (_, spillee) = ilg_fold cheaper lst (infinity, t) in
                if RS.mem spillee.v cg.spills then
                  impossf "Graph coloring ralloc chose to spill a previously spilled temp...."
                else
                  ( ilg_switch cg.tempLG SimplifyWorklist spillee
                  ; freezeMoves spillee cg
                  ; ((cfg, proc), true)
                  )
            (*e: selectSpill *)
            (*s: assignColors *)
            exception GraphColoring

            let assignColors cg (cfg, ({Proc.target = PA.T target} as proc)) =
              Debug.eprintf "color" "color::assignColors()\n"; flush stderr;
              (*s: colorTemp *)
              let colorLookup temp cmap =
                let t = getAlias (RM.find temp cg.regToItem) cg in
                if is_tmp target t then
                  try RM.find t cmap
                  with Not_found -> impossf "colorLookup(): uncolored temp %s" (RU.ToString.reg temp)
                else
                  Color.of_reg t in

              let colorTemp temp =
                let removeUsedColor colors adjTemp =
                  let t = RM.find (getAlias adjTemp cg) cg.regToItem in
                  if t.list=*= Colored || t.list=*= Precolored
                  then CS.remove (colorLookup t.v cg.colorMap) colors
                  else colors in
                let possColors = possibleColors target cg temp.v in
                let okColors =
                  List.fold_left removeUsedColor possColors (listMapFind temp.v cg.adjListMap) in

                if CS.is_empty possColors then
                  impossf "No hardware register available for %s " (printReg temp.v)
                else if CS.is_empty okColors then
                  if RS.mem temp.v cg.spills then 
                     (*s: complain of spilling a spill temp, then die *)
                     begin
                       Cfgutil.print_cfg cfg;
                       print_cgInfo cg;
                       impossf "Spilling a spill temp: %s" (printReg temp.v);
                     end 
                     (*e: complain of spilling a spill temp, then die *)
                  else ilg_switch cg.tempLG Spilled temp
                else
                  begin
                    cg.colorMap <- RM.add temp.v (CS.choose okColors) cg.colorMap;
                    ilg_switch cg.tempLG Colored temp;
                  end in
              (*e: colorTemp *)
              let colorSelectStack () =
                let () = ilg_iter colorTemp (!(cg.tempLG SelectStack)) in
              if is_some (!(cg.tempLG SelectStack)) then
                Impossible.impossible "Failed to color a temp?"   in
              let colorCoalescedTemps () =
                let setColor temp = 
                  cg.colorMap <- RM.add temp.v (colorLookup temp.v cg.colorMap) cg.colorMap in
                ilg_iter setColor (!(cg.tempLG Coalesced)) in
              let () = colorSelectStack () in
              let () = match !(cg.tempLG Spilled) with
                       | None -> colorCoalescedTemps ()
                       | _    -> () in
              ((cfg, proc), true)
            (*e: assignColors *)
            (*s: applyColors *)
            (*s: define span updating functions *)
            let update_spans upd cfg =
              let block b = 
                let (head, last) = GR.goto_end (GR.unzip b) in
                let live_inl  = Live.live_in_last last in
                let rec vis_head h live_out = match h with
                  | GR.First (GR.Label ((_, lbl), _, {contents = Some ss}) as f) ->
                      RTD.upd_spans (upd (G.add_live_spansf f live_out)) ss
                  | GR.First _ -> ()
                  | GR.Head (h, m) -> vis_head h (Live.live_in_middle live_out m) in
                ( match last with
                  | GR.Call {GR.cal_spans = Some ss; cal_i = i} -> RTD.upd_spans (upd live_inl) ss
                  | _ -> ()
                ; vis_head head live_inl
                ) in
              G.iter_blocks block cfg
            (*e: define span updating functions *)
            let applyColors cg (cfg, ({Proc.target = PA.T target; Proc.var_map = vMap} as proc)) =
              let find (((s,_,_), i, _) as t) =
                try RM.find t cg.colorMap
                with Not_found -> impossf "Uncolored temp %c%d" s i in
              let subst_reg t = if is_tmp target t then find t else Color.of_reg t in
              let rewrite_rtl = Color.subst subst_reg in
              let cfg =  G.map_rtls rewrite_rtl cfg in
              let rewrite_span_loc live_in l =
                let guard = function RP.Reg r -> is_tmp target r | _ -> false in
                let map l = match l with
                  | RP.Reg tmp ->
                      if RSX.mem (Register.Reg tmp) live_in then
                            Color.reg (find tmp)
                      else
                        raise RTD.DeadValue
                  | _      -> impossf "GC reg allocator emitting RT data: guard failed" in
                Rtlutil.Subst.loc_of_loc ~guard ~map l in
              ( update_spans rewrite_span_loc cfg
              ; ((cfg, proc), true)
              )
            (*e: applyColors *)
            (*s: resetProgram *)
            (*s: insert spills *)
            let reads_writes rtl loc =
              let read  reg (r,w) = (r || Register.eq loc reg, w) in
              let write reg (r,w) = (r, w || Register.eq loc reg) in
              Rtlutil.ReadWrite.fold_promote ~read ~write rtl (false,false)
            (*x: insert spills *)
            let updateBlock proc spillees uid block (blockMap, spillMap, newTemps) =
              let len = ilg_fold (fun _ rst -> rst + 1) spillees 0 in
              Debug.eprintf "color" "  color::updateBlock(%d)\n" len; flush stderr;
              let PA.T tgt = proc.Proc.target in
              (*s: define update functions for each type of node in the cfg *)
              let updateInstr map_rtl instr spillee
                              (node, loads, spillMap, spills, newTemps as accum) =
                let spillee = spillee.v in
                let reads, writes = reads_writes instr spillee in
                if reads || writes then 
                  let (mem, spill_regs) = RM.find spillee spillMap in
                  let tmp   = Talloc.Multiple.reg_like proc.Proc.temps spillee in
                  let subst = fun x -> if Register.eq x spillee then tmp else x in 
                  let map = Rtlutil.Subst.reg ~map:subst in
                  let loads  =
                    if reads then tgt.T.machine.T.reload proc mem tmp :: loads else loads in
                  let spills =
                    if writes then tgt.T.machine.T.spill proc tmp mem :: spills else spills in
                  let spillMap = RM.add tmp (mem, tmp::spill_regs) spillMap in
                  let newTemps = tmp::newTemps in
                  (map_rtl map, loads, spillMap, spills, newTemps)
                else accum in

              let updateLast spillee (last, loads, spillMap, spills, newTemps) =
                match updateInstr (fun map -> G.map_rtll map map last) (GR.last_instr last) spillee
                                  (last, loads, spillMap, [], newTemps) with
                | (_, _, _, [], _) as x -> x
                | _ -> impossf "GC reg allocator trying to spill from a last node" in

              let updateMiddle spillee (middle, loads, spillMap, spills, newTemps) =
                updateInstr (fun map -> G.map_rtlm map middle) (GR.mid_instr middle) spillee
                            (middle, loads, spillMap, spills, newTemps) in
              (*e: define update functions for each type of node in the cfg *)


              let (head, last) = GR.goto_end (GR.unzip block) in
              let m = (proc, tgt.T.machine, proc.P.exp_of_lbl) in
              let add_nodes shuffles (g : G.zgraph) =
                let add_subgraph g block =
                  let (subg, _) = G.block2cfg m block in
                  G.splice_focus_entry g (G.unfocus subg) in
                List.fold_left add_subgraph g shuffles in
              let rec fold_block g h spillMap newTemps = match h with
                | GR.First _ -> (G.to_blocks (G.unfocus g), spillMap, newTemps)
                | GR.Head (h, m) ->
                  let (middle, loads, spillMap, spills, newTemps) = 
                    ilg_fold updateMiddle spillees (m, [], spillMap, [], newTemps) in
                  let g = add_nodes spills g in
                  let ((head, tail), blockmap) = G.openz g in
                  let tail = GR.Tail (middle, tail) in
                  let g = G.tozgraph ((head, tail), blockmap) in
                  let g = add_nodes loads g in
                  fold_block g h spillMap newTemps in
              let (last, loads, spillMap, spills, newTemps) = 
                ilg_fold updateLast spillees (last, [], spillMap, [], newTemps) in
              (*s: verify that there are no spills *)
              (match spills with
               | [] -> ()
               | _  -> imposs "colorgraph: unexpected spill while allocating last node");


              (*
                let (head, last) = GR.goto_end (GR.unzip block) in
                let add_inst_lst = List.fold_left (fun tl ld -> GR.Tail (GR.Instruction ld, tl)) in
                let rec vis_head h tail spillMap newTemps = match h with
                  | GR.First f      -> (UM.add uid (f, tail) blockMap, spillMap, newTemps)
                  | GR.Head  (h, m) ->
                    let (middle, loads, spillMap, spills, newTemps) = 
                      ilg_fold updateMiddle spillees (m, [], spillMap, [], newTemps) in
                    let tail = List.fold_left add_inst_lst tail spills in
                    let tail = GR.Tail (middle, tail) in
                    let tail = List.fold_left add_inst_lst tail loads in
                    vis_head h tail spillMap newTemps in
                let (last, loads, spillMap, _, newTemps) = 
                  ilg_fold updateLast spillees (last, [], spillMap, [], newTemps) in
                let tail = List.fold_left add_inst_lst (GR.Last last) loads in
                vis_head head tail spillMap newTemps
              *)
              (*e: verify that there are no spills *)
              let g = G.tozgraph ((GR.First (GR.first (GR.unzip block)), GR.Last last), blockMap) in
              fold_block (add_nodes loads g) head spillMap newTemps
            (*x: insert spills *)
            let rewrite cfg ({Proc.target = PA.T target} as proc) spillees =
              let spillMap =
                ilg_fold (fun s map -> RM.add s.v (spill_slot_for proc s.v, []) map)
                         spillees RM.empty in


              let rewrite_span_loc live_in l =
                let guard = function RP.Reg r -> is_tmp target r && RM.mem r spillMap
                                   | _ -> false in
                let map l = match l with
                  | RP.Reg tmp ->
                      if RSX.mem (Register.Reg tmp) live_in then
                        Rtl.Dn.loc (AU.aloc (fst (RM.find tmp spillMap)) (Register.width tmp))
                      else raise RTD.DeadValue
                  | _ -> impossf "guard failed" in
                Rtlutil.Subst.loc_of_loc ~guard ~map l in
              let () = update_spans rewrite_span_loc cfg in
              let blocks = G.to_blocks cfg in
              Debug.eprintf "color" "  color::rewrite() -- done with updateSpans\n"; flush stderr;
              let (blocks, spillMap, newTemps) =
                UM.fold (updateBlock proc spillees) blocks (UM.empty, spillMap, []) in
              (G.of_blocks blocks, spillMap, newTemps)
            (*e: insert spills *)

            let resetProgram cg (cfg, proc) =
              Debug.eprintf "color" "color::resetProgram()\n"; flush stderr;
              (* handle addition of new temps to the cfg (those in spilledTemps) *)
              let (cfg, newSpillMap, newTemps) = rewrite cfg proc (!(cg.tempLG Spilled)) in
              Debug.eprintf "color" "  color::resetProgram() -- done with rewrite\n"; flush stderr;
              let () = if is_some (!(cg.tempLG Initial))
                       then Impossible.impossible "temps still on initial list" in
              let () = List.iter (fun t -> cg.spills <- RS.add t cg.spills) newTemps in
              let () = cg.spillMap <- RM.fold RM.add newSpillMap cg.spillMap         in

              let () = cg.tempLG Precolored := None in
              let () = cg.tempLG Initial := None in
              let () = cg.tempLG SimplifyWorklist := None in
              let () = cg.tempLG FreezeWorklist := None in
              let () = cg.tempLG SpillWorklist := None in
              let () = cg.tempLG Spilled := None in
              let () = cg.tempLG Coalesced := None in
              let () = cg.tempLG Colored := None in
              let () = cg.tempLG SelectStack := None in

              let () = cg.moveLG CoalescedMove   := None in
              let () = cg.moveLG ConstrainedMove := None in
              let () = cg.moveLG FrozenMove      := None in
              let () = cg.moveLG WorklistMove    := None in
              let () = cg.moveLG ActiveMove      := None in

              let () = cg.adjSet <- EdgeSet.empty in
              let () = cg.regToItem <- RM.empty in
              let () = cg.adjListMap <- RM.empty in
              let () = cg.degreeMap <- RM.empty in
              let () = cg.moveListMap <- RM.empty in
              let () = cg.aliasMap <- RM.empty in
              let () = cg.colorMap <- RM.empty in
              ((cfg, proc), true)
            (*x: resetProgram *)
            let haveSpilledTemps cg proc = (proc, is_some (!(cg.tempLG Spilled)))
            (*x: resetProgram *)
            let updateProgram cg (cfg, proc) =
              let coalesced = ilg_fold (fun n set -> MoveSet.add n.v set)
                                       (!(cg.moveLG CoalescedMove)) MoveSet.empty in
              let filter_copies (first, tail) =
                let is_copy middle =
                   match getMove middle with
                   | Some rs -> MoveSet.mem rs coalesced
                   | _       -> false in
                let rec filt head tail = match tail with
                  | GR.Tail (m, t) when is_copy m -> filt head t
                  | GR.Tail (m, t)                -> filt (GR.Head (head, m)) t
                  | GR.Last _                     -> GR.zipht head tail in
                filt (GR.First first) tail in
              ((G.of_blocks (UM.map filter_copies (G.to_blocks cfg)), proc), true)
            (*e: resetProgram *)
        (*x: graph coloring builtins *)
        (* claude: dropped the Lua module-registration table this chunk used
         * to end with, exposing build/makeWorklist/.../applyColors to Lua
         * under "ColorGraph" - see the comment above [[graph coloring
         * builtins]]'s opening. [[ralloc]] below calls the same functions
         * directly. Old table, kept for comparison against the port:
         *
         * let proc = ProcT.makemap V.userdata V.projection
         * let ( **-> ) = V.( **-> )
         * let stageFn = V.efunc (cgmap **-> proc **-> V.resultpair proc V.bool)
         * let graph_coloring_module =
         *   [ "build",            stageFn build
         *   ; "makeWorklist",     stageFn makeWorklist
         *   ; "simplify",         stageFn simplify
         *   ; "coalesce",         stageFn coalesce
         *   ; "freeze",           stageFn freeze
         *   ; "selectSpill",      stageFn selectSpill
         *   ; "assignColors",     stageFn assignColors
         *   ; "haveSpilledTemps", stageFn haveSpilledTemps
         *   ; "clearCGInfo",      stageFn clearCGInfo
         *   ; "resetProgram",     stageFn resetProgram
         *   ; "updateProgram",    stageFn updateProgram
         *   ; "applyColors",      stageFn applyColors
         *   ; "printCG",          stageFn printCGInfo
         *   ; "cgInfo",           cgmap.V.embed cgInfo
         *   (* not used anywhere except maybe for debugging:
         *    "setRegisters", V.efunc (V.value **-> V.result V.unit)
         *      (fun value -> cgInfo.luaColorOverride <- value;
         *                    cgInfo.colors <- SpaceMap.empty) *)
         *   ]
         *
         * let init = Interp.register_module "ColorGraph" graph_coloring_module
         *)
        (*e: graph coloring builtins *)

  (* claude: the driver that used to be a Lua policy script (see the comment
   * at the top of [[graph coloring builtins]]) - Appel's iterated register
   * coalescing main loop: build the interference graph, repeatedly prefer
   * simplify over coalesce over freeze over spilling a candidate until none
   * of those can make progress, assign colors, and - if that produced any
   * spills - rewrite the program to materialize them and start over from
   * build; otherwise remove the now-redundant coalesced moves and rewrite
   * every temp to its assigned color. *)
  let ralloc _ (cfg, proc) =
    let cg = cgInfo in
    let rec fixpoint (cfg, proc) =
      let (cfg, proc), changed = simplify cg (cfg, proc) in
      if changed then fixpoint (cfg, proc) else
      let (cfg, proc), changed = coalesce cg (cfg, proc) in
      if changed then fixpoint (cfg, proc) else
      let (cfg, proc), changed = freeze cg (cfg, proc) in
      if changed then fixpoint (cfg, proc) else
      let (cfg, proc), changed = selectSpill cg (cfg, proc) in
      if changed then fixpoint (cfg, proc) else
      (cfg, proc) in
    let rec main (cfg, proc) =
      let (cfg, proc), _ = clearCGInfo cg (cfg, proc) in
      let (cfg, proc), _ = build cg (cfg, proc) in
      let (cfg, proc), _ = makeWorklist cg (cfg, proc) in
      let (cfg, proc) = fixpoint (cfg, proc) in
      let (cfg, proc), _ = assignColors cg (cfg, proc) in
      let (cfg, proc), spilled = haveSpilledTemps cg (cfg, proc) in
      if spilled then
        let (cfg, proc), _ = resetProgram cg (cfg, proc) in
        main (cfg, proc)
      else
        let (cfg, proc), _ = updateProgram cg (cfg, proc) in
        applyColors cg (cfg, proc) in
    main (cfg, proc)
end

module XXX = PreMake(RegColor)
include XXX
(*e: colorgraph.ml  *)
(*e: colorgraph.ml *)
