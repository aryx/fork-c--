(*s: dominator.ml *)
(*s: dominator.ml  *)
module type GRAPHINFO = sig
  
  type t
  type result
  val getNodeNumber : t -> int
  val getSuccs      : t -> int list array 
  val getPreds      : t -> int list array
  val translate      : int array -> t -> result
  
end
(*x: dominator.ml  *)
module type DOMINATORTREE = 
functor (G : GRAPHINFO) -> sig
    
  val dominatorTree : G.t -> G.result

end
(*x: dominator.ml  *)
type tree = 
    Leaf of Zipcfg.label option * Zipcfg.Rep.labelkind option
  | Node of Zipcfg.label option * Zipcfg.Rep.labelkind option * tree list
(*x: dominator.ml  *)
module LengauerTarjan : DOMINATORTREE   = 
  functor (G : GRAPHINFO) -> struct
(*x: dominator.ml  *)
  module A = Array
  module L = List
(*x: dominator.ml  *)
let dominatorTree graph = 
(*x: dominator.ml  *)
    let nodeNumber = G.getNodeNumber graph 
    and succs =      G.getSuccs graph 
    and preds =      G.getPreds graph in 
(*x: dominator.ml  *)
    let vertex =     A.init nodeNumber (fun i -> -1)
    and parent =     A.init nodeNumber (fun i -> -1)
    and bucket =     A.init nodeNumber (fun i -> [])
    and dfnum =      A.init nodeNumber (fun i -> -1)
    and semi =       A.init nodeNumber (fun i -> -1)
    and ancestor =   A.init nodeNumber (fun i -> -1)
    and idom =       A.init nodeNumber (fun i -> -1)
    and samedom =    A.init nodeNumber (fun i -> -1) 
    and counter =    ref 0 in 
(*x: dominator.ml  *)
(* claude: unvisited nodes are marked with dfnum = -1, not 0 - 0 is dfnum's
 * own root's *real* number, so testing "= 0" here would treat the root as
 * still-unvisited on any later edge back into it and re-run the whole dfs
 * from there, corrupting vertex/parent/counter (TODO/dominator.nw bug). *)
let rec dfs p n =
      if A.get dfnum n = -1 then (
        A.set dfnum   n !counter ; 
        A.set vertex !counter n ; 
        A.set parent  n  p ;
        counter := !counter + 1 ;
        L.iter (fun w -> dfs n w) (A.get succs n)
      ) in
(*x: dominator.ml  *)
    let ancestorWithLowestSemi v = 
      let u = ref v 
      and v = ref v in 
      while A.get ancestor !v != -1 do
        if A.get dfnum (A.get semi !v) < A.get dfnum (A.get semi !u)
        then u := !v ;
        v := A.get ancestor !v ;
      done ;
      !u
(*x: dominator.ml  *)
    and link p n = 
      A.set ancestor n p in 
(*x: dominator.ml  *)
let dominators () =
(*x: dominator.ml  *)
dfs (-1) 0 ;
(*x: dominator.ml  *)
      for i = !counter - 1 downto 1 do 
        
        let n  = A.get vertex i in  
        let p  = A.get parent n in  
        let s  = ref p 
        and s' = ref p in 
(*x: dominator.ml  *)
        (* claude: semi/bucket used to be set *inside* this L.iter, once per
         * predecessor instead of once after considering all of them - bucket
         * then got [[n]] added once per predecessor (duplicated, and into
         * whatever intermediate, not-yet-final [[s]] each iteration had) -
         * TODO/dominator.nw bug. Appel's algorithm sets both once, after the
         * loop over preds(n) has settled on the true minimum. *)
        L.iter
          (fun v ->
            if dfnum.(v) <= dfnum.(n)
            then s' := v
            else s' := A.get semi (ancestorWithLowestSemi v) ;
            if A.get dfnum !s' < A.get dfnum !s then  s := !s' ;
          ) (A.get preds n) ;
        A.set semi n !s ;
        A.set bucket !s (n :: A.get bucket !s) ;
(*x: dominator.ml  *)
        link p n ;
(*x: dominator.ml  *)
        L.iter 
          (fun v -> 
            let y = ancestorWithLowestSemi v in
            if A.get semi y = A.get semi v 
            then A.set idom v p 
            else A.set samedom v y ;
          ) (A.get bucket p) ;
      
        A.set bucket p [] ;
(*x: dominator.ml  *)
      done;
(*x: dominator.ml  *)
      for i = 1 to !counter - 1 do 
        
        let n = A.get vertex i in 
        if A.get samedom n != -1
        then A.set idom n (A.get idom (A.get samedom n)) 
        
      done ; in
(*x: dominator.ml  *)
    dominators () ; 
(* We should not have the graph appearing here ...  *)
    G.translate idom graph  

end
(*x: dominator.ml  *)
(* claude: rewritten against the current Zipcfg (this file predates the
 * fork's reorg): [[Branch]]/[[Cbranch]]/[[Mbranch]] no longer carry
 * [[label option]]s (see zipcfg.mli), and label-list search with a
 * hand-rolled [[=*=]] equality is no longer needed - [[Rep.succs]] already
 * gives a block's successor uids directly, and [[Rep.id]]/[[Unique.Map]]
 * give a proper uid -> index map instead of an O(n) position search. Also
 * fixed: [[getPreds]]'s "for i = 0 to max" was one past the end of a
 * [[max]]-length array (Invalid_argument on any non-trivial graph), and it
 * was O(n^2) besides; [[translate]]'s "find idom num" filtered idom's
 * *values* for ones equal to num, i.e. it returned [num; num; ...] (one
 * copy of num per child) instead of the children's own indices, so the
 * tree it built recursed into [[num]] again forever. *)
(* claude: kept at file level, not nested inside [[ZipGraph]], so that
 * [[Query]] further down can number a graph's blocks the exact same way
 * [[ZipGraph]] does - [[ZipGraph]]'s own signature ascription (below)
 * hides everything but [[GRAPHINFO]]'s four values, so nested helpers
 * would not be reachable from outside it. *)

(* claude: node 0 must be the entry block - LengauerTarjan's [[dfs]] starts
 * numbering at node 0 and treats it as the tree's root. Despite the name,
 * [[Zipcfg.postorder_dfs]] lists the entry *first*: its own local [[next]]
 * only conses a node once every remaining sibling in [[children]] has
 * been fully walked via CPS, and the outermost call's remaining-siblings
 * list is what's left of *entry's* children - so entry's own cons is the
 * last one to fire, and since each cons prepends to the accumulator
 * threaded through every deeper call, "last to cons" means "ends up at
 * the head of the final list". (A first version of this file assumed the
 * opposite - "postorder" reads as "children before parent" - and
 * reversed the list, which silently put some other node at index 0 and
 * made every dominates_idx/back-edge query here wrong; verified against
 * the actual block order by tracing a 2-node graph by hand, and against
 * fold_layout's use of postorder_dfs, which only makes sense - laying
 * a procedure's blocks out for emission - if the entry comes first.) *)
let blocks_of graph = Array.of_list (Zipcfg.postorder_dfs graph)

let index_of blocks =
  let n = Array.length blocks in
  let m = ref Unique.Map.empty in
  for i = 0 to n - 1 do m := Unique.Map.add (Zipcfg.Rep.id blocks.(i)) i !m done;
  !m

module ZipGraph : GRAPHINFO with type t = Zipcfg.graph
                            and type result = tree = struct

  module A = Array
  module L = List
  module G = Zipcfg
  module B = Zipcfg.Rep

  type t = G.graph
  type result = tree

  let getNodeNumber graph = A.length (blocks_of graph)

  let getSuccs graph =
    let bs = blocks_of graph in
    let idx = index_of bs in
    let succs_of b =
      L.map (fun u -> Unique.Map.find u idx) (B.succs (B.last (B.unzip b))) in
    A.map succs_of bs

  let getPreds graph =
    let succs = getSuccs graph in
    let preds = A.make (A.length succs) [] in
    A.iteri (fun p ss -> L.iter (fun s -> preds.(s) <- p :: preds.(s)) ss) succs;
    preds

  let translate idom graph =
    let bs = blocks_of graph in
    let n = A.length bs in
    let name i = (B.blocklabel bs.(i), B.blockkind bs.(i)) in
    let children_of p =
      let acc = ref [] in
      for i = n - 1 downto 0 do
        if i <> p && idom.(i) = p then acc := i :: !acc
      done;
      !acc in
    let rec build i =
      let (lbl, kind) = name i in
      match children_of i with
      | [] -> Leaf (lbl, kind)
      | cs -> Node (lbl, kind, L.map build cs) in
    build 0

end

(* claude: not part of TODO/dominator.nw - a friendlier query layer on top
 * of [[ZipGraph]]/[[LengauerTarjan]] for consumers that want dominance
 * questions answered ("does A dominate B?", "what are this loop's back
 * edges?") rather than the label tree [[ZipGraph.translate]] builds, e.g.
 * opti/licm.ml and opti/strength_reduction.ml's natural-loop detection.
 * Spliced into the tail of the existing "dominator.ml" chunk (no new
 * chunk of its own) since it postdates the original .nw. *)
module Query = struct
  module A = Array
  module L = List
  module G = Zipcfg
  module B = Zipcfg.Rep
  module IS = Set.Make (struct type t = int let compare = compare end)

  (* Reuses ZipGraph's node numbering (getNodeNumber/getSuccs/getPreds) but
   * keeps the raw idom array instead of folding it into a [[tree]]. *)
  module IdomGraph = struct
    include ZipGraph
    type result = int array
    let translate idom (_graph : Zipcfg.graph) = idom
  end
  module Idom = LengauerTarjan (IdomGraph)

  type t = {
    blocks : B.block array;      (* index -> block, entry at index 0 *)
    idom   : int array;          (* idom.(i) = index of immediate dominator, -1 for the entry *)
    succs  : int list array;
    preds  : int list array;
  }

  let analyze graph =
    { blocks = blocks_of graph;
      idom   = Idom.dominatorTree graph;
      succs  = ZipGraph.getSuccs graph;
      preds  = ZipGraph.getPreds graph }

  (* does node [[a]] dominate node [[b]]? (every node dominates itself) *)
  let dominates_idx t a b =
    let rec walk i = i = a || (t.idom.(i) >= 0 && walk t.idom.(i)) in
    walk b

  (* A loop's [[body]] includes its [[header]]. [[preheader]] is set only
   * when the header has exactly one predecessor outside the loop, and
   * that predecessor has no other successor - i.e. control can only ever
   * reach the loop by falling straight through it, so an invariant
   * computation can be moved there verbatim with no new block and no
   * edge to redirect. Anything less tidy (a header reachable from
   * several places outside the loop, or a would-be preheader that also
   * branches elsewhere) is reported with [[preheader = None]]; it still
   * describes a real loop, just not one this simple scheme can hoist
   * into. *)
  type loop = {
    header    : B.block;
    body      : B.block list;
    preheader : B.block option;
  }

  (* nodes that can reach [[n]] without going through [[h]], plus [[h]]
   * itself - the standard natural-loop construction for a back edge
   * n -> h with h dominating n (Aho/Sethi/Ullman, ch. 9). *)
  let natural_loop t n h =
    let body = ref (IS.add h (IS.singleton n)) in
    let rec add = function
      | [] -> ()
      | m :: rest ->
          if IS.mem m !body then add rest
          else (body := IS.add m !body; add (t.preds.(m) @ rest))
    in
    add t.preds.(n);
    !body

  let loops t =
    let back_edges = ref [] in
    A.iteri (fun n succs ->
      L.iter (fun h -> if dominates_idx t h n then back_edges := (n, h) :: !back_edges) succs)
      t.succs;
    let by_header = Hashtbl.create 16 in
    L.iter (fun (n, h) ->
      let prev = try Hashtbl.find by_header h with Not_found -> [] in
      Hashtbl.replace by_header h (n :: prev))
      !back_edges;
    Hashtbl.fold (fun h ns acc ->
      let body = L.fold_left (fun acc n -> IS.union acc (natural_loop t n h)) IS.empty ns in
      let external_preds = L.filter (fun p -> not (IS.mem p body)) t.preds.(h) in
      let preheader = match external_preds with
        | [p] when t.succs.(p) = [h] -> Some t.blocks.(p)
        | _ -> None
      in
      { header = t.blocks.(h);
        body = L.map (fun i -> t.blocks.(i)) (IS.elements body);
        preheader } :: acc)
      by_header []
end
(*e: dominator.ml  *)
(*e: dominator.ml *)
