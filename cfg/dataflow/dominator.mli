(*s: dominator.mli *)
(*s: dominator.mli  *)
module type GRAPHINFO = sig
  
  type t
  type result
  val getNodeNumber : t -> int
  val getSuccs      : t -> int list array 
  val getPreds      : t -> int list array
  val translate     : int array -> t -> result  

end
(*x: dominator.mli  *)
module type DOMINATORTREE = 
  functor (G : GRAPHINFO) -> sig
    
  val dominatorTree : G.t -> G.result

end
(*x: dominator.mli  *)
type tree = 
    Leaf of Zipcfg.label option * Zipcfg.Rep.labelkind option
  | Node of Zipcfg.label option * Zipcfg.Rep.labelkind option * tree list
module ZipGraph : GRAPHINFO with type t = Zipcfg.graph 
                            and type result = tree
module LengauerTarjan : DOMINATORTREE   
(*e: dominator.mli  *)
(*e: dominator.mli *)
