(*s: assembler/cfgutil.ml *)
(*s: cfgutil.ml *)
module A  = Ast 
module DG = Dag
module G  = Zipcfg
module GR = Zipcfg.Rep
module OG = Cfgx.M 
module P  = Proc
module PA = Preast2ir
module T  = Target
module UM = Unique.Map 
(*x: cfgutil.ml *)
let block_name g uid =
  try
    (match UM.find uid (G.to_blocks g) with
    | GR.Entry, _ -> "<entry>"
    | GR.Label ((_, l), _, _), _ -> l)
  with Not_found -> "<unknown-uid>"
(*x: cfgutil.ml *)
let pr = Printf.eprintf
let pr_first = function
  | GR.Entry -> pr "<entry>\n"
  | GR.Label ((_, l), _, _) -> pr "%s:\n" l
let pr_mid m = match m with
  | GR.Instruction  r
  | GR.Stack_adjust r -> pr "%s\n" (Rtlutil.ToString.rtl r)
let lasttype = function
  | GR.Exit        -> "exit"
  | GR.Branch    _ -> "branch"
  | GR.Cbranch   _ -> "cbranch"
  | GR.Mbranch   _ -> "mbranch"
  | GR.Call      _ -> "call"
  | GR.Cut       _ -> "cut"
  | GR.Return    _ -> "return"
  | GR.Jump      _ -> "jump"
  | GR.Forbidden  _ -> "forbidden"
let pr_last g l =
  let succ_names l = String.concat " " (List.map (block_name g) (GR.succs l)) in
  pr "<%s> %s [%s]\n" (lasttype l) (Rtlutil.ToString.rtl (GR.last_instr l))
                      (succ_names l)
let pr_last' l =
  pr "<%s> %s\n" (lasttype l) (Rtlutil.ToString.rtl (GR.last_instr l))
let print_block g (first, tail) =
  pr_first first;
  let rec pr_tail = function
    | GR.Tail (m, tail) ->
        pr_mid m; pr_tail tail
    | GR.Last GR.Exit -> pr "<exit>\n"
    | GR.Last l -> pr_last g l in
  pr_tail tail

let print_cfg g =
  let () = Printf.eprintf "\nBEGIN CFG\n" in
  let () =
    try List.iter (print_block g) (G.postorder_dfs g)
    with Not_found -> Unique.Map.iter (print_block g) (G.to_blocks g) in
  let () = Printf.eprintf "END CFG\n" in
  flush stderr
(*x: cfgutil.ml *)
let matches next = match next with
| None -> (fun _ -> false)
| Some (u, l) -> (fun (u', l') -> Unique.eq u u')

let emit proc cfg call_emit rtl_emit sym_emit =
  let PA.T tgt = proc.P.target in
  let rec seq_emit = function  (* emit sequence of RTLs only *)
    | DG.Rtl r -> rtl_emit r
    | DG.Seq (a, b) -> seq_emit a; seq_emit b
    | DG.Nop -> ()
    | DG.If _ | DG.While _ -> Impossible.impossible "expected linear code sequence" in
  let block b next () =
    let first f () = match f with
    | GR.Entry -> ()
    | GR.Label ((_, l), _, _) -> sym_emit l in
    let middle m () =
      if GR.is_executable m then
        rtl_emit (GR.mid_instr m) in
    let last l () = match l with
    | GR.Branch (_, tgt) when matches next tgt -> ()
    | GR.Cbranch (_, ttgt, ftgt) when not (matches next ftgt) ->
        rtl_emit (GR.last_instr l);
        let (b, r) = tgt.T.machine.T.goto.T.embed proc (proc.P.exp_of_lbl ftgt) in
        seq_emit b;
        rtl_emit r
    | GR.Call c -> call_emit c
    | _ -> rtl_emit (GR.last_instr l) in
    GR.fold_fwd_block first middle last b () in
  G.fold_layout block () cfg
(*x: cfgutil.ml *)
let extend_with preds nodes =
  let rec add_predecessors visited candidates = match candidates with
  | [] -> visited
  | node :: candidates ->
      if List.exists (OG.eq node) visited then
        add_predecessors visited candidates
      else
        add_predecessors (node :: visited) (preds node @ candidates) in
  add_predecessors nodes (List.flatten (List.map preds nodes))

type node = F of GR.first | M of GR.middle | L of GR.last

let numbered_layout_nodes g =
  let add_block (f, t) next (i, nodes') =
    let (i, nodes') = (i+1, (i, F f) :: nodes') in
    let is_next (u', _) = match next with
    | Some (u, _) -> Unique.eq u u'
    | None -> false in
    let rec tail t (i, nodes') = match t with
    | GR.Tail (m, t) -> tail t (i+1, (i, M m) :: nodes')
    | GR.Last (GR.Branch (_, lbl)) when is_next lbl -> (i, nodes') 
    | GR.Last l      -> (i+1, (i, L l) :: nodes') in
    tail t (i, nodes') in
  let _, nodes' = G.fold_layout add_block (0, []) g in
  List.rev nodes'

let id_matches u = function
  | F GR.Entry -> Unique.eq u GR.entry_uid
  | F (GR.Label ((u', _), _, _)) -> Unique.eq u u'
  | L _ | M _ -> false

let cfg2dot ~compress ~live ~name g =
  let () = if live then assert false in
  let nodes = numbered_layout_nodes g in
  let number u = fst (List.find (fun (i, n) -> id_matches u n) nodes) in
  let spr = Printf.sprintf in
  let number_and_rtl (n, node) =
    let instr rtl = spr "N%d: %s" n (Rtlutil.ToString.rtl rtl) in
    match node with
    | F GR.Entry -> "Entry"
    | F (GR.Label ((_, l), _, _)) -> spr "N%d: %s" n l
    | M m -> instr (GR.mid_instr m)
    | L GR.Exit -> "Exit" 
    | L l -> instr (GR.last_instr l) in
  let dotnode node = spr "N%d [label=%S]" (fst node) (number_and_rtl node) in
  let edge_fwd from to' =
    let edge = spr "N%d -> N%d" from to' in
    if live then
      (* edge ^ "[label=\"" ^ Register.SetX.to_string (L.live_in to') ^ "\"]\n" *)
      assert false
    else
      edge ^ "\n" in
  let _edge_bwd from to' = spr "N%d -> N%d [dir=back,style=dotted]\n" to' from in
  let edges_leaving node tail = match snd node with
  | F _ | M _ -> edge_fwd (fst node) (fst node + 1) :: tail
  | L last ->
    let add_edge dir edges u = dir (fst node) (number u) :: edges in
    let tail = List.fold_left (add_edge edge_fwd) tail (GR.succs last) in
    tail in
  let oldv = Rtlutil.ToAST.verbosity Rtlutil.ToAST.Low in
  let openg  = "digraph " ^ name ^ " {\n" in
  let closeg = "}\n" in
  let pagesize = "  page=\"8,10.5\"\n" in
  let compress = if compress then "  ratio=compress\n" else "" in
  let body = List.fold_right (fun n t -> edges_leaving n t)      nodes [closeg] in
  let body = List.fold_right (fun n t -> dotnode n :: "\n" :: t) nodes body     in
  let _ = Rtlutil.ToAST.verbosity oldv in
  String.concat "" (openg :: pagesize :: compress :: body)
(*x: cfgutil.ml *)
let cfg2ast instr g ~name =
  let block b next prev' =
    let mid n prev' = instr (GR.mid_instr n) :: prev' in
    let first n prev' = match n with
    | GR.Label (l,_,_) -> A.LabelStmt (snd l) :: prev'
    | GR.Entry -> prev' in
    let last n prev' = match n with
    | GR.Exit -> A.CommentStmt "exit node!" :: prev'
    | GR.Branch (_, lbl) when matches next lbl -> prev'
    | GR.Forbidden _ -> A.CommentStmt "forbidden to reach this point" :: prev'
    | _ -> instr (GR.last_instr n) :: prev' in
    let rec tail t prev' = match t with
    | GR.Last l -> last l prev'
    | GR.Tail (m, t) -> tail t (mid m prev') in
    let (f, t) = b in
    tail t (first f prev') in
  let oldv = Rtlutil.ToAST.verbosity Rtlutil.ToAST.Low in
  let stmts' = G.fold_layout block [] g in
  let _ = Rtlutil.ToAST.verbosity oldv in
    ( None
    , name
    , []
    , List.rev_map (fun s -> Ast.StmtBody s) stmts'
    , Srcmap.null
    )
(*e: cfgutil.ml *)
(*e: assembler/cfgutil.ml *)
