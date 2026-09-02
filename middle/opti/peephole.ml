(*s: peephole.ml *)
(*s: peephole.ml  *)
module D  = Dataflow.F
module G  = Zipcfg
module GR = Zipcfg.Rep
module P  = Proc
module PA = Preast2ir
module RP = Rtl.Private
module RU = Rtlutil
module Dn = Rtl.Dn
module Up = Rtl.Up
module SS = Strutil.Set
module T  = Target

module ToString = RU.ToString
(*s: [[Peephole]] utilities *)
let () =
  Debug.register "peephole-search"
    "search for a substitutable location in peephole optimizer"

let wrap = 
  if Debug.on "peephole-search" then
    (fun succ l resume ->
      Printf.eprintf "Substituting for %s\n" (ToString.loc (Up.loc (fst l)));
      succ l (fun () ->
        (Printf.eprintf "Abandoning %s\n" (ToString.loc (Up.loc (fst l))); resume())))
  else
    (fun succ -> succ)

let wrap2 = 
  if Debug.on "peephole-search" then
    (fun succ l l' resume ->
      Printf.eprintf "Substituting for %s and %s\n" (ToString.loc (Up.loc (fst l))) 
        (ToString.loc (Up.loc (fst l')));
      succ l l' (fun () ->
        (Printf.eprintf "Abandoning %s and %s\n" (ToString.loc (Up.loc (fst l)))
                                     (ToString.loc (Up.loc (fst l'))); resume())))
  else
    (fun succ -> succ)
(*x: [[Peephole]] utilities *)
let is_copy rtl = match Dn.rtl rtl with
| RP.Rtl [(RP.Const (RP.Bool true),
           RP.Store(RP.Reg _, RP.Fetch(RP.Reg _, _), _))] -> true
| _ -> false
(*e: [[Peephole]] utilities *)
(*x: peephole.ml  *)
let peephole = "peephole"  (* avoid allocating again and again... *)
let () = Debug.register peephole "peephole optimizer"
let debug = Debug.on peephole
(*x: peephole.ml  *)
let tx ((g, proc) : Ast2ir.proc) =
  let PA.T tgt = proc.target in
  let replace =
    if debug then
      (fun rtl old ->
        Printf.eprintf "Replacing old rtl %s\n     with new rtl %s\n"
          (ToString.rtl old) (ToString.rtl rtl);
        rtl) 
    else
      (fun rtl _ -> rtl) in
  let ok =
    if debug then
      (fun i -> if tgt.T.is_instruction i then true
                else (Printf.eprintf "### rejected substitution %s\n"
                           (ToString.rtl i); false))
    else
      tgt.T.is_instruction in
  (*s: propagation of available expressions and replacement of RTLs *)
  (*s: utility functions that substitute expressions for locations *)
  let slocs = RU.Subst.Fetch.rtl in
  let subst1 (loc, e) rtl =
    Simplify.rtl (slocs ~guard:(RU.Eq.loc loc) ~fetch:(fun _ _ -> e) rtl) in
  let subst2 (loc,e) (loc',e') rtl =
    Simplify.rtl (slocs ~guard:(fun l -> RU.Eq.loc l loc || RU.Eq.loc l loc')
                        ~fetch:(fun l _ -> if RU.Eq.loc l loc then e else e') rtl) in
  (*x: utility functions that substitute expressions for locations *)
  let candidates avail rtl =
    let add loc pairs = match loc with
    | RP.Reg _ when not (List.mem_assoc loc pairs) ->  (* don't make duplicates *)
        (match Avail.in_loc avail loc with
        | Some e -> (loc, e) :: pairs
        | None -> pairs)
    | _ -> pairs in
    RU.Fold.LocFetched.rtl add rtl [] in
  (*x: utility functions that substitute expressions for locations *)
  let rec search list succ fail = match list with
  | [] -> fail()
  | x :: xs -> succ x (fun () -> search xs succ fail) in

  let rec search_pairs list succ fail = match list with
  | [] -> fail ()
  | x :: xs -> search xs (fun x' resume -> succ x x' resume)
                         (fun () -> search_pairs xs succ fail) in
  (*e: utility functions that substitute expressions for locations *)
  (*s: code that may mutate the RTL at [[node]] *)
  let search avail update rtl =
   (*s: do our best to use [[update]] to replace [[rtl]] *)
   let try_pair le le' resume =
     let rtl' = subst2 le le' rtl in
     if ok rtl' then update rtl' else resume () in
   let try_one le resume =
     let rtl' = subst1 le rtl in
     if ok rtl' then update rtl' else resume () in
   (*x: do our best to use [[update]] to replace [[rtl]] *)
   let cs = candidates avail (Dn.rtl rtl) in
   search_pairs cs (wrap2 try_pair) (fun () -> search cs (wrap try_one) (fun () -> None))
   (*e: do our best to use [[update]] to replace [[rtl]] *)
  in
  let middle_out avail m = match m with
  | GR.Stack_adjust _ -> None
  | _ ->
      let rtl = GR.mid_instr m in
      if debug then
        Printf.eprintf "=> Considering %s\n" (ToString.rtl rtl);
      let update new_rtl = Some (G.single_middle (G.new_rtlm (replace new_rtl rtl) m)) in
      search avail update rtl in
  let last_outs avail l = 
    let rtl = GR.last_instr l in
    if debug then
      Printf.eprintf "=> Considering %s\n" (ToString.rtl rtl);
    let update new_rtl = Some (G.single_last (G.new_rtll (replace new_rtl rtl) l)) in
    search avail update rtl in
  (*e: code that may mutate the RTL at [[node]] *)
  (*e: propagation of available expressions and replacement of RTLs *)
  { D.name = "peephole"; D.middle_out = middle_out; D.last_outs = last_outs }

let _ = Debug.register "avail-dataflow" "computation of available expressions"

(* claude: upstream's Backplane.of_dataflow (LUA/lua-cmm-driver/backplane.nw:899)
 * is just this lift from a graph rewrite to a procedure rewrite. This fork
 * dropped the rest of Backplane (see arch/x86/x86backend.ml's header
 * comment on why), so inline the one function actually needed here
 * instead of porting the whole module. *)
let of_dataflow graph_rewrite _ (g, proc) =
  let g, changed = graph_rewrite g in
  (g, proc), changed

let subst_forward v gproc =
  let pass = D.a_t Availpass.analysis (tx gproc) in
  let pass = if Debug.on "avail-dataflow" then D.debug Avail.to_string pass else pass in
  of_dataflow (D.rewrite pass ~entry_fact:Avail.unknown) v gproc

let sequential v (g,proc) =
  let pass = D.a_t Availpass.analysis (tx (g,proc)) in
  D.run_anal Availpass.analysis Avail.unknown g;
  of_dataflow (D.rewrite_solved pass ~entry_fact:Avail.unknown) v (g,proc)
(*e: peephole.ml  *)
(*e: peephole.ml *)
