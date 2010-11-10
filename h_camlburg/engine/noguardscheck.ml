(*s: noguardscheck.ml *)
(*s: modules *)
module S = Spec
open Printf
(*e: modules *)
(*s: auxiliary functions *)
let compose f g x = f (g x)
(*x: auxiliary functions *)
let maximum = List.fold_left max min_int
(*x: auxiliary functions *)
let unused_int_literal ints = 
  if ints = [] then 0 else (maximum ints) + 1
(*x: auxiliary functions *)
let (id_of_string,
     string_of_id,
     unused_string_id,
     used_string_ids) =
  let count  = ref 0 in
  let strmap : (string * int) list ref = ref [] in
  let i_of_s s =
    try List.assoc s !strmap
    with Not_found ->
      incr(count);
      strmap := (s, !count) :: !strmap;
      !count
  in
  let s_of_i i =
    if List.length !strmap >= i then
      fst (List.nth !strmap (i - 1))
    else raise Not_found
  in
  (i_of_s,
   s_of_i,
   (fun () -> incr(count); !count),
   (fun () -> List.map snd !strmap))
(*x: auxiliary functions *)
let unused_char_literal chars =
  if chars = [] then char_of_int 0
  else
    char_of_int ((maximum (List.map int_of_char chars) + 1) mod 255)
(*e: auxiliary functions *)
(*s: variable naming *)
let mk_var_gensym ()=
  let count = ref 0 in
  fun () -> incr(count); "E"^(string_of_int !count)
(*e: variable naming *)
(*s: rule naming *)
let mk_rule_gensym () =
  let count = ref 0 in
  fun () -> incr(count); "rule_"^(string_of_int !count)
(*e: rule naming *)
(*s: twelf rule type *)
type twelf_rule = {
    rule_name : string;
    goal_type : string;
    goal_expr : string;
    type_env  : (string * string) list
  }
(*e: twelf rule type *)
(*s: twelf code generation functions *)
let gen_pattern pattern =
  let gensym = mk_var_gensym() in
  let rec gen = function
    | S.Literal (S.Int    i)    -> (string_of_int i, [])
    | S.Literal (S.String s)    ->
        let str_id = string_of_int (id_of_string s) in
        ("str_lit_"^str_id, [])
    | S.Literal (S.Char   c)    -> 
        ("char_lit_"^(string_of_int (int_of_char c)), [])
    | S.Var (name, S.Term t)    -> (gensym(), [])
    | S.Var (name, S.NonTerm t) ->
        let v = gensym() in
        (v, [v,t])
    | S.Con (name, args)        ->
        let (arg_strs, env) = List.split (List.map gen args) in
        let twelf_con_name  = String.lowercase name
        and args_string     = String.concat " " arg_strs
        and env'            = List.flatten env in
        ("("^twelf_con_name^" "^args_string^")", env')
  in gen pattern
(*x: twelf code generation functions *)
let gen_rule =
  let gensym = mk_rule_gensym() in
  let gen r =
    let (goal, to_prove) = gen_pattern r.S.pattern in
    { rule_name = gensym();
      goal_type = r.S.nonterm;
      goal_expr = goal;
      type_env  = to_prove }
  in gen
(*x: twelf code generation functions *)
let print_rule r =
  let goal_match = r.rule_name^" : match_"^r.goal_type in
  if r.type_env <> [] then
    let precond_str (name, ty) = "<- match_"^ty^" "^name in
    let precond_strs = List.map precond_str r.type_env in
    (printf "%s %s\n  " goal_match r.goal_expr;
     printf "%s.\n\n" (String.concat "\n  " precond_strs))
  else
    printf "%s %s.\n\n" goal_match r.goal_expr
(*x: twelf code generation functions *)
let return_type = function
  | S.Literal (S.Int    i)    -> "int"
  | S.Literal (S.String s)    -> "string"
  | S.Literal (S.Char   c)    -> "char"
  | S.Var (name, S.Term t)    -> t
  | S.Var (name, S.NonTerm t) -> "exp"
  | S.Con (name, args)        -> "exp"
(*x: twelf code generation functions *)
let constructor_types rules =
  let rec accum_types tys = function
    | S.Con (name, args) ->
        let ctor_name = String.lowercase name in
        let ty   = (ctor_name,
                    List.map return_type args @ ["exp"]) in
        let tys' =
          if List.mem_assoc ctor_name tys then
            (assert(List.assoc ctor_name tys = snd ty);
             tys)
          else ty :: tys in
        List.fold_left accum_types tys' args
    | _ -> tys
  in
  let extract_pattern rule = rule.S.pattern in
  List.fold_left accum_types [] (List.map extract_pattern rules)
(*x: twelf code generation functions *)
let print_constructor_type (ctor_name, ctor_arg_types) =
  let args_str = String.concat " -> " ctor_arg_types in
  printf "%s : %s.\n" ctor_name args_str
(*x: twelf code generation functions *)
let goal_types =
  let add_goal t tys = if List.mem t tys then tys else t :: tys in
  let rec add_goals tys = function
    | S.Literal (S.Int    i)    -> tys
    | S.Literal (S.String s)    -> tys
    | S.Literal (S.Char   c)    -> tys
    | S.Var (name, S.Term t)    -> tys
    | S.Var (name, S.NonTerm t) -> add_goal t tys
    | S.Con (name, args)        -> List.fold_left add_goals tys args
  in
  let add_rule_goals tys r =
    add_goals (add_goal r.S.nonterm tys) r.S.pattern
  in
  List.fold_left add_rule_goals []
(*x: twelf code generation functions *)
let atomic_types =
  let add_ty t tys = if List.mem t tys then tys else t :: tys in
  let rec add_tys tys = function
    | S.Literal (S.Int    i)    -> add_ty "int"    tys
    | S.Literal (S.String s)    -> add_ty "string" tys
    | S.Literal (S.Char   c)    -> add_ty "char"   tys
    | S.Var (name, S.Term t)    -> add_ty t        tys
    | S.Con (name, args)        -> List.fold_left add_tys tys args
    | _                         -> tys
  in
  List.fold_left (fun tys r -> add_tys tys r.S.pattern) []
(*x: twelf code generation functions *)
let int_literals =
  let add_int i xs = if List.mem i xs then xs else i :: xs in
  let rec add_ints xs = function
    | S.Literal (S.Int i)    -> add_int i xs
    | S.Con     (name, args) -> List.fold_left add_ints xs args
    | _                      -> xs
  in
  List.fold_left (fun xs r -> add_ints xs r.S.pattern) []
(*x: twelf code generation functions *)
let char_literals =
  let add_char c xs = if List.mem c xs then xs else c :: xs in
  let rec add_chars xs = function
    | S.Literal (S.Char c)   -> add_char c xs
    | S.Con     (name, args) -> List.fold_left add_chars xs args
    | _                      -> xs
  in
  List.fold_left (fun xs r -> add_chars xs r.S.pattern) []
(*x: twelf code generation functions *)
let is_chain_rule rule =
  match rule.S.pattern with
  | S.Var (name, S.NonTerm t) -> true
  | _                         -> false
(*x: twelf code generation functions *)
let gen_chain_rule rules rule =
  let chain_var_type =
    match rule.S.pattern with
    | S.Var (name, S.NonTerm t) -> t
    | _                         -> raise (Invalid_argument "not a chain rule")
  in
  let to_change = List.find_all (fun r -> r.S.nonterm = chain_var_type) rules in
  List.map (fun r -> {r with S.nonterm = rule.S.nonterm}) to_change
(*x: twelf code generation functions *)
let rules_equal r1 r2 =
  r1.S.nonterm = r2.S.nonterm && r1.S.pattern = r2.S.pattern
(*x: twelf code generation functions *)
let rec find_chain_rules_fixpoint rules accum_rules chain_rules =
  let crs =
    List.flatten (List.map (gen_chain_rule (rules @ accum_rules)) chain_rules) in
  let rules_not_eq r1 r2 = not (rules_equal r1 r2) in
  let crs' = List.find_all
      (fun x -> (List.for_all (rules_not_eq x) accum_rules)
             && (List.for_all (rules_not_eq x) rules)) crs in
  if crs' <> [] then
    find_chain_rules_fixpoint rules (accum_rules @ crs') chain_rules
  else accum_rules
(*x: twelf code generation functions *)
let gen_elf spec =
  let chain_rules     = List.find_all is_chain_rule spec.S.rules in
  let non_chain_rules = List.find_all (compose not is_chain_rule) spec.S.rules in
  let twelf_rules     = List.map gen_rule non_chain_rules in
  let ctor_types  = constructor_types spec.S.rules in
  let goal_types  = goal_types spec.S.rules in
  let atomic_types = atomic_types spec.S.rules in
  let chain_fixpoint = 
    List.map gen_rule (find_chain_rules_fixpoint non_chain_rules [] chain_rules) in

  printf "exp : type.\n\n";

  S.StringSet.iter 
    (fun t -> 
      if not (List.mem t ["string"; "int"; "char"]) then
        printf "%s : type.\n%s_lit : %s.\n\n" t t t)
    spec.S.terms;

  (if List.mem "int" atomic_types then
    let int_lits = int_literals spec.S.rules in
    let int_lits' = (unused_int_literal int_lits) :: int_lits in
    printf "int : type.\n";
    List.iter (fun i -> printf "%i : int.\n" i) int_lits';
    printf "\n");

  (if List.mem "string" atomic_types then
    let str_ids = used_string_ids() in
    let str_ids' = unused_string_id() :: str_ids in
    printf "string : type.\n";
    List.iter (fun i -> printf "str_lit_%i : string.\n" i) str_ids';
    printf "\n");

  (if List.mem "char" atomic_types then
    let char_lits = char_literals spec.S.rules in
    let char_lits' = (unused_char_literal char_lits) :: char_lits in
    printf "char : type.\n";
    List.iter (fun c -> printf "char_lit_%i : char.\n" (int_of_char c)) char_lits';
    printf "\n");

  List.iter print_constructor_type ctor_types;
  printf "\n";

  List.iter (fun t -> printf "match_%s : exp -> type.\n" t) goal_types;
  printf "\n";

  List.iter (fun t -> printf "%%mode match_%s +E.\n" t) goal_types;
  printf "%%worlds ()";
  List.iter (fun t -> printf " (match_%s E)" t) goal_types;
  printf ".\n\n";

  List.iter print_rule twelf_rules;

  printf "%% chain rules\n\n";
  List.iter print_rule chain_fixpoint;

  printf "%%covers";
  List.iter (fun t -> printf " (match_%s +E)" t) goal_types;
  printf ".\n"
(*e: twelf code generation functions *)
(*e: noguardscheck.ml *)
