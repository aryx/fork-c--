open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_cmm file = 
  if not (file =~ ".*\\.c--") 
  then pr2 "warning: seems not a .c-- file";

  let toks = Parse_cmm.tokens file in
  toks +> List.iter (fun x ->
    let s = Scan.tok2str x in
    pr2 s
  );
  ()


let test_parse_cmm file  =
  if not (file =~ ".*\\.c--") 
  then pr2 "warning: seems not a .c-- file";

  let ast = Parse_cmm.parse file in
  pr2_gen ast;
  ()

let test_pp_cmm file  =
  let ast = Parse_cmm.parse file in
  let pp = Astpp.program ast in
  let s = Pp.ppToString 0 pp in
  pr2 s;
  ()

let test_dump_cmm file  =
  let ast = Parse_cmm.parse file in

  let temp = Common.new_temp_file "cmm" "sexp" in
  Common.with_open_outfile temp (fun (_, chan) ->
    AstUtil.sexp_wr_toplevel_list ast chan;
  );
  let s = Common.read_file temp in
  (* Common.cat temp +> List.iter pr2; *)
  let sexp_opt = Sexp.parse_str s in
  match sexp_opt with
  | Sexp.Done (t, pos) -> 
      let str = Sexp.to_string_hum t in
      pr2 str
  | Sexp.Cont _ ->
      failwith "parse error on sexp"
  



(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
    "-parse_cmm", "   <file>", 
    Common.mk_action_1_arg test_parse_cmm;
    "-tokens_cmm", "   <file>", 
    Common.mk_action_1_arg test_tokens_cmm;
    "-pp_cmm", "   <file>", 
    Common.mk_action_1_arg test_pp_cmm;
    "-dump_cmm", "   <file>", 
    Common.mk_action_1_arg test_dump_cmm;

]
