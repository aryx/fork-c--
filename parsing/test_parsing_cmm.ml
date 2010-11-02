open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_cmm file = 
  if not (file =~ ".*\\.c--") 
  then pr2 "warning: seems not a .c-- file";

(*
  Flag_parsing_php.verbose_lexing := true;
  Flag_parsing_php.verbose_parsing := true;
*)

  let toks = Parse_cmm.tokens file in
  toks +> List.iter (fun x ->
    let s = Scan.tok2str x in
    pr2 s
  );
  ()


let test_parse_cmm file  =
  raise Todo

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
    "-parse_cmm", "   <file>", 
    Common.mk_action_n_arg test_parse_cmm;
    "-tokens_cmm", "   <file>", 
    Common.mk_action_1_arg test_tokens_cmm;
]
