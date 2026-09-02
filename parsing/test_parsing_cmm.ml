open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_cmm caps file = 
(*
  if not (file =~ ".*\\.c--") 
  then UCommon.pr2 "warning: seems not a .c-- file";
*)

  let toks = Parse_cmm.tokens caps file in
  toks |> List.iter (fun x ->
    let s = Scan.tok2str x in
    (* UCommon.pr2 s *)
    print_string s
  );
  ()


let test_parse_cmm caps file  =
(*
  if not (file =~ ".*\\.c--") 
  then UCommon.pr2 "warning: seems not a .c-- file";
*)
  let ast = Parse_cmm.parse caps file in
  print_string (Dumper.dump ast);
  ()

let test_pp_cmm caps file  =
  let ast = Parse_cmm.parse caps file in
  let pp = Astpp.program ast in
  let s = Pp.ppToString 0 pp in
  print_string s;
  ()

let test_dump_cmm caps file  =
  let _ast = Parse_cmm.parse caps file in
  failwith "OLD: AstUtil.sexp_wr_toplevel_list ast chan"
(*
  let s = Common.read_file temp in
  (* Common.cat temp |> List.iter UCommon.pr2; *)
  let sexp_opt = Sexp.parse_str s in
  match sexp_opt with
  | Sexp.Done (t, pos) -> 
      let str = Sexp.to_string_hum t in
      UCommon.pr2 str
  | Sexp.Cont _ ->
      failwith "parse error on sexp"
*)  
