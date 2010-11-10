(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)

open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*
 * Pad's driver for the different c-- analysis. Here are the important
 * types and analysis. They are mostly in the same order than MAKESUBDIRS
 * in the Makefile:
 * 
 * - DONE Common.filename
 * 
 * - DONE Ast.program (in parsing/, and its basic printer Astpp.emit)
 *   functions: Parse_cmm.tokens, Parse_cmm.parse, Driver.parse
 * 
 *   todo: a vof_ast that pretty prints cleanly not using the asdl-based
 *    not so good sexp printer
 * 
 * - DONE Nast.t (in front_nelab/)
 *   functions: Nast.program
 * 
 *   todo: a vof_nast that pretty prints cleanly
 * 
 * - DONE 'a Nelab.compunit * 'a Fenv.Dirty.env' (in front_nelab/)
 *   functions: Nelab.program taking lots of parameters
 * 
 *   'compunit' contains itself some mentions to Rtl and the 'a variable
 *   is bounded to a polymorphic assembler passed as a parameter to the
 *   'compunit' builder. Here are the dependent submodules:
 * 
 *   * DONE Elabstmt.stmt
 *     subfunction: Elablstmt.elab_stmts taking some rtl hook,
 *      a region, a fenv, a nast.stmt list and returning a list of
 *      elaborate statements.
 * 
 *     todo: should have some unit tests independent of nelab
 * 
 *   * DONE Rtl.Private.*, especially 'const', 'exp (in front_rtl/, 
 *     and its checker Rtldebug.typecheck, and
 *     its printer in Rtlutil.ToString.rtl)
 *     functions: Nelab.program will build a compunit containing Rtl stuff
 *      in its leaves
 * 
 *     todo: should have some unit tests independent of front_elab/
 * 
 *   * DONE `proc Fenv.env (in front_fenv/)
 *     subfunction: Fenv.clean which takes a Dirty env and return a Clean env
 *     note that the assembler is in the fatenv !!
 * 
 *     todo: should have sone unit tests independent of front_elab/
 * 
 *   * Asm.assembler (in front_asm/ ) see below
 * 
 * 
 * - Cfg.S.cfg and especially Cfg.S.kind (in front_cfg/, and its printer in
 *   Cfg.S.print_node) ??
 *  
 *    * Dag.block ??
 * 
 * - Zipcfg.graph and zgraph (in front_zipcfg/, ) 
 *    ???
 * 
 * - Ast2ir.tgt (in front_ir/, ) ??
 *   function: Ast2ir.translate
 * 
 *   takes a tgt, build from ??? 
 *   a clean fatenv, an optimizer ??, a nelab compunit
 *   and does some side effects on the assembler in the clean fatenv.
 * 
 *    * - Target.?? machine ? t ? (in front_target/, ) ???
 *  
 * - Asm.assembler (in front_asm/)
 * 
 *    * arch/dummy/
 * 
 *    * arch/interpreter
 * 
 *    * arch/x86/
 *       - x86asm.make to build the assembler (need a Cfgutil.emit func)
 *       - x86.target to get a target
 * 
 *    * arch/ppc/
 * 
 *    * arch/...
 * 
 *  - Cfgutil.emit (in assembler/) passed to x86asm.make
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* action mode *)
let action = ref ""

let version = "0.1"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Subsystems actions *)
(*****************************************************************************)

(* filename -> ast *)
let test_driver_parse file =
  let (srcmap, ast) = Driver.parse file in

  (* todo: write a vof_ast so can pretty print it cleanly *)
  let pp = Astpp.program ast in
  let s = Pp.ppToString 0 pp in
  pr2 s;
  ()

(* filename -> ast -> nast *)
let test_nast file =
  let (srcmap, ast) = Driver.parse file in
  let nast = Nast.program ast in
  (* todo: write a vof_nast so can pretty print that *)
  pr2_gen nast


(* filename -> ast -> nast -> nelab *)
let test_nelab file =
  let (srcmap, ast) = Driver.parse file in
  let nast = Nast.program ast in

  (* the assembler is rarely called by the nelab builder. It's part
   * of the returned fatenv but it's not that used.
   *)
  let assembler = Dummyasm.asm in

  let validator = fun rtl -> None (* ??? *) in
  let swap = true in (* ??? *)

  let res_or_error = 
    Nelab.program ~swap validator srcmap assembler nast
  in
  (* todo: write a vof_compunit and vof_fenv so can pretty print that *)
  pr2_gen res_or_error

let test_x86 file =
  let (srcmap, ast) = Driver.parse file in

  let tgt = X86.target in
  let asm = 
    let chan = open_out "/tmp/cmm.asm" in
    X86asm.make Cfgutil.emit chan
  in
  (* pad: ugly *)
  Block._empty_vfp_hook := (fun ptrwidth ->
    Block.relative (Vfp.mk ptrwidth) "empty block" 
      Block.at ~size:0 ~alignment:1;
  );

  Driver.compile
    tgt
    (fun proc -> ()) (* ?? optimizer ? *)
    ~exportglobals:true (* ?? *)
    ~src:(srcmap, ast)
    ~asm
    ~validate:true (* ?? *)
    ~swap:false (* ?? give weird error mesage when set to true *);
  ()
  

let test_rtl file =
  (* use Rtldebug ? *)
  raise Todo

(*---------------------------------------------------------------------------*)
(* misc *)
(*---------------------------------------------------------------------------*)

let test_driver_version () =
  Driver.version ()

let test_driver_scan file =
  Driver.scan file

let test_emit_asdl file =
  let (srcmap, ast) = Driver.parse file in
  Driver.emit_asdl (srcmap, ast)


let test_driver_elab file =
  let (srcmap, ast) = Driver.parse file in

  (* pad: does not really work :( create empty file 
   *   let chan = open_out "/tmp/cmm.dot" in
   *   Dotasm.asm ~compress:false ~live:true chan
   *)
  let assembler = Dummyasm.asm in

  let env_and_compunit_maybe = 
    Driver.elab 
      ~swap:true (* ??? *)
      (fun rtl -> None) (* ??? *)
      (srcmap, ast)
      assembler
  in
  pr2_gen env_and_compunit_maybe;
  ()

let test_driver_compile file =
  let (srcmap, ast) = Driver.parse file in

  let tgt = Dummy.dummy32b' in
  let asm = 
      let chan = open_out "/tmp/cmm.dot" in
      Dotasm.asm ~compress:false ~live:true chan
  in
  (* pad: ugly *)
  Block._empty_vfp_hook := (fun ptrwidth ->
    Block.relative (Vfp.mk ptrwidth) "empty block" 
      Block.at ~size:0 ~alignment:1;
  );

  Driver.compile
    tgt
    (fun proc -> ()) (* ?? optimizer ? *)
    ~exportglobals:true (* ?? *)
    ~src:(srcmap, ast)
    ~asm
    ~validate:true (* ?? *)
    ~swap:true (* ?? *);
  ()


(*---------------------------------------------------------------------------*)
(* The command line actions *)
(*---------------------------------------------------------------------------*)

let extra_actions () = [
    "-driver_parse", "   <file>", 
    Common.mk_action_1_arg test_driver_parse;
    "-driver_scan", "   <file>", 
    Common.mk_action_1_arg test_driver_scan;

    "-driver_emit_asdl", "   <file>", 
    Common.mk_action_1_arg test_emit_asdl;
    "-driver_elab", "  <file>", 
    Common.mk_action_1_arg test_driver_elab;
    "-driver_compile", "  <file>", 
    Common.mk_action_1_arg test_driver_compile;

    "-test_nast", "  <file>", 
    Common.mk_action_1_arg test_nast;
    "-test_nelab", "  <file>", 
    Common.mk_action_1_arg test_nelab;

    "-test_x86", "  <file>", 
    Common.mk_action_1_arg test_x86;

    "-test_rtl", "  <file>", 
    Common.mk_action_1_arg test_rtl;

    "-driver_version", "   ", 
    Common.mk_action_0_arg test_driver_version;
]

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)


let main_action xs = 
  raise Todo


(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
 Test_parsing_cmm.actions () ++
 extra_actions () ++
 []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  Common.cmdline_flags_other () ++
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "qc-- version: %s" version);
    exit 0;
  ), 
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () -> 
    pr2 "version: $Date: 2010/10/26 00:44:57 $";
    raise (Common.UnixExit 0)
    ), 
  "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    "Usage: " ^ Common.basename Sys.argv.(0) ^ 
      " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 

    (match args with
   
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | [] when !action = "-yyy" -> 
        pr2 "yyy"

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )



(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )
