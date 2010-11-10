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
 * - DONE Ast.program (in parsing/, and its printer Astpp.emit)
 *   functions: Parse_cmm.tokens, Parse_cmm.parse, Driver.parse
 *   todo: a vof_ast that pretty prints cleanly
 * 
 * - DONE Nast.t (in front_nelab/)
 *   functions: Nast.program
 *   todo: a vof_nast that pretty prints cleanly
 * 
 * - 'a Nelab.compunit * 'a Fenv.Dirty.env' (in front_nelab/)
 *   functions: Nelab.program taking lots of parameters
 * 
 *   'compunit' contains itself some mentions to Rtl and the 'a variable
 *   is bounded to a polymorphic assembler passed as a parameter to the
 *   'compunit' builder. Here are then the dependent submodules:
 * 
 *   * Rtl.?? (in front_rtl/, and its checker Rtldebug.typecheck, and
 *     its printer in Rtlutil.ToString.rtl)
 * 
 *   * Asm.assembler ?? (in front_asm/ )
 * 
 *   * Fenv.??? (in front_fenv/)
 * 
 * - Cfg.S.cfg and especially Cfg.S.kind (in front_cfg/, and its printer in
 *   Cfg.S.print_node
 *  
 *    * Dag.block ??
 * 
 * - Zipcfg.graph and zgraph (in front_zipcfg/, 
 * 
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

  let validator = 
    fun rtl -> None (* ??? *)
  in
  let assembler = 
    let chan = open_out "/tmp/cmm.dot" in
    (* pad: does not really work :( create empty file *)
    Dotasm.asm ~compress:false ~live:true chan
  in
  let swap = true in (* ??? *)

  let res_or_error = 
    Nelab.program ~swap validator srcmap assembler nast
  in
  (* todo: write a vof_compunit and vof_fenv so can pretty print that *)
  pr2_gen res_or_error
  

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

  let chan = open_out "/tmp/cmm.dot" in
  
  let assembler = 
    (* pad: does not really work :( create empty file *)
    Dotasm.asm ~compress:false ~live:true chan
  in

  let env_and_compunit_maybe = 
    Driver.elab 
      ~swap:true (* ??? *)
      (fun rtl -> None) (* ??? *)
      (srcmap, ast)
      assembler
  in
  pr2_gen env_and_compunit_maybe;
  ()

let test_rtl file =
  (* use Rtldebug ? *)
  raise Todo


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

    "-test_nast", "  <file>", 
    Common.mk_action_1_arg test_nast;
    "-test_nelab", "  <file>", 
    Common.mk_action_1_arg test_nelab;

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
