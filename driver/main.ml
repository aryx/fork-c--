(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common

module Common2 = Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*
 * Pad's driver for the different c-- analysis. Here are the important
 * types and analysis. They are mostly in the same order than MAKESUBDIRS
 * in the Makefile:
 * 
 * - DONE Ast.program (in parsing/, and its basic printer Astpp.emit)
 *   functions: Parse_cmm.tokens, Parse_cmm.parse, Driver.parse
 * 
 * - DONE Nast.t (in front_nelab/)
 *   functions: Nast.program
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
(* Types and constants *)
(*****************************************************************************)

type caps = < Cap.exit >

let usage = 
  "usage: c-- [options] [file or dir]"

let version = "0.1"

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(*****************************************************************************)
(* Subsystems actions *)
(*****************************************************************************)

(* filename -> tokens *)
let dump_tokens file =
  Driver.scan file

(* filename -> ast *)
let dump_ast caps file =
  let (_srcmap, ast) = Driver.parse file in
  let s = Ast.show_program ast in
  Console.print caps s;
  ()

(* pretty printer *)
let pp_ast caps file =
  let (srcmap, ast) = Driver.parse file in
  let pp = Astpp.program ast in
  let s = Pp.ppToString 0 pp in
  Console.print caps s;
  ()


(* filename -> ast -> nast *)
let dump_nast caps file =
  let (srcmap, ast) = Driver.parse file in
  let nast = Nast.program ast in
  let s = Nast.show nast in
  Console.print caps s

type res_or_error1 =
  (unit Fenv.Dirty.env' * unit Nelab.compunit) Error.error
[@@deriving show]

(* filename -> ast -> nast -> nelab *)
let dump_nelab caps file =
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
  let s = show_res_or_error1 res_or_error in
  Console.print caps s






let test_x86 (caps : < Cap.stdout; ..>) file =
  let (srcmap, ast) = Driver.parse file in

  let tgt = X86.target in

  let dest = "/tmp/cmm.asm" in
  Logs.info (fun m -> m "writing in %s" dest);
  let asm = 
    let chan = open_out dest in
    X86asm.make Cfgutil.emit chan
  in
  (* pad: ugly *)
  Block._empty_vfp_hook := (fun ptrwidth ->
    Block.relative (Vfp.mk ptrwidth) "empty block" 
      Block.at ~size:0 ~alignment:1;
  );

  Driver.compile
    tgt
    (X86backend.optimizer asm)
    ~exportglobals:true (* ?? *)
    ~src:(srcmap, ast)
    ~asm
    ~validate:true (* ?? *)
    ~swap:false (* ?? give weird error mesage when set to true *);
  Console.print caps "Done";
  ()
  

let test_rtl file =
  (* use Rtldebug ? *)
  raise Todo

(*---------------------------------------------------------------------------*)
(* misc *)
(*---------------------------------------------------------------------------*)

let test_driver_version () =
  Driver.version ()

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
  UCommon.pr2_gen env_and_compunit_maybe;
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

let extra_actions (caps : < Cap.stdout; ..>) = [
    "-dump_tokens", "   <file>", 
    Arg_.mk_action_1_arg dump_tokens;
    "-dump_ast", "   <file>", 
    Arg_.mk_action_1_arg (dump_ast caps);
    "-pp_ast", "   <file>", 
    Arg_.mk_action_1_arg (pp_ast caps);
    "-dump_nast", "  <file>", 
    Arg_.mk_action_1_arg (dump_nast caps);
    "-dump_nelab", "  <file>", 
    Arg_.mk_action_1_arg (dump_nelab caps);

    "-driver_emit_asdl", "   <file>", 
    Arg_.mk_action_1_arg test_emit_asdl;
    "-driver_elab", "  <file>", 
    Arg_.mk_action_1_arg test_driver_elab;
    "-driver_compile", "  <file>", 
    Arg_.mk_action_1_arg test_driver_compile;


    "-test_x86", "  <file>", 
    Arg_.mk_action_1_arg (test_x86 caps);

    "-test_rtl", "  <file>", 
    Arg_.mk_action_1_arg test_rtl;

    "-driver_version", "   ", 
    Arg_.mk_action_0_arg test_driver_version;
]

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs = 
  raise Todo


(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions caps =
 Test_parsing_cmm.actions () @
 extra_actions caps @
 []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main (caps : < caps; Cap.stdout; Cap.stderr; ..>) (argv: string array) :
   Exit.t = 
  let level = ref (Some Logs.Warning) in
  let backtrace = ref false in
  let action = ref "" in

  let options = [
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " trace the main functions";
    "-backtrace", Arg.Set backtrace,
    " show backtraces for erros";
  ] @
  Arg_.options_of_actions action (all_actions caps) @
  [
  "-version",   Arg.Unit (fun () -> 
    UCommon.pr2 (spf "c-- version: %s" version);
    exit 0;
  ), 
 "  guess what";
  ]
  in
  (* This may raise ExitCode *)
  let args = Arg_.parse_options options usage argv in
  Logs_.setup ~level:!level ();
  Logs.info (fun m -> m "ran from %s" (Sys.getcwd()));

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Profiling.profile_code "Main total" (fun () -> 

    (match args with
   
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Arg_.action_list (all_actions caps)) -> 
        Arg_.do_action !action xs (all_actions caps);
        Exit.OK

    | _ when not (String_.empty !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
      (try
        main_action (Fpath_.of_strings (x::xs))
       with exn ->
         if !backtrace
         then raise exn
         else
           (match exn with
           | Failure s | Sys_error s ->
              Logs.err (fun m -> m "c--: %s" s);
              Exit.Err s
           | exn -> raise exn
           )
      )       
    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Arg_.usage usage options; 
        failwith "too few arguments"
    )
  )



(*****************************************************************************)
let _ =
  Cap.main (fun (caps: Cap.all_caps) ->
    let argv = CapSys.argv caps in
    Exit.exit caps (Exit.catch (fun () -> main caps argv))
  )
