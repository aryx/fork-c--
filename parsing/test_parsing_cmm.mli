
(* This makes accessible the different test_xxx functions above from 
 * the command line, e.g. '$ pfff -parse_cmm foo.c-- will call the 
 * test_parse_cmm function.
 *)
val actions : unit -> Common.cmdline_actions
