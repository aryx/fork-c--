
(* Need:
 * - for/exec/wait: to run external gcc/as/ld
 * - open_in: to open the .c-- files to parse
 * - stderr? use Logs instead? stdout?
 * - exit: ??
 *)

type caps = < Cap.forkew; Cap.open_in; Cap.stderr; Cap.stdout; Cap.exit >

val main : < caps; .. > -> string array -> Exit.t
