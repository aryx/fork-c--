(*s: this.in *)
(* Do not edit - this file is created from this.in through mk(1) 
 * If this file does not compile, check the following files:
 * (1) main2.nw - this.in is defined here
 * (2) mkfile target this.ml - constructs the boot string
 *)

let system          = "@this@"
(*x: this.in *)
let name channel = 
    let s = try let minus = String.rindex system '-' in
                String.sub system 0 minus
            with Not_found -> "not configured"
    in
         output_string channel s
(*x: this.in *)
let version channel = 
    let s = try let minus = String.rindex system '-' in
                String.sub system (minus+1) (String.length system - minus - 1)
            with Not_found -> "not configured"
    in
        output_string channel s
(*e: this.in *)
