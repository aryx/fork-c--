(* Yoann Padioleau
 * 
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let is_eof tok = 
  match tok with
  | Parse.EOF -> true
  | _ -> false

let tokens file =

  let map = Srcmap.mk () in
  Srcmap.sync map 0 (file, 0, 0);

  Common.with_open_infile file (fun chan -> 
    let lexbuf = Lexing.from_channel chan in
    
    let rec aux acc = 
      let tok = Scan.token lexbuf map in
      
      if is_eof tok
      then List.rev (tok::acc)
      else aux (tok::acc)
    in
    aux []
  )
      
(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let parse file =
  raise Todo
