(*s: luautil.ml *)
module MakeLib (C: Lua.Lib.CORE) = struct
    module V = C.V
    let ( **-> ) = V.( **-> )
    let ( **->> ) x y = x **-> V.result y

    (*s: utility functions *)
    let split path =
        let sep    = ':'                in    (* path separator *)
        let length = String.length path in
        let rec loop start =
            if start >= length then
                []
            else 
                let next = try String.index_from path start sep with
                           Not_found -> length
                in           
                    String.sub path start (next-start) :: loop (next+1) 
        in
            loop 0 
    (*x: utility functions *)
    let lookup   g f     = try Luahash.find g.V.globals (V.String f)
                           with Not_found -> V.Nil
    let dofile   g file  = C.apply (lookup g "dofile")   g  file
    let dostring g str   = C.apply (lookup g "dostring") g  str

    let defaultdir       = Filename.concat This.install_dir "lib/qc--"
    let luapath          = "QCMMLUAPATH"
    let defaultpath      = ".::*"                  (*XXX fix XXX*)
    (*x: utility functions *)
    let dosearchfile g file boot =
      let path = try Sys.getenv luapath with Not_found -> defaultpath in
      let rec search = function
        | "*" :: dirs -> 
            (match boot with
             | None   -> search dirs
             | Some t ->
                 let cont =
                   try let s = Luahash.find t (V.String file) in (fun () -> dostring g [s])
                   with Not_found -> (fun () -> search dirs) in
                 cont ()   (* call to dostring must be outside of handler *)
            )
        | dir :: dirs -> 
            let d = if dir =$= "" then defaultdir else dir in
            let p = Filename.concat d file in
            if Sys.file_exists p then dofile g [V.String p] else search dirs
        | [] -> C.error
                  (Printf.sprintf "dosearchfile: can't find `%s' in path `%s'" file path)
      in
          if Filename.is_relative file then search (split path) 
          else dofile g [V.String file]
    (*x: utility functions *)
    let catch f args =
      try f args
      with ex ->
        let exstr = Printexc.to_string ex in
        Printf.eprintf "Caml exception: %s\n" exstr;
        []

    let catch_lua = function
      | V.Function (_, f) :: args -> catch f args
      | v                 :: args -> V.projection v     "function"
      | []                        -> V.projection V.Nil "function"
    (*x: utility functions *)
    module U = Unix
    let redir fh =
      let original_fh = U.dup fh in
      (fun file ->
        flush_all();
        match file with
          Some f ->
            let ch = open_out f in
            U.dup2 (U.descr_of_out_channel ch) fh;
            close_out ch
        | None ->
            U.dup2 original_fh fh
      )
      
    let redirect_stdout = redir U.stdout
    let redirect_stderr = redir U.stderr
    (*x: utility functions *)
    let file_exists fname = try Unix.access fname [Unix.F_OK]; true with _ -> false
    (*x: utility functions *)
    let rec files_nonwhite_equivalent f1 f2 =
      let finish answer = answer in
      try
        let ch1 = try open_in f1 with _ -> open_in "/dev/null" in
        let finish answer = (close_in ch1; finish answer) in
        try
          let ch2 = try open_in f2 with _ -> open_in "/dev/null" in
          let finish answer = (close_in ch2; finish answer) in
          finish (compare_channels ch1 ch2)
        with _ -> finish false
      with _ -> finish false
    and compare_channels ch1 ch2 =          
      try
        let ch1 = ch1, ref 0 in
        let ch2 = ch2, ref 0 in
        let sp i = i == int_of_char ' '
        and input_chr ch = int_of_char (
          match input_char ch with
          | ' ' | '\t' | '\n' | '\r' -> ' '
          | c -> c) in
        let rec get_char (ch,buf) =
          let last = !buf in
          let rec scan_forward () =
            try let chr = input_chr ch in
                if sp last && sp chr then scan_forward ()
                else buf := chr
            with End_of_file -> buf := -1 in
          scan_forward (); last in
        let rec cmp_char() =
          let c1 = get_char ch1
          and c2 = get_char ch2 in
          if c1 == c2 then
            if c1 == -1 then true
            else cmp_char() 
          else false in
        cmp_char()
      with _ -> false
    (*e: utility functions *)

    let init g =
      C.register_module "Util"
      [ "dosearchfile",    V.efunc (V.string **-> V.option V.table **-> V.resultvs)
                           (dosearchfile g)
      ; "catch",           V.caml_func catch_lua
      ; "redirect_stdout", V.efunc (V.option V.string **->> V.unit) redirect_stdout
      ; "redirect_stderr", V.efunc (V.option V.string **->> V.unit) redirect_stderr
      ; "file_exists",   V.efunc (V.string **->> V.bool)          file_exists
      ; "files_nonwhite_equivalent",   V.efunc (V.string **-> V.string **->> V.bool) files_nonwhite_equivalent
      ; "call",            V.efunc (V.value **-> V.list V.value **-> V.resultvs)
                           (function V.Function (_, f) -> f
                                   | v -> V.projection v "function")
      ] g;
end
(*e: luautil.ml *)
