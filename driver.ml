(*s: driver.ml *)
module E  = Error
module M  = Metrics
module PA = Preast2ir
module T  = Target
(*x: driver.ml *)
let version () =
    ( This.name     stdout 
    ; output_string stdout " "
    ; This.version  stdout 
    ; output_string stdout 
        " (see also http://www.cminusminus.org)\n"
    )
(*x: driver.ml *)
let scan file =
    let fd          = try open_in file
                      with Sys_error(msg) -> E.error msg    in
    let finally ()  = close_in fd                           in
    let lexbuf      = Lexing.from_channel fd                in
    let map         = Srcmap.mk ()                          in
    let scanner     = Scan.scan map                         in
    let location lb = Srcmap.location map (Lexing.lexeme_start lb) in
    let rec loop lb =
        match scanner lb with
            | Parse.EOF -> ()
            | tok       ->
                let (file,line,col) = location lb           in
                let tok             = Scan.tok2str tok      in
                    ( Printf.printf "%-16s %3d %2d %s\n" file line col tok
                    ; loop lb
                    )
    in
        ( Srcmap.sync map 0 (file,1,1)
        ; loop lexbuf
        ; finally ()
        )
(*x: driver.ml *)
let parse (file:string) = 
    let fd          = try open_in file
                      with Sys_error(msg) -> E.error msg    in
    let finally ()  = close_in fd                           in
    let lexbuf      = Lexing.from_channel fd                in
    let map         = Srcmap.mk ()                          in
    let scanner     = Scan.scan map                         in
        try
            ( Srcmap.sync map 0 (file,1,1)
            ; (map, Parse.program scanner lexbuf) 
            )
        with
            | Parsing.Parse_error ->
              ( finally()
              ; E.errorPointPrt (map, Lexing.lexeme_start lexbuf) "parse error"
              ; E.error "parse error - compilation aborted"
              )
            | E.ErrorExn msg ->
              ( finally()
              ; E.errorPointPrt (map, Lexing.lexeme_start lexbuf) msg
              ; E.error "parse error - compilation aborted"
              )
            | e ->
              ( finally()
              ; raise e
              )
(*x: driver.ml *)
let emit_asdl (map,ast) =
        AstUtil.sexp_wr_program ast stdout
(*x: driver.ml *)
let pretty (map,ast) = Astpp.program ast
let print doc width channel = Pp.ppToFile channel width doc
(*x: driver.ml *)
let metrics_ok src tgt =
  let int i = string_of_int i in
  let string s = s in
  let outcome = ref true in
  let unequal property to_string source target = 
    outcome := false;
    Printf.eprintf "source code specifies %s %s, but target %s specifies %s %s\n"
      property (to_string source) tgt.T.name property (to_string target) in
  let (<>) = Pervasives.(<>) in
  if src.M.byteorder <> tgt.T.byteorder then
    unequal "byteorder" Rtlutil.ToUnreadableString.agg src.M.byteorder tgt.T.byteorder;
  if src.M.wordsize <> tgt.T.wordsize then
    unequal "wordsize" int src.M.wordsize tgt.T.wordsize;
  if src.M.pointersize <> tgt.T.pointersize then
    unequal "pointersize" int src.M.pointersize tgt.T.pointersize;
  if src.M.memsize <> tgt.T.memsize then
    unequal "memsize" int src.M.memsize tgt.T.memsize;
  if src.M.float <> Float.name tgt.T.float then
    unequal "float" string src.M.float (Float.name tgt.T.float);
  if src.M.charset <> tgt.T.charset then
    unequal "charset" string src.M.charset tgt.T.charset;
  !outcome
(*x: driver.ml *)
let elab ~swap validate (map,ast) asm =
  Nelab.program ~swap validate map asm (Nast.program ast)

let compile (PA.T target) opt ~exportglobals ~src ~asm ~validate ~swap =
  (* old: failwith "TODO: pad: Driver.compile and mvaludate" *)

  let validate = if validate then Mvalidate.rtl target else (fun _ -> None) in
  let abort () = Error.error "compilation aborted because of errors" in
  let as_ok = function Error.Ok x -> x | Error.Error -> abort () in
  as_ok (Error.ematch (elab swap validate src asm) (fun (env,compunit) ->
    if metrics_ok (Fenv.Dirty.metrics env) target then
      if Fenv.Dirty.errorFlag env then
        abort ()
      else
        Ast2ir.translate (PA.T target) (Fenv.clean env) opt exportglobals compunit
    else
      Error.error "metrics of source code don't match the target"))
(*e: driver.ml *)
