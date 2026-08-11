(*s: main.ml *)
module S = Spec
let rcsid   = "$Id: main.nw,v 1.28 2006-03-08 22:33:10 nr Exp $"
let version = [ "ocamlburg 1.5 (" ^ rcsid ^ ")" 
              ; "(c) 2002, 2003 Christian Lindig <lindig@eecs.harvard.edu>"
              ; "               Norman Ramsey <nr@eecs.harvard.edu>"
              ; "               Kevin Redwine <redwine@eecs.harvard.edu>"
              ; 
              ]

exception Error of string
let error msg = raise (Error msg)

(*x: main.ml *)
let read (file:string) =
    let fd        = try open_in file
                    with Sys_error(msg) -> error msg in
    let finally   = fun () -> close_in fd in              
    let lexbuf    = Lexing.from_channel fd in
    let map       = Srcmap.mk () in
    let lexer lb  = Lex.token lb map in
    let here lb   = Srcmap.Str.point (map, Lexing.lexeme_start lb) in
        try 
            let () = Srcmap.sync map 0 (file,1,1)
            and t  = Parse.spec lexer lexbuf 
                in finally ();t     (* the result *)
        with
            | Parseerror.Error(msg) ->
                ( finally ()
                ; error (Printf.sprintf "semantic error at %s: %s" 
                         (here lexbuf) msg)
                ) 
            | Parsing.Parse_error ->
                ( finally ()
                ; error (Printf.sprintf "parse error at %s" (here lexbuf))
                )          
            | Lex.Error msg -> 
                ( finally ()
                ; error 
                    (Printf.sprintf "scan error at %s: %s" (here lexbuf) msg)
                )
            | e ->
                ( finally ()    (* just clean up *)
                ; raise e
                )
(*x: main.ml *)
let help = 
    [ "usage: ocamlburg file.mlb"
    ; "       ocamlburg -version       show version, authors"
    ; "       ocamlburg -norm          for debugging, show normalized rules"
    ; "       ocamlburg -twelf         print twelf code for coverage check"
    ; "       ocamlburg -help          this"
    ]
    
let main () =
    let argv        = Array.to_list Sys.argv in
        match List.tl argv with
        | ["-norm"; file] ->
            let spec  = read file               in
            let rules = Norm.rules spec.S.rules in
            let doc   = Spec.PrettyPrint.rules rules in
                Code.Print.doc doc
        | ["-twelf"; file] ->
            let spec  = read file in
            Noguardscheck.gen_elf spec
        | ["-version"] -> 
            List.iter print_endline version
        | ["-help"] | ["--help"] ->
            List.iter print_endline help            
        | [file] -> 
            let spec = read file in
                (try Burg.generate spec stdout 
                 with Burg.Error msg -> error ("semantic error: "^msg))
        | _ -> 
            error (Printf.sprintf "try `ocamlburg -help'") 
(*x: main.ml *)
let _ = 
    try 
        main (); exit 0
    with 
        Error msg -> 
            ( Printf.eprintf "%s\n" msg
            ; exit 1
            )
(*e: main.ml *)
