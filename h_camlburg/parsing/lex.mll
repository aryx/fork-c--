(*s: lex.mll *)
{   
    let rcsid = "$Id: lex.nw,v 1.9 2006-03-08 22:33:10 nr Exp $"

    exception Error of string
    let error msg = raise (Error msg)
    
    (*s: prolog *)
    module T = Parse
    (*x: prolog *)
    let nl lexbuf map =
        let next = (Lexing.lexeme_start lexbuf) + 1 in
            Srcmap.nl map next
    (*x: prolog *)
    let return x = fun map -> x
    (*x: prolog *)
    let get         = Lexing.lexeme
    let getchar     = Lexing.lexeme_char
    (*x: prolog *)
    let keywords    = Hashtbl.create 27
    let keyword s   = try Hashtbl.find keywords s with Not_found -> T.ID(s)
    let _ = Array.iter (fun (str,tok) -> Hashtbl.add keywords str tok)
        [| "start"  , T.START
        ;  "term"   , T.TERM
        ;  "type"   , T.TYPE 
        ;  "head"   , T.HEAD
        ;  "tail"   , T.TAIL
        |]
    (*e: prolog *)
}
(*s: regular expressions *)
let alpha       = ['a'-'z' 'A'-'Z']
let digit       = ['0'-'9']
let nl          = '\n'          
let id          = alpha(digit|alpha|'_')*
(*e: regular expressions *)
(*s: token *)
rule token = parse
    eof                     { fun map -> T.EOF(map) }
  | [' ' '\t' '\r']+        { fun map -> token lexbuf map } 
  | "--" [^ '\n']*          { fun map -> token lexbuf map } (* comment *)
  (* pad: to make easier for syncweb generated files *)
  | "/*" [^ '\n']*          { fun map -> token lexbuf map } (* comment *)
  | "(*" [^ '\n']*          { fun map -> token lexbuf map } (* comment *)
  | nl                      { fun map -> nl lexbuf map; token lexbuf map }
  | nl [' ' '\t' '\r']* '#' { fun map -> line lexbuf map 0; token lexbuf map }
  | '#'                     { fun map ->
                              if Lexing.lexeme_start lexbuf = 0 then
                                (line lexbuf map 0; token lexbuf map)
                              else
                                error "illegal character `#'"
                            }    
  | digit+                  { return (T.INT(int_of_string(get lexbuf)))}
  | id                      { return (keyword (get lexbuf)) }
  | "{:"                    { fun map -> 
                              let p = Lexing.lexeme_start lexbuf + 2 in 
                              let s = action lexbuf 0 map (Buffer.create 80) in
                                T.CODE(Srcmap.location map p,s)
                            } 
  
  | '"'                     { fun map ->
                              let s = string lexbuf map (Buffer.create 80) in 
                                T.STRING(s)
                            }    
  | '\''                    { fun map -> T.CHAR(char lexbuf map) }
  | '%'                     { return T.PERCENT }
  | "%%"                    { return T.PPERCENT }
  | '<'                     { return T.LT }
  | '>'                     { return T.GT }
  | ':'                     { return T.COLON }
  | '('                     { return T.LPAREN }
  | ')'                     { return T.RPAREN }
  | '['                     { return T.LBRACKET }
  | ']'                     { return T.RBRACKET }
  
  | ','                     { return T.COMMA }

  | _                       { fun map ->
                              error ( "illegal character `"
                                    ^ get lexbuf
                                    ^ "' in specification"
                                    )
                            }        
(*e: token *)
(*s: action *)
and action = parse 
    eof                     { fun n map buf ->
                              error "unexpected EOF in semantic action"
                            }
  | ":}"                    { fun n map buf ->
                              if n = 0 then (Buffer.contents buf)
                              else ( Buffer.add_string buf ":}"
                                   ; action lexbuf (n-1) map buf 
                                   )
                            }
  
  | "{:"                    { fun n map buf ->
                              ( Buffer.add_string buf (get lexbuf)
                              ; action lexbuf (n+1) map buf
                              )
                            }  
  | [^ ':' '{' '\n']+
  | ':'
  | '{'                     { fun n map buf ->
                              let s = get lexbuf in
                              ( Buffer.add_string buf s
                              ; action lexbuf n map buf
                              )
                            }
                            
  | nl                      { fun n map buf -> 
                              ( Buffer.add_char buf '\n'
                              ; nl lexbuf map
                              ; action lexbuf n map buf
                              )
                            }
  | _                       { fun n map buf ->
                              error ("illegal character `"
                                    ^ get lexbuf
                                    ^ "'in action string"
                                    )
                            }  
(*e: action *)
(*s: line *)
and line = parse 
    eof                 { fun map l ->
                          error "unterminated line directive" 
                        }
  | [' ' '\t']+         { line lexbuf }
  | '"' [^ '"']+ '"'    { fun map l ->
                          let string = get lexbuf in
                          let len    = String.length string in
                          let file   = String.sub string 1 (len-2) in
                          let pos    = Lexing.lexeme_start lexbuf in
                          let loc    = file, l-1, 1 in
                                ( Srcmap.sync map pos loc
                                ; () (* return *)
                                )
                        }
  | digit+              { fun map l -> 
                          (* inline'ing the l' expression caused an
                          int_of_string failure with ocamlopt *)
                          let l' = int_of_string (Lexing.lexeme lexbuf)
                          in  line lexbuf map l'
                        }
  | id                  { line lexbuf }
  | _                   { fun map l -> 
                          error "illegal character in line directive"
                        }
(*e: line *)
(*s: string *)
and string = parse 
    eof         { fun map buf -> 
                    error ("end of file in string: " ^ Buffer.contents buf) }   
  | '\n'        { fun map buf -> 
                    error ("end of line in string: " ^ Buffer.contents buf) }
  | '\\' _      { fun map buf -> 
                  let c = getchar lexbuf 1 in
                  let k = match c with
                      (* | 'n'  -> '\n' *)
                      (* | 't'  -> '\t' *)
                      (* | 'r'  -> '\r' *)
                      | '\n' -> '\n'
                      | _    -> c
                  in   
                     ( Buffer.add_char buf k
                     ; string lexbuf map buf
                     )
                }
  | [^'"' '\'' '\n' '\\']+  
                { fun map buf -> 
                   let s = get lexbuf  in
                     ( Buffer.add_string buf s
                     ; string lexbuf map buf
                     )
                }
  | '"'         { fun map buf -> Buffer.contents buf }
                 
  | _           { fun map buf -> 
                  error ( "illegal character in string: " 
                        ^ Buffer.contents buf
                        ) 
                }
(*e: string *)
(*s: char *)
and char = parse
    eof         { fun map -> error "end of file in character constant" }
  | '\\' _ "'"  { fun map ->
                  let c = getchar lexbuf 1 in
                    match c with
                    | 'n'   -> '\n'
                    | 't'   -> '\t'
                    | 'r'   -> '\r'
                    | _     -> c
                }
  | [^'\\' '\''] "'"
                { fun map -> getchar lexbuf 0 }
                
  | _           { fun map -> 
                  error (Printf.sprintf 
                            "illegal character constant: %c"
                            (getchar lexbuf 0))
                }            
(*e: char *)
{
    (*s: epilog *)
    let to_string = function
        | T.ID(s)               -> Printf.sprintf "id(%s)" s
        | T.CODE((f,l,c),s)     -> Printf.sprintf "action(%s)" s 
        | T.INT(d)              -> Printf.sprintf "int(%d)" d
        | T.STRING(s)           -> Printf.sprintf "\"%s\"" (String.escaped s)
        | T.CHAR(c)             -> Printf.sprintf "'%s\'"  (Char.escaped c)

        | T.COLON     -> ":"
        | T.COMMA     -> ","  
        | T.EOF _     -> "<eof>"
        | T.EOL       -> "<eol>"
        | T.GT        -> ">"
        | T.LBRACKET  -> "["
        | T.LPAREN    -> "("
        | T.LT        -> "<"
        | T.PERCENT   -> "%"
        | T.PPERCENT  -> "%%"
        | T.RBRACKET  -> "]"
        | T.RPAREN    -> ")"
        | T.SEMI      -> ";"  
        | T.START     -> "start"  
        | T.TERM      -> "term"
        | T.TYPE      -> "type"  
        | T.HEAD      -> "head" 
        | T.TAIL      -> "tail" 
    (*x: epilog *)
    let scan file =
        let fd          = try open_in file
                          with Sys_error(msg) -> error msg      in
        let finally ()  = close_in fd                           in
        let lexbuf      = Lexing.from_channel fd                in
        let map         = Srcmap.mk ()                          in
        let scanner lb  = token lb map                          in
        let location lb = Srcmap.location map (Lexing.lexeme_start lb) in
        let rec loop lb =
            match scanner lb with
                | T.EOF _   -> ()
                | tok       ->
                    let (file,line,col) = location lb           in
                    let tok             = to_string tok         in
                        ( Printf.printf "%-16s %3d %2d %s\n" file line col tok
                        ; flush stdout
                        ; loop lb
                        )
        in
            ( Srcmap.sync map 0 (file,1,1)
            ; loop lexbuf
            ; finally ()
            )
    (*x: epilog *)
    let main () =
        let argv        = Array.to_list Sys.argv in
            match List.tl argv with
            | file::_   -> scan file; exit 0
            | []        -> error "file name expected on command line"

    (* let _ = main () *)
    (*e: epilog *)
}    
(*e: lex.mll *)
