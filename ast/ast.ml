(*
  Machine generated. Edit at your own risk 
  Reproduce with the following
 --line_width=74
 --no_action=false
 --output_directory=.
 --pickler=sexp
 --view=OCaml
 *)
(* module Ast*)
  type name = (string)
  and conv = (string)
  and hint = (string)
  and reg = (string)
  and target = (string)
  and alias_set = (string)
  and size = (int)
  and align = (int)
  and aligned = (int)
  and in_alias = (string)
  and op = (string)
  and region = (int * int)
  (* coupling: Srcmap.rgn printer *)
  [@printer fun fmt _ -> Format.pp_print_string fmt "<>"]
  and ty =
        T of (ty * region)
      | BitsTy of (size)
      | TypeSynonym of (name)
  
  and name_or_mem =
        NM of (name_or_mem * region)
      | Name of (hint option * name * aligned option)
      | Mem of (ty * expr * aligned option * in_alias list)
  
  and actual = (hint option * expr * aligned option)
  and expr =
        E of (expr * region)
      | Sint of (string * ty option)
      | Uint of (string * ty option)
      | Float of (string * ty option)
      | Char of (int * ty option)
      | Fetch of (name_or_mem)
      | BinOp of (expr * op * expr)
      | UnOp of (op * expr)
      | PrimOp of (name * actual list)
  
  and import = (string option * name)
  and export = (name * string option)
  and variance =
        Invariant
      | Invisible
      | Variant
  
  and register = (variance * hint option * ty * name * reg option)
  and arch =
        Memsize of (int)
      | ByteorderBig
      | ByteorderLittle
      | FloatRepr of (string)
      | Charset of (string)
      | WordSize of (int)
      | PointerSize of (int)
  
  and decl =
        D of (decl * region)
      | Import of (ty option * import list)
      | Export of (ty option * export list)
      | Const of (ty option * name * expr)
      | Typedef of (ty * name list)
      | Registers of (register list)
      | Pragma
      | Target of (arch list)
  
  and bare_formal = (hint option * variance * ty * name * aligned option)
  and formal = (region * bare_formal)
  and memsize =
        NoSize
      | DynSize
      | FixSize of (expr)
  
  and init =
        I of (init * region)
      | InitExprs of (expr list)
      | InitStr of (string)
      | InitUStr of (string)
  
  and datum =
        Da of (datum * region)
      | Label of (name)
      | Align of (align)
      | MemDecl of (ty * memsize * init option)
  
  and cformal = (region * hint option * name * aligned option)
  and flow = Fl of (flow * region)
      | CutsTo of (name list)
      | UnwindsTo of (name list)
      | ReturnsTo of (name list)
      | NeverReturns
      | Aborts
  
  and mem = Al of (mem * region)
      | Reads of (name list)
      | Writes of (name list)
  
  and procann =
        Flow of (flow)
      | Alias of (mem)
  
  and altcont = (expr * expr)
  and range =
        Point of (expr)
      | Range of (expr * expr)
  
  and guarded = (expr option * expr)
  and arm = ArmAt of (arm * region)
      | Case of (range list * body list)
  
  and stmt = S of (stmt * region)
      | IfStmt of (expr * body list * body list)
      | SwitchStmt of (range option * expr * arm list)
      | LabelStmt of (name)
      | ContStmt of (name * cformal list)
      | SpanStmt of (expr * expr * body list)
      | AssignStmt of (name_or_mem list * guarded list)
      | CallStmt of (name_or_mem list *
          conv option *
          expr *
          actual list *
          target list *
          procann list)
      | PrimStmt of (name_or_mem list *
          conv option *
          name *
          actual list *
          flow list)
      | GotoStmt of (expr * target list)
      | JumpStmt of (conv option * expr * actual list * target list)
      | CutStmt of (expr * actual list * flow list)
      | ReturnStmt of (conv option * altcont option * actual list)
      | EmptyStmt
      | CommentStmt of (string)
      | LimitcheckStmt of (expr * expr option)
  
  and body = B of (body * region)
      | DeclBody of (decl)
      | StmtBody of (stmt)
      | DataBody of (datum list)
  
  and proc = (conv option * name * formal list * body list * region)

  and section = Sec of (section * region)
      | Decl of (decl)
      | Procedure of (proc)
      | Datum of (datum)
      | SSpan of (expr * expr * section list)
  
  and toplevel = Top of (toplevel * region)
      | Section of (name * section list)
      | TopDecl of (decl)
      | TopProcedure of (proc)
  
  and program = (toplevel list)
  
[@@deriving show { with_path = false }]
