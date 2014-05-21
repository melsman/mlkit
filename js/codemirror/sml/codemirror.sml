structure CodeMirror :> CODE_MIRROR = struct

  open JsCore

  structure EditorProperties = struct
    type t = foreignptr
    fun empty () : t = exec0{stmt="return {};",res=fptr} ()
    fun setPropertyStringArray t p xs : unit =
        let fun pp xs = 
                String.concatWith "," (map (fn s => "\"" ^ s ^ "\"") xs)
        in 
          exec2 {stmt="t[p] = [" ^ pp xs ^ "];",
                 arg1=("t",fptr),
                 arg2=("p",string),
                 res=unit} (t,p)
        end
    fun stylesheets (t:t) (sheets:string list) : unit =
        setPropertyStringArray t "stylesheet" sheets
    fun parserfiles (t:t) (files: string list) : unit =
        setPropertyStringArray t "parserfile" files
    fun path (t:t) (p: string) : unit =
        setProperty t string "path" p
    fun height (t:t) (p: string) : unit =
        setProperty t string "height" p
    fun width (t:t) (p: string) : unit =
        setProperty t string "width" p
    fun minHeight (t:t) (p: string) : unit =
        setProperty t string "minHeight" p
    fun lineNumbers (t:t) (b: bool) : unit =
        setProperty t bool "lineNumbers" b
    fun textWrapping (t:t) (b: bool) : unit =
        setProperty t bool "textWrapping" b
    fun readOnly (t:t) (b: bool) : unit =
        setProperty t bool "readOnly" b
  end

  type editor = foreignptr

  fun newEditor {id:string, properties:EditorProperties.t} : editor =
      exec2 {stmt="return CodeMirror.fromTextArea(id,props);",
             arg1=("id",string),
             arg2=("props",fptr),
             res=fptr} (id,properties)

  fun getCode (e : editor) : string =
      exec1 {stmt="return e.getCode();",
             arg1=("e",fptr),
             res=string} e

  fun setCode (e : editor) (s:string) : unit =
      exec2 {stmt="return e.setCode(s);",
             arg1=("e",fptr),
             arg2=("s",string),
             res=unit} (e,s)

  fun focus (e : editor) : unit =
      exec1 {stmt="return e.focus();",
             arg1=("e",fptr),
             res=unit} e

  fun selection (e : editor) : string =
      exec1 {stmt="return e.selection();",
             arg1=("e",fptr),
             res=string} e

  fun replaceSelection (e : editor) (s:string) : unit =
      exec2 {stmt="return e.replaceSelection(s);",
             arg1=("e",fptr),
             arg2=("s",string),
             res=unit} (e,s)

  fun reindent (e : editor) : unit =
      exec1 {stmt="return e.reindent();",
             arg1=("e",fptr),
             res=unit} e

  fun reindentSelection (e : editor) : unit =
      exec1 {stmt="return e.reindentSelection();",
             arg1=("e",fptr),
             res=unit} e

  fun undo (e : editor) : unit =
      exec1 {stmt="return e.undo();",
             arg1=("e",fptr),
             res=unit} e

  fun redo (e : editor) : unit =
      exec1 {stmt="return e.redo();",
             arg1=("e",fptr),
             res=unit} e
(*
  (* Lines *)
  type line = foreignptr
  val nthLine           : editor -> int -> line option
  val lineNumber        : editor -> line -> int
  val nextLine          : editor -> line -> line option
  val prevLine          : editor -> line -> line option
  val selectLines       : editor -> line * int -> line * int -> unit
  val moveCursor        : editor -> line * int -> unit

  (* Searching *)
  type casesensitive = bool
  type cursorpos
  datatype position = Start | Current | Cursor of cursorpos
  type search
  val findNext          : search -> bool
  val findPrevious      : search -> bool
  val getSearchCursor   : editor -> string -> position -> casesensitive -> search
*)
end
