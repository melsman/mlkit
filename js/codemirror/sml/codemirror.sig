signature CODE_MIRROR = sig

  structure EditorProperties : sig
    type t
    val empty : unit -> t
    val stylesheets : t -> string list -> unit
    val parserfiles : t -> string list -> unit
    val path        : t -> string -> unit
    val height      : t -> string -> unit
    val width       : t -> string -> unit
    val minHeight   : t -> string -> unit
    val readOnly    : t -> bool -> unit
    val lineNumbers : t -> bool -> unit
    val textWrapping: t -> bool -> unit
  end

  type editor
  val newEditor : {id:string, properties: EditorProperties.t} -> editor

  val getCode           : editor -> string
  val setCode           : editor -> string -> unit
  val focus             : editor -> unit
  val selection         : editor -> string
  val replaceSelection  : editor -> string -> unit
  val reindent          : editor -> unit
  val reindentSelection : editor -> unit
  val undo              : editor -> unit
  val redo              : editor -> unit
(*
  (* Lines *)
  type line
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
