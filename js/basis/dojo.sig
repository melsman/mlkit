(** Dojo/Dijit layout widgets.

The interface supports a monadic style of programming. The
implementation of the monad is the continuation monad, capturing the
continuation-based approach to loading libraries in the Dojo
framework. For general information about the Dojo framework, see
http://dojotoolkit.org/reference-guide/1.10/. In particuler see
http://dojotoolkit.org/reference-guide/1.10/.
*)

signature DOJO = sig
  type 'a M
  val >>= : 'a M * ('a -> 'b M) -> 'b M
  val ret : 'a -> 'a M

  type icon = string * string
  type hash = (string * string) list
  type widget

  val pane               : hash -> Js.elem -> widget M
  val titlePane          : hash -> widget -> widget M
  val linkPane           : hash -> widget M
  val accordionContainer : hash -> widget list -> widget M
  val tabContainer       : hash -> widget list -> widget M
  val borderContainer    : hash -> widget list -> widget M
  val layoutContainer    : hash -> widget list -> widget M
  val attachToElement    : Js.elem -> widget M -> (unit -> unit) -> unit
  val run                : unit M -> unit
  val setProperties      : hash -> widget -> unit
  val setBoolProperty    : string*bool -> widget -> unit
  val setContent         : widget -> string -> unit
  val setContentElement  : widget -> Js.elem -> unit
  val selectChild        : widget -> widget -> unit
  val addChild           : widget -> widget -> unit
  val startup            : widget -> unit (* shouldn't be necessary... Hmm *)
  val domNode            : widget -> Js.elem

  val dialog             : hash -> Js.elem -> widget M
  val showDialog         : widget -> unit
  val hideDialog         : widget -> unit 
  val runDialog          : string -> Js.elem -> unit

  type treeStore
  val treeStore          : hash list -> treeStore M
  val treeStoreAdd       : treeStore -> hash -> unit
  val treeStoreRemove    : treeStore -> string -> unit
  val tree               : hash -> string -> (string*string -> unit) 
                           -> treeStore -> widget M

  type tabmap = widget * (string*widget)list ref
  val advTabContainer    : hash -> (tabmap*{select:string->unit,close:string->unit}) M

  val set_onClose        : widget -> (unit -> bool) -> unit
  val set_onShow         : widget -> (unit -> unit) -> unit

  structure Menu : sig
    type menu
    val mk     : hash -> (widget * menu) M
    val menu   : menu -> string -> menu M
    val item   : menu -> string * icon option * (unit -> unit) -> unit M
  end

  type 'a editCon
  val optionBox           : 'a editCon -> 'a option editCon   (* raises Fail if applied to a 't option editCon', for some t *)
  val orEmptyBox          : string editCon -> string editCon
  val textBox             : hash -> string editCon
  val numBox              : hash -> string editCon
  val realBox             : hash -> real editCon
  val intBox              : hash -> int editCon
  val dateBox             : hash -> string editCon
  val validationBox       : hash -> {fromString: string -> 'a option, toString: 'a -> string} -> 'a editCon
  val filterSelectBox     : hash -> {id:string,name:string}list -> string editCon

  structure Editor : sig
    type 'a t
    val mk                : 'a editCon -> 'a t M
    val getValue          : 'a t -> 'a
    val setValue          : 'a t -> 'a -> unit
    val domNode           : 'a t -> Js.elem
    val toForeignPtr      : 'a t -> foreignptr
    val startup           : 'a t -> unit   (* in particular for filterSelectBox *)
  end

  structure Form : sig
    type t
    val mk           : hash -> t M 
    val domNode      : t -> Js.elem
    val toForeignPtr : t -> foreignptr
    val validate     : t -> bool
    val startup      : t -> unit
  end                                  

  structure Button : sig
    type t
    val mk           : hash -> (unit->unit) -> t M
    val domNode      : t -> Js.elem
    val toForeignPtr : t -> foreignptr
  end

  structure RestGrid : sig
    type t
    type colspec
    type button = {label:string,icon:icon option}
    datatype typ = INT | STRING | NUM of int
    val valueColspec  : {field:string,label:string,editor:'a editCon option,typ:typ} -> colspec
    val valuePrettyColspec : {field:string,label:string,pretty:string->Js.elem,editor:'a editCon option,typ:typ} -> colspec
    val valuePrettyWithIdColspec : {field:string,label:string,prettyWithId:string*string->Js.elem,editor:'a editCon option,typ:typ} -> colspec
    val deleteColspec : {label:string,button:button} -> colspec
    val actionColspec : {label:string,button:button,onclick:(string->string)->unit} -> colspec  (* arg to onclick is a function for looking up a field value, given a key *)
    val hidden        : bool -> colspec -> colspec
    val unhidable     : bool -> colspec -> colspec
    val sortable      : bool -> colspec -> colspec   (* e.g.: val cs = hidden true (sortable true cs)  *)

    val mk            : {target:string, headers:(string*string)list, idProperty:string, addRow:(button*button) option, notify:string->unit, notify_err:string->unit} -> colspec list -> t M
    val domNode       : t -> Js.elem
    val toStore       : t -> foreignptr
    val setCollection : t -> {target:string} -> unit 
    val setSort       : t -> {field:string} -> unit 
    val startup       : t -> unit
    val refresh       : t -> unit
  end

  structure Grid : sig
    type t
    val mk           : hash -> (string*string) list -> t M
    val add          : t -> (string*string) list list -> unit
    val domNode      : t -> Js.elem
    val toForeignPtr : t -> foreignptr
  end

  (* TODO: Toolbar, Fieldset *)

  structure Icon : sig
    val save : icon
    val print : icon
    val cut : icon
    val copy : icon
    val clear : icon
    val delete : icon
    val undo : icon
    val edit : icon
    val newTask : icon
    val editTask : icon
    val editProperty : icon
    val task : icon
    val filter : icon
    val configure : icon
    val search : icon
    val application : icon
    val bookmark : icon
    val chart : icon
    val connector : icon
    val database : icon
    val documents : icon
    val mail : icon
    val leaf : icon
    val file : icon
    val function : icon
    val key : icon
    val package : icon
    val sample : icon
    val table : icon
    val users : icon
    val folderClosed : icon
    val folderOpen : icon
    val error : icon
  end

  structure EditorIcon : sig
    val sep : icon
    val save : icon
    val print : icon
    val cut : icon
    val copy : icon
    val paste : icon
    val delete : icon
    val cancel : icon
    val undo : icon
    val redo : icon
    val selectAll : icon
    val bold : icon
    val italic : icon
    val underline : icon
    val strikethrough : icon
    val superscript : icon
    val subscript : icon
    val justifyCenter : icon
    val justifyFull : icon
    val justifyLeft : icon
    val justifyRight : icon
    val indent : icon
    val outdent : icon
    val listBulletIndent : icon
    val listBulletOutdent : icon
    val listNumIndent : icon
    val listNumOutdent : icon
    val tabIndent : icon
    val leftToRight : icon
    val rightToLeft : icon
    val toggleDir : icon
    val backColor : icon
    val foreColor : icon
    val hiliteColor : icon
    val newPage : icon
    val insertImage : icon
    val insertTable : icon
    val space : icon
    val insertHorizontalRule : icon
    val insertOrderedList : icon
    val insertUnorderedList : icon
    val createLink : icon
    val unlink : icon
    val viewSource : icon
    val removeFormat : icon
    val fullScreen : icon
    val wikiword : icon
  end
end

(**

[type 'a M] is the type of the underlying continuation monad that
encapsulates the loading of necessary libraries and execution of
initialization, etc.

[ret a] lifts a value a into a monad.

[m >>= f] makes it possible to sequentialize computations.

[type hash] 

[pane h] returns a computation that computes a basic leaf widget. Set
the content property in the hash to initialize the content or set it
dynamically using the setProperties function.

[accordionContainer h ws] returns a computation that computes an
accordion widget made from the ws widgets. Use the title property of
each of the ws widgets to set the title of each of the underlying
widgets.

[tabContainer h ws] returns a computation that computes a tab widget
made from the ws widgets. Use the title property of each of the ws
widgets to set the title of each of the underlying widgets.

[borderContainer h ws] returns a computation that computes a border
container widget made from the ws widgets. The border container
widgets has up to five regions, namely the regions left, right, top,
bottom, and center. Use the "region" property on the ws widgets to set
the regions. Use the "splitter" boolean property on all of the ws
widgets (except the center one) to specify that the widget can be
resized by the user (the center region will automatically be
adjusted). You also want to set the height and the width of the border
container widget using the "style" property.

[attachToElement e m k] initializes the widget (by running the monad
computation m) and attaches it to the element e. After the attachment,
the continuation k is run.

[setProperties h w] sets the properties in the hash h on the widget
w. Notice that not all properties may be set dynamically; consult the
Dojo/Dijit documentation for details.

[type treeStore] is the type of a tree store in memory.

[treeStore hs] returns a monad for constructing a treeStore based on a
list of hash values (nodes) containing id, name, and parent keys (the
root is the node that does not have a parent key.

[tree h id onClick store] returns a monad for constructing a tree
widget with the underlying store. The hash h may contain a title,
default width and the other properties that controls the context in
which the tree appears. The id is the id of the root of the tree. The
onClick method is triggered whenever a node in the tree is clicked on
by the end user - the argument to the onClick function is a pair of
the id and the name of the node clicked on.

[runDialog title e] opens a Dojo dialog window with the caption title
containing the DOM element e. Set the width and height style
attributes on the element e to control the size of the dialog window.

*)
