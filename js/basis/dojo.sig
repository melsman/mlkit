(** Dojo/Dijit layout widgets.

The interface supports a monadic style of programming. The
implementation of the monad is the continuation monad, capturing the
continuation-based approach to loading libraries in the Dojo
framework. For general information about the Dojo framework, see
http://dojotoolkit.org/reference-guide/1.9/. In particuler see
http://dojotoolkit.org/reference-guide/1.9/.
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
  val attachToElement    : Js.elem -> widget M -> unit
  val run                : unit M -> unit
  val setProperties      : hash -> widget -> unit
  val setContent         : widget -> string -> unit
  val setContentElement  : widget -> Js.elem -> unit
  val selectChild        : widget -> widget -> unit
  val addChild           : widget -> widget -> unit

  val dialog             : hash -> Js.elem -> widget M
  val showDialog         : widget -> unit
  val hideDialog         : widget -> unit 

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

  structure Menu: sig
    type menu
    val mk     : hash -> (widget * menu) M
    val menu   : menu -> string -> menu M
    val item   : menu -> string * icon option * (unit -> unit) -> unit M
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

[attachToElement e m] initializes the widget (by running the monad
computation m) and attaches it to the element e.

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

*)
