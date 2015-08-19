
structure Dojo :> DOJO = struct
  type icon = string * string
  type hash = (string * string) list   
  type widget = foreignptr

  fun log s = JsCore.call1 ("console.log",JsCore.string,JsCore.unit) s

  val fptr2unit_T = JsCore.==>(JsCore.fptr,JsCore.unit)
  val unit2unit_T = JsCore.==>(JsCore.unit,JsCore.unit)

  fun require0 (f:unit->unit) : unit =
      JsCore.exec1 {stmt="require(['dojo/domReady!'],f);",
                    arg1=("f",unit2unit_T),
                    res=JsCore.unit} f
  fun require1 (s:string) (f:foreignptr->unit) : unit =
      JsCore.exec2 {stmt="require([s,'dojo/domReady!'],f);",
                    arg1=("s",JsCore.string),
                    arg2=("f",fptr2unit_T),
                    res=JsCore.unit} (s,f)

  infix >>=
  type 'a M = ('a -> unit) -> unit
  fun ret a f = f a
  fun (m : 'b M) >>= (k : 'b -> 'a M) : 'a M =
    fn c:'a -> unit => m(fn v:'b => k v c)

  fun domNode (c:widget) : Js.elem =
      Js.Element.fromForeignPtr(JsCore.getProperty c JsCore.fptr "domNode")

  fun startup (c:widget) : unit =
      JsCore.method0 JsCore.unit c "startup"

  fun run (m : unit M) : unit = m (fn x => x)

  fun attachToElement (e: Js.elem) (m : widget M) : unit =   (* run *)
      let val () = JsCore.exec0 {stmt="this.dojoConfig = {parseOnLoad: true};",
                                 res=JsCore.unit} ()
      in Js.loadScript "dojo/dojo.js"
                      (fn () =>
                       require0(fn () =>
                                   m(fn c =>
                                        let val n = domNode c
                                        in Js.appendChild e n
                                         ; startup c
                                        end
                                    )
                               )
                      )
      end

  fun addChild (e:widget) (p:widget) : unit =
      JsCore.method1 JsCore.fptr JsCore.unit e "addChild" p

  fun mkHash h = JsCore.Object.fromList JsCore.string h

  fun new0 c arg = 
      let val obj = JsCore.exec2{stmt="return new c(h);", arg1=("c",JsCore.fptr), arg2=("h",JsCore.fptr),
                                 res=JsCore.fptr} (c,arg)
      in obj
      end

  fun new c h =
      new0 c (mkHash h)

  fun stackContainer (kind:string) (h:hash) (panes: widget list) : widget M =
      fn (f: widget -> unit) =>
         require1 kind
                  (fn Sc =>
                      let val sc = new Sc(h)
                      in List.app (addChild sc) panes
                       ; f sc
                      end)

  val tabContainer = stackContainer "dijit/layout/TabContainer"
  val borderContainer = stackContainer "dijit/layout/BorderContainer"
  val layoutContainer = stackContainer "dijit/layout/LayoutContainer"
  val accordionContainer = stackContainer "dijit/layout/AccordionContainer"

  fun setContentElement w (e: Js.elem) =
    JsCore.exec2{stmt="w.set('content', e);",
                 arg1=("w",JsCore.fptr), arg2=("e",JsCore.fptr),
                 res=JsCore.unit} (w,Js.Element.toForeignPtr e)

  fun pane (h:hash) e : widget M =
      fn (f: widget -> unit) =>
         require1 "dijit/layout/ContentPane"
                  (fn Cp => 
                      let val p = new Cp(h)
                      in setContentElement p e
                       ; f p
                      end)

  fun dialog (h:hash) e : widget M =
      fn (f: widget -> unit) =>
         require1 "dijit/Dialog"
                  (fn D => 
                      let val d = new D(h)
                      in setContentElement d e
                       ; f d
                      end)

  fun showDialog d = JsCore.method0 JsCore.unit d "show"
  fun hideDialog d = JsCore.method0 JsCore.unit d "hide"

  fun runDialog title e =
      run (dialog[("title", title)] e >>= (ret o showDialog))

  fun titlePane (h:hash) (w: widget) : widget M =
      fn (f: widget -> unit) =>
         require1 "dijit/TitlePane"
                  (fn Tp => 
                      let val p = new Tp(h)
                          val () = addChild p w
                      in f p
                      end)

  fun linkPane (h:hash) : widget M =
      fn (f: widget -> unit) =>
         require1 "dijit/layout/LinkPane"
                  (fn Lp => 
                      let val p = new Lp(h)
                      in f p
                      end)

  fun setProp w t k v =
    JsCore.exec3{stmt="w.set(k,v);",
                 arg1=("w",JsCore.fptr), 
                 arg2=("k",JsCore.string),
                 arg3=("v",t), 
                 res=JsCore.unit} (w,k,v)

  fun setProperties h w =
      List.app (fn (k,v) => setProp w JsCore.string k v) h

  fun setBoolProperty (k,v) w =
      setProp w JsCore.bool k v

  fun setContent w s = setProp w JsCore.string "content" s

  fun selectChild w w2 = JsCore.method2 JsCore.fptr JsCore.bool JsCore.unit w "selectChild" w2 true
  fun removeChild w w2 = JsCore.method1 JsCore.fptr JsCore.unit w "removeChild" w2

  type treeStore = foreignptr
  fun treeStore (hs:hash list) : treeStore M =
      fn (f: treeStore -> unit) =>
         require1 "dojo/store/Memory" (fn Memory => 
         require1 "dojo/store/Observable" (fn Observable =>
                      let val data = JsCore.Array.fromList JsCore.fptr (List.map mkHash hs)
                          val h = JsCore.exec1{stmt="return {data:data,getChildren:function(obj) { return this.query({parent:obj.id}); }};",
                                               arg1=("data",JsCore.fptr),
                                               res=JsCore.fptr} data
                          val p = new0 Memory(h)
                          val p = JsCore.exec2{arg1=("Con",JsCore.fptr),
                                               arg2=("s",JsCore.fptr),
                                               res=JsCore.fptr,
                                               stmt="return new Con(s);"} (Observable,p)
                      in f p
                      end))

  fun treeStoreAdd ts h = JsCore.method1 JsCore.fptr JsCore.unit ts "add" (mkHash h)
  fun treeStoreRemove ts s = JsCore.method1 JsCore.string JsCore.unit ts "remove" s
      
  fun tree (h: hash) rootId onClick (store:treeStore) : widget M =
      fn (f: widget -> unit) =>
         require1 "dijit/tree/ObjectStoreModel" (fn ObjectStoreModel =>
         require1 "dijit/Tree" (fn Tree =>
         let val modelArg = 
                 JsCore.exec2{stmt="return {store:store,query:{id:id},mayHaveChildren:function(item){return item.kind == 'folder';}};",arg1=("store",JsCore.fptr),
                              arg2=("id",JsCore.string),res=JsCore.fptr}(store,rootId)
             val model = new0 ObjectStoreModel(modelArg)
             val treeArg = mkHash h
             val () = JsCore.Object.set JsCore.fptr treeArg "model" model
             val () = JsCore.exec2{stmt="a.onClick = function(item) { f([item.id,item.name]); };",
                                   arg1=("a",JsCore.fptr), arg2=("f",JsCore.===>(JsCore.string,JsCore.string,
                                                                                 JsCore.unit)),
                                   res=JsCore.unit} (treeArg,onClick)
             val tree = new0 Tree(treeArg)
         in f tree
         end))

  type tabmap = widget * (string*widget)list ref
  fun advTabContainer (h:hash) : (tabmap * {select:string->unit,close:string->unit}) M =
      tabContainer h [] >>= (fn tabs =>
      let val tm = ref []
          fun withTab s f =
              case List.find (fn (s',_) => s=s') (!tm) of
                  SOME (_,p) => f p
                | NONE => ()         
          fun select s = withTab s (selectChild tabs)
          fun close s = 
              (withTab s (removeChild tabs);
               tm := List.filter (fn (s',_) => s<>s') (!tm))
      in ret ((tabs,tm), {select=select,close=close})
      end)

  fun set_onClose (w:widget) (f: unit -> bool) : unit =
      JsCore.exec2{stmt="w.onClose = f;",
                   arg1=("w",JsCore.fptr),
                   arg2=("f",JsCore.==>(JsCore.unit,JsCore.bool)),
                   res=JsCore.unit} (w,f)

  fun set_onShow (w:widget) (f: unit -> unit) : unit =
      JsCore.exec2{stmt="w.onShow = f;",
                   arg1=("w",JsCore.fptr),
                   arg2=("f",JsCore.==>(JsCore.unit,JsCore.unit)),
                   res=JsCore.unit} (w,f)

  structure Menu = struct
    type menu = foreignptr * bool
    fun mk h : (widget * menu) M =
      fn (f: widget * menu -> unit) =>
         require1 "dijit/MenuBar" (fn MenuBar =>
         let val menubar = new MenuBar(h)
         in f (menubar,(menubar,true))
         end)
        
    fun menu (m,_) s =
      fn (f: menu -> unit) =>
         require1 "dijit/PopupMenuBarItem" (fn PopupMenuBarItem =>
         require1 "dijit/DropDownMenu" (fn DropDownMenu =>
         let val dropdownmenu = new DropDownMenu([])
             val popupmenubaritem = 
                 JsCore.exec3{arg1=("C",JsCore.fptr),
                              arg2=("s",JsCore.string),
                              arg3=("d",JsCore.fptr),
                              stmt="return new C({label: s, popup: d});",
                              res=JsCore.fptr} (PopupMenuBarItem, s, dropdownmenu)
             val () = addChild m popupmenubaritem
         in f (dropdownmenu, false)
         end))

    fun item (m,top) (s,i,onclick) : unit M =
        let val path = if top then "dijit/MenuBarItem" else "dijit/MenuItem"
        in fn (f : unit -> unit) =>
              require1 path (fn MenuItem =>
              let val h = [("label",s)]
                  val h = case i of SOME p => p :: h
                                  | NONE => h
                  val i = new MenuItem(h)           
                  val () = JsCore.exec2{arg1=("i",JsCore.fptr),
                                        arg2=("f",JsCore.==>(JsCore.unit,JsCore.unit)),
                                        stmt="i.set('onClick', f);",
                                        res=JsCore.unit} (i,onclick)
              in addChild m i
               ; f()
              end)
        end
  end

  structure JsUtil = struct

    fun mk_con0 (con:string) (h:foreignptr) : foreignptr M = 
      fn (k: foreignptr -> unit) => require1 con (fn F => k(new0 F h))

    fun mk_con (con:string) (h:hash) : foreignptr M = mk_con0 con (mkHash h)

    fun get (p:string) (obj:foreignptr) : string =
        JsCore.exec2{arg1=("obj",JsCore.fptr),
                     arg2=("p",JsCore.string),
                     stmt="return obj.get(p);",
                     res=JsCore.string} (obj,p)
    fun set (p:string) (obj:foreignptr) (v:string) : unit =
        JsCore.exec3{arg1=("obj",JsCore.fptr),
                     arg2=("p",JsCore.string),
                     arg3=("v",JsCore.string),
                     stmt="obj.set(p,v);",
                     res=JsCore.unit} (obj,p,v)

    fun callFptrArr f xs =
        JsCore.exec2{arg1=("f",JsCore.fptr),arg2=("xs",JsCore.fptr),res=JsCore.fptr,
                     stmt="return f(xs);"} (f,JsCore.Array.fromList JsCore.fptr xs)

  end

  structure TextBox = struct
    type t = foreignptr
    type 'a M = 'a M
    val mk = JsUtil.mk_con "dijit/form/TextBox"
    val getValue = JsUtil.get "value"
    val setValue = JsUtil.set "value"
    val domNode = domNode
    fun toForeignPtr x = x
  end

  structure NumberTextBox = struct
    type t = foreignptr
    type 'a M = 'a M
    val mk = JsUtil.mk_con "dijit/form/NumberTextBox"
    val getValue = JsUtil.get "value"
    val setValue = JsUtil.set "value"
    val domNode = domNode
    fun toForeignPtr x = x
  end

  structure DateTextBox = struct
    type t = foreignptr
    type 'a M = 'a M
    val mk = JsUtil.mk_con "dijit/form/DateTextBox"
    val getValue = JsUtil.get "value"
    val setValue = JsUtil.set "value"
    val domNode = domNode
    fun toForeignPtr x = x
  end

  structure Button = struct
    type t = foreignptr
    fun mk (h:hash) (f:unit->unit) : t M =
        let val h = mkHash h
            val () = JsCore.exec2{arg1=("h",JsCore.fptr), arg2=("f",JsCore.==>(JsCore.unit,JsCore.unit)),
                                  res=JsCore.unit,stmt="h.onClick = f;"} (h,f)
        in JsUtil.mk_con0 "dijit/form/Button" h
        end
    val domNode = domNode
    fun toForeignPtr x = x    
  end           


  structure RestGrid = struct
    type t = {grid: foreignptr, store: foreignptr} 
    datatype colspec = VALUE of {field:string,label:string,
                                 editor:string option,sortable:bool}
                     | DELETE of {label:string}

    fun fromColspec idProperty store (VALUE {field,label,editor,sortable}) =
        let val h = [("field",field),("label",label)]
            val h = case editor of
                        SOME ed => let val h = mkHash(h @ [("editor",ed),("editOn","dblclick")])
                                       val () = JsCore.Object.set JsCore.bool h "autoSave" true
                                   in h
                                   end
                      | NONE => mkHash h
            val () = JsCore.Object.set JsCore.bool h "sortable" sortable
        in h
        end
      | fromColspec idProperty store (DELETE {label}) =
        let val h = [("field","delete"),("label",label)]
            val h = mkHash h
            val () = JsCore.Object.set JsCore.bool h "sortable" false
            val () = JsCore.Object.set (JsCore.====>(JsCore.fptr,JsCore.fptr,JsCore.fptr,JsCore.unit)) h "renderCell" 
                                        (fn (obj,value,node) => 
                                            let val nodeElem = Js.Element.fromForeignPtr node
                                                val img = Js.Element.taga0 "img" [("style","height:16px;"),("src","/trash32.png")]
                                            in Js.appendChild nodeElem img
                                             ; Js.installEventHandler img Js.onclick (fn () => (JsCore.method1 JsCore.int JsCore.unit store "remove" (JsCore.Object.get JsCore.int obj idProperty);
                                                                                                true))
                                            end)
        in h
        end
                                               
    fun mk {target: string, idProperty: string} (colspecs:colspec list) : t M =
        fn (k: t -> unit) =>
        require1 "dojo/_base/declare" (fn declare =>
        require1 "dgrid/OnDemandGrid" (fn OnDemandGrid =>
        require1 "dgrid/Keyboard" (fn Keyboard =>
        require1 "dgrid/Selection" (fn Selection =>
        require1 "dgrid/Editor" (fn Editor =>
        require1 "dstore/Rest" (fn Rest =>
        require1 "dstore/Trackable" (fn Trackable =>
        let val MyRest = JsUtil.callFptrArr declare [Rest,Trackable]
            val h = [("target",target),("idProperty",idProperty)]
            val store = new MyRest(h)
            val MyGrid = JsUtil.callFptrArr declare [OnDemandGrid,Keyboard,Selection,Editor]
            val cols = List.map (fromColspec idProperty store) colspecs
            val columns = JsCore.Array.fromList JsCore.fptr cols
            val arg = JsCore.Object.fromList JsCore.fptr [("columns",columns),
                                                          ("collection",store)]
            val grid = new0 MyGrid(arg)
            val res = k {grid=grid,store=store}
        in res
        end)))))))

    type object = foreignptr
    fun put ({grid,store}:t) (obj:foreignptr) : unit =
        (JsCore.method1 JsCore.fptr JsCore.unit store "put" obj;
         JsCore.method0 JsCore.unit grid "refresh")

    val domNode : t -> Js.elem = fn ({grid,...}) => domNode grid
    val toForeignPtr : t -> foreignptr = #grid
    val toStore : t -> foreignptr = #store
    val toWidget : t -> widget = #grid
  end

  structure Grid = struct
    type t = foreignptr
    fun mk h cols : t M =
      fn (f: t -> unit) =>
         require1 "dgrid/Grid" (fn Grid =>
         let val h = mkHash h
             val () = JsCore.Object.set JsCore.fptr h "columns" (mkHash cols)
             val grid = new0 Grid(h)
         in f grid
         end)
    fun add (g:t) (hs:hash list) : unit =
        let val hs = List.map mkHash hs
            val a = JsCore.Array.fromList JsCore.fptr hs
        in JsCore.method1 JsCore.fptr JsCore.unit g "renderArray" a
        end
    val domNode = domNode
    fun toForeignPtr x = x
  end

  structure Icon = struct
    fun wrap s = ("iconClass","dijitIcon dijitIcon" ^ s)
    val save = wrap "Save"
    val print = wrap "Print"
    val cut = wrap "Cut"
    val copy = wrap "Copy"
    val clear = wrap "Clear"
    val delete = wrap "Delete"
    val undo = wrap "Undo"
    val edit = wrap "Edit"
    val newTask = wrap "NewTask"
    val editTask = wrap "EditTask"
    val editProperty = wrap "EditProperty"
    val task = wrap "Task"
    val filter = wrap "Filter"
    val configure = wrap "Configure"
    val search = wrap "Search"
    val application = wrap "Application"
    val bookmark = wrap "Bookmark"
    val chart = wrap "Chart"
    val connector = wrap "Connector"
    val database = wrap "Database"
    val documents = wrap "Documents"
    val mail = wrap "Mail"
    val leaf = wrap "Leaf"
    val file = wrap "File"
    val function = wrap "Function"
    val key = wrap "Key"
    val package = wrap "Package"
    val sample = wrap "Sample"
    val table = wrap "Table"
    val users = wrap "Users"
    val folderClosed = wrap "FolderClosed"
    val folderOpen = wrap "FolderOpen"
    val error = wrap "Error"
  end

  structure EditorIcon = struct
  fun wrap s = ("iconClass","dijitEditorIcon dijitEditorIcon" ^ s)
  val sep = wrap "Sep"
  val save = wrap "Save"
  val print = wrap "Print"
  val cut = wrap "Cut"
  val copy = wrap "Copy"
  val paste = wrap "Paste"
  val delete = wrap "Delete"
  val cancel = wrap "Cancel"
  val undo = wrap "Undo"
  val redo = wrap "Redo"
  val selectAll = wrap "SelectAll"
  val bold = wrap "Bold"
  val italic = wrap "Italic"
  val underline = wrap "Underline"
  val strikethrough = wrap "Strikethrough"
  val superscript = wrap "Superscript"
  val subscript = wrap "Subscript"
  val justifyCenter = wrap "JustifyCenter"
  val justifyFull = wrap "JustifyFull"
  val justifyLeft = wrap "JustifyLeft"
  val justifyRight = wrap "JustifyRight"
  val indent = wrap "Indent"
  val outdent = wrap "Outdent"
  val listBulletIndent = wrap "ListBulletIndent"
  val listBulletOutdent = wrap "ListBulletOutdent"
  val listNumIndent = wrap "ListNumIndent"
  val listNumOutdent = wrap "ListNumOutdent"
  val tabIndent = wrap "TabIndent"
  val leftToRight = wrap "LeftToRight"
  val rightToLeft = wrap "RightToLeft"
  val toggleDir = wrap "ToggleDir"
  val backColor = wrap "BackColor"
  val foreColor = wrap "ForeColor"
  val hiliteColor = wrap "HiliteColor"
  val newPage = wrap "NewPage"
  val insertImage = wrap "InsertImage"
  val insertTable = wrap "InsertTable"
  val space = wrap "Space"
  val insertHorizontalRule = wrap "InsertHorizontalRule"
  val insertOrderedList = wrap "InsertOrderedList"
  val insertUnorderedList = wrap "InsertUnorderedList"
  val createLink = wrap "CreateLink"
  val unlink = wrap "Unlink"
  val viewSource = wrap "ViewSource"
  val removeFormat = wrap "RemoveFormat"
  val fullScreen = wrap "FullScreen"
  val wikiword = wrap "Wikiword"
  end
end

