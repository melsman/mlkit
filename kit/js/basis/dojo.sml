
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
      JsCore.exec1{stmt="c.startup();",arg1=("c",JsCore.fptr),res=JsCore.unit} c

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
      JsCore.exec2{stmt="e.addChild(p);", arg1=("e",JsCore.fptr),
                   arg2=("p",JsCore.fptr), res=JsCore.unit} (e, p)

  fun mkHash h =
      let val hash = JsCore.exec0{stmt="return {};", res=JsCore.fptr} ()
          val () = List.app (fn (k,v) => JsCore.setProperty hash JsCore.string k v) h
      in hash
      end

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

  fun titlePane (h:hash) (w: widget) : widget M =
      fn (f: widget -> unit) =>
         require1 "dijit/TitlePane"
                  (fn Tp => 
                      let val p = new Tp(h)
                          val () = JsCore.exec2{arg1=("p",JsCore.fptr),
                                                arg2=("w",JsCore.fptr),
                                                stmt="p.addChild(w);",
                                                res=JsCore.unit}(p,w)
                      in f p
                      end)

  fun linkPane (h:hash) : widget M =
      fn (f: widget -> unit) =>
         require1 "dijit/layout/LinkPane"
                  (fn Lp => 
                      let val p = new Lp(h)
                      in f p
                      end)

  fun setProperties h w =
      List.app (fn (k,v) => JsCore.setProperty w JsCore.string k v) h

  fun setContent w s =
    JsCore.exec2{stmt="w.set('content', s);",
                 arg1=("w",JsCore.fptr), arg2=("s",JsCore.string),
                 res=JsCore.unit} (w,s)

  type treeStore = foreignptr
  fun treeStore (hs:hash list) : treeStore M =
      fn (f: treeStore -> unit) =>
         require1 "dojo/store/Memory"
                  (fn Memory => 
                      let val data = JsCore.exec0{stmt="return new Array();",res=JsCore.fptr}()
                          fun add d h =
                              let val h = mkHash h
                              in JsCore.exec2{stmt="d.push(h);",arg1=("d",JsCore.fptr),arg2=("h",JsCore.fptr),res=JsCore.unit} (d,h)
                              end
                          val () = List.app (add data) hs
                          val h = JsCore.exec1{stmt="return {data:data,getChildren:function(obj) { return this.query({parent:obj.id}); }};",
                                               arg1=("data",JsCore.fptr),
                                               res=JsCore.fptr} data
                          val p = new0 Memory(h)
                      in f p
                      end)
  exception DojoUnimplemented
  val treeStoreAdd       : treeStore -> hash -> unit = fn _ => raise DojoUnimplemented
  val treeStoreRemove    : treeStore -> string -> unit = fn _ => raise DojoUnimplemented
  fun tree (h: hash) rootId onClick (store:treeStore) : widget M =
      fn (f: widget -> unit) =>
         require1 "dijit/tree/ObjectStoreModel" (fn ObjectStoreModel =>
         require1 "dijit/Tree" (fn Tree =>
         let val modelArg = 
                 JsCore.exec2{stmt="return {store:store,query:{id:id}};",arg1=("store",JsCore.fptr),
                              arg2=("id",JsCore.string),res=JsCore.fptr}(store,rootId)
             val model = new0 ObjectStoreModel(modelArg)
             val treeArg = mkHash h
             val () = JsCore.setProperty treeArg JsCore.fptr "model" model
             val () = JsCore.exec2{stmt="a.onClick = function(item) { f(item.id); };",
                                   arg1=("a",JsCore.fptr), arg2=("f",JsCore.==>(JsCore.string,
                                                                                JsCore.unit)),
                                   res=JsCore.unit} (treeArg,onClick)
             val tree = new0 Tree(treeArg)
         in f tree
         end))

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

