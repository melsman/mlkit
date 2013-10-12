signature APP_ARG = sig
  val codemirror_module  : string
  val application_title  : string
  val application_teaser : string
  val initinput          : string
  val compute            : string -> unit   (* output on stdout goes to the output area *)
  val links              : Js.elem
  val footer             : Js.elem
  val script_paths       : string list
  val onloadhook         : {out: string -> unit} -> unit
  val syntaxhighlight    : bool
end

functor AppFun(X : APP_ARG) : sig end =
struct

  open Js.Element
  infix &

  fun scrollDown e = 
      JsCore.exec1 {stmt="t.scrollTop = t.scrollHeight;", arg1=("t",JsCore.fptr),res=JsCore.unit}
      (Js.Element.toForeignPtr e)

  fun userAgent() =
      JsCore.exec0{stmt="return navigator.userAgent;",res=JsCore.string} ()

  val agent = String.translate (Char.toString o Char.toLower) (userAgent())
  val touchDevices = ["ipod", "ipad", "iphone", "series60", "symbian", "android", "windows ce", "blackberry"]

  fun touchScreen a =
      let fun has s = String.isSubstring s a
      in List.exists (fn s => String.isSubstring s a) touchDevices
      end

  val outarea = taga0 "textarea" [("readonly","readonly")]
  val outputarea = taga "div" [("class","textareacontainer")] outarea
  val execbutton = taga0 "input" [("type","button"),("value","Compile and Run")]
  val clearoutbutton = taga0 "input" [("type","button"),("value","Clear Output")]

  val outRef : (string -> unit) ref = ref (fn _ => raise Fail "outRef not initialized")
  fun out s = !outRef s

  fun whileSome f g =
      case f() of
          SOME x => (g x; whileSome f g)
        | NONE => ()

  fun clearoutarea () =
      whileSome (fn() => Js.firstChild outarea)
                (fn e => Js.removeChild outarea e)

  fun outfun s =
      (Js.appendChild outarea (Js.createTextNode s);
       scrollDown outarea)
          
  val topelem = Js.documentElement Js.document

  fun appendStyleLink path =
      case Js.firstChild topelem of
          SOME head =>
          Js.appendChild head (taga0 "link" [("rel","stylesheet"), ("href", path)])
        | NONE => raise Fail "appendStyleLink"

  fun appendScript path =
      case Js.firstChild topelem of
          SOME head =>
          Js.appendChild head (taga0 "script" [("type","text/javascript"), ("src", path)])
        | NONE => raise Fail "appendScript"

  fun cleanupBody () =
      case Js.firstChild topelem of
          SOME head =>
          (case Js.nextSibling head of
               SOME goodbody => 
               (case Js.nextSibling goodbody of
                    SOME badbody => Js.removeChild topelem badbody
                  | NONE => raise Fail "cleanupBody")
             | NONE => raise Fail "cleanupBody2")
        | NONE => raise Fail "cleanupBody3"

  fun createBody () =
      case Js.firstChild topelem of
          SOME head =>
          ((case Js.nextSibling head of
                SOME body => Js.removeChild topelem body
              | NONE => ());
           Js.appendChild topelem (taga0 "body" [("class","claro"), ("id", "body")]))
        | NONE => raise Fail "createBody"

  val () = appendStyleLink "dijit/themes/claro/claro.css"
  val () = appendStyleLink "codemirror/dist/css/docs.css"
  val () = appendStyleLink "appfunstyle.css"
  val () = createBody ()

  fun getElem id : Js.elem =
      case Js.getElementById Js.document id of
          SOME e => e
        | NONE => raise Fail "getElem"

(*
  fun load scriptpath =
      appendToTop (taga0 "script" [("type","text/javascript"), 
                                   ("src", scriptpath)])

  val () = List.app load X.script_paths
*)

(*
  val link_tr =
      taga "tr" [("height","30px")]
          (taga "td" [("align","left")] X.links &
                taga "td" [("align","right")] (clearinbutton & clearoutbutton & execbutton))

  val middle_tr =
      tag "tr" (taga "td" [("width","50%")] inputarea & taga "td" [("width","50%")] outputarea)

  val footer_tr =
      taga "tr" [("height","30px")]
      (taga "td" [("colspan","2")] X.footer)

  val heads =
      taga0 "link" [("rel","stylesheet"),("type","text/css"),
                    ("href","js/codemirror/dist/css/docs.css")] &
      taga0 "link" [("rel","stylesheet"),("type","text/css"),
                    ("href","js/appfun/style.css")]            

  val elem =
      tag "head" heads &
      tag "body"
       (taga "table" [("width","100%"),("height","100%")]
        (head_tr &
         comments_tr &
         link_tr &         
         middle_tr &
         footer_tr
        )
       )
 
  val _ = appendToTop elem
*)

  type editor = 
       {get: unit -> string,
        set: string -> unit}

  fun mkEditor id_inarea inarea : editor =
      if not X.syntaxhighlight orelse touchScreen agent then
        {get=fn() => Js.value inarea,
         set=fn s => case Js.firstChild inarea of
                       SOME c => Js.replaceChild inarea ($s) c
                     | NONE => Js.appendChild inarea ($s)
        }
      else
      let val kind = X.codemirror_module
          val tokenizefile =
              "../contrib/" ^ kind ^ "/js/tokenize" ^ kind ^ ".js"
          val parsefile =
              "../contrib/" ^ kind ^ "/js/parse" ^ kind ^ ".js"
          val stylefile =
              "codemirror/dist/contrib/" ^ kind ^ "/css/" ^ kind ^ "colors.css"
          val properties =
              let open CodeMirror.EditorProperties
                  val t = empty()
              in textWrapping t true
               ; lineNumbers t true
               ; path t "codemirror/dist/js/"
               ; parserfiles t [tokenizefile,parsefile]
               ; stylesheets t [stylefile] 
               ; height t "100%"
               ; width t "100%"
               ; t
              end
          val ed = CodeMirror.newEditor {id=id_inarea, properties=properties}
      in {get=fn () => CodeMirror.getCode ed,
          set=fn s => CodeMirror.setCode ed s}
      end

  fun exec_print (f:'a -> unit) (v:'a) : string =
      let val p_old = Control.printer_get()
          val buf : string list ref = ref nil
          fun p_new s = buf := (s :: !buf)
          val () = Control.printer_set p_new
          val () = f v handle _ => ()
          val res = String.concat(rev(!buf))
      in Control.printer_set p_old; 
         res
      end

  fun exec (editor:editor) =
      let val res = exec_print X.compute (#get editor ())
      in out res;
         false
      end

  val documentation =
      taga0 "iframe" [("src","doc/str_idx.html"),("style", "height:100%; width:100%; border:0;")]

  open Dojo infix >>=

  fun noop() = ()
  fun put s () = outfun (s^"\n")

  val logo = taga0 "img" [("src", "smltojs_logo_transparent_small.png"),("height","30px")]

  val menuStyle = ("style","border:0;padding:2;")

  structure Dropbox = struct
    type client = foreignptr
    type datastoremanager = foreignptr
    type datastore = foreignptr
    type table = foreignptr
    structure J = JsCore
    val op ==> = J.==> infix ==>

    fun load f = Js.loadScript "https://www.dropbox.com/static/api/dropbox-datastores-1.0-latest.js" f
    fun client k =
        J.exec1{stmt="return new Dropbox.Client({key:k});",
                arg1=("k", J.string),
                res=J.fptr} k
        
    fun callMethod0 (t:'a J.T) m (obj:foreignptr) : 'a =
        J.exec1{arg1=("obj",J.fptr),
                stmt="return obj." ^ m ^ "();",
                res=t} obj

    fun callMethod1 (a1t: 'a1 J.T, t:'a J.T) m (obj:foreignptr) (a1:'a1) : 'a =
        J.exec2{arg1=("obj",J.fptr),
                arg2=("a1",a1t),
                stmt="return obj." ^ m ^ "(a1);",
                res=t} (obj,a1)

    fun callMethod2 (a1t: 'a1 J.T, a2t: 'a2 J.T, t:'a J.T) m (obj:foreignptr) (a1:'a1,a2:'a2) : 'a =
        J.exec3{arg1=("obj",J.fptr),
                arg2=("a1",a1t),
                arg3=("a2",a2t),
                stmt="return obj." ^ m ^ "(a1,a2);",
                res=t} (obj,a1,a2)
               
    fun authenticate c (onerr: string option -> unit) =
        J.exec2{arg1=("c",J.fptr),
                arg2=("f",(J.option J.string) ==> J.unit),
                stmt="c.authenticate({interactive:false}, f);",
                     res=J.unit} (c,onerr)

    fun dropboxUid c =
        callMethod0 J.string "dropboxUid" c

    fun authenticate0 c =
        callMethod0 J.unit "authenticate" c

    fun isAuthenticated c =
        callMethod0 J.bool "isAuthenticated" c

    fun getDatastoreManager c =
        callMethod0 J.fptr "getDatastoreManager" c

    fun openDefaultDatastore dsm f =
        J.exec2{arg1=("dsm",J.fptr),
                arg2=("thef",J.===>(J.string,J.fptr,J.unit)),
                stmt="return dsm.openDefaultDatastore(function(err,ds) {thef([err,ds]);});",
                res=J.unit} (dsm,f)

    fun getTable ds s : table =
        callMethod1 (J.string, J.fptr) "getTable" ds s

    type hash = (string * string) list
    type hash0 = foreignptr

    fun mkHash (h: hash) : hash0 =
        let val hash = JsCore.exec0{stmt="return {};", res=JsCore.fptr} ()
            val () = List.app (fn (k,v) => JsCore.setProperty hash JsCore.string k v) h
        in hash
        end

    type record = foreignptr
    fun insert (t:table) (h:hash) : record =
        callMethod1 (J.fptr, J.fptr) "insert" t (mkHash h)

    fun idx (t: 'a J.T) (a: foreignptr) i : 'a =
        J.exec2{arg1=("a",J.fptr),arg2=("i",J.int),res=t,
                stmt="return a[i];"}(a,i)

    fun len (a: foreignptr) : int =
        J.getProperty a J.int "length"

    fun query t h : record list =
        let val records = callMethod1 (J.fptr, J.fptr) "query" t (mkHash h)
            fun loop i acc = 
                if i < 0 then acc 
                else loop (i-1) (idx J.fptr records i :: acc)
        in loop (len records - 1) nil
        end

    fun set t r k v =
        callMethod2 (J.string,t,J.unit) "set" r (k,v)

    fun get t r k =
        callMethod1 (J.string,t) "get" r k
  end

  val DropboxKey = "ncraemhwgbtj509"

  val filesElement = tag0 "div"

  type file = {name: string, content: unit -> string}
               
  val files : file list ref = ref nil

  fun fileNew (name: string, content: string) : unit =
      files := ({name=name,content=fn () => content} :: !files)

  fun getFile r =
      let val name = Dropbox.get JsCore.string r "name"
      in Js.appendChild filesElement (tag "p" ($name))
      end

  fun getDropboxData c =
      let val dsm = Dropbox.getDatastoreManager c
      in Dropbox.openDefaultDatastore dsm (fn (err, ds) =>           
                let val () = Js.appendChild filesElement (tag "p" ($"hello"))
                    val filesTable = Dropbox.getTable ds "files"
                    val fileRecords = Dropbox.query filesTable []
                in List.app getFile fileRecords;
                   Js.appendChild filesElement (tag "p" ($"no more files"))
                end
         )
      end

  fun menuHandle_DropboxAuth () =
      let val c = Dropbox.client DropboxKey
      in Dropbox.authenticate0 c
      end

  val authElement = tag0 "div"

  val () = 
      Dropbox.load (fn () =>
                       let val c = Dropbox.client DropboxKey
                           val () = Dropbox.authenticate c (fn _ => ())
                           val text = if Dropbox.isAuthenticated c then
                                        (getDropboxData c;
                                         "Authenticated: " ^ (Dropbox.dropboxUid c))
                                      else "Not authenticated"
                       in Js.appendChild authElement ($("Status: " ^ text))
                       end)

  val count = ref 0
  fun menuHandle_NewBuffer tabs () =
      let 
        val num = !count before count := !count + 1
        val bufname = "Untitled-" ^ Int.toString num ^ ".sml"
        val id_inarea = bufname ^ "_inarea"
        val inarea = taga "textarea" [("id",id_inarea),("style","border:0;")] ($("val a = \"" ^ bufname ^ "\""))
        val inputarea = taga "div" [("class","textareacontainer")] inarea          
        val () = run (pane [("style","height:100%;"),("title",bufname),("closable","true")] inputarea >>= (fn page =>
                      (addChild tabs page;
                       ret ())))
        val editor = mkEditor id_inarea inarea  (* requires id_inarea to be in document tree *)
        fun clearinputarea () = (#set editor ""; true)
      in Js.installEventHandler execbutton Js.onclick (fn () => exec editor)
      end

  fun menu tabs =
      Menu.mk [("region", "center"),menuStyle] >>= (fn (w_left, m_left) =>
      Menu.menu m_left "File" >>= (fn m_file =>
      Menu.item m_file ("New", SOME ("iconClass","dijitIcon dijitIconNewTask"), menuHandle_NewBuffer tabs) >>= (fn () =>
      Menu.item m_file ("Dropbox Authorize", SOME EditorIcon.copy, menuHandle_DropboxAuth) >>= (fn () =>
      Menu.item m_left ("Compile&amp;Run", NONE, put "compile&run") >>= (fn () =>
      Menu.item m_left ("Clear output", NONE, clearoutarea) >>= (fn () =>
      pane [("region", "left"),("style","padding:0;padding-right:10;background-color:#eeeeee;")] logo >>= (fn logo =>
      Menu.mk [("region", "right"),menuStyle] >>= (fn (w_right, m_right) =>
      Menu.menu m_right "Help" >>= (fn m_help =>
      Menu.item m_help ("Basis Library", SOME EditorIcon.copy, put "about") >>= (fn () =>
      Menu.item m_help ("About", SOME EditorIcon.cut, put "about") >>= (fn () =>
      Menu.item m_help ("Licence", NONE, noop) >>= (fn () =>
      layoutContainer [("region", "top"),("style","height:30px;")] [logo, w_left, w_right]
      ))))))))))))

  val everything =
      tabContainer [("region", "top"),("splitter","true"),("style","height:70%;border:0;"),("tabPosition","bottom")] [] >>= (fn tabs =>
      pane [("title","Dropbox Content")] authElement >>= (fn authpane =>
      pane [("title","Files")] filesElement >>= (fn filespane =>
      accordionContainer [("region", "left"),("splitter","true"), ("style", "width:20%;")] [authpane, filespane] >>= (fn accord =>
      menu tabs >>= (fn top =>   
      pane [("region", "center"),("style","height:30%;")] outputarea >>= (fn centerbot =>
      borderContainer [("region", "center")] [tabs,centerbot] >>= (fn center =>
      pane [("region", "right"), ("splitter","true")] documentation >>= (fn right =>
      borderContainer [("style", "height: 100%; width: 100%;")]
                      [accord,top,right,center]
      ))))))))

  val () = attachToElement (getElem "body") everything

  fun onload() =
      let
        val () = cleanupBody()
        val () = outRef := outfun
        val () = X.onloadhook {out=out}
      in ()
      end

  fun setWindowOnload (f: unit -> unit) : unit =
      let open JsCore infix ==>
      in exec1{arg1=("a", unit ==> unit),
               stmt="return window.onload=a;",
               res=unit} f
      end

  val () = setWindowOnload onload

end
