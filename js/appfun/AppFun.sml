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

  val id_inarea = "inarea"
  val inarea = taga "textarea" [("id",id_inarea)] ($X.initinput)
  val outarea = taga0 "textarea" [("readonly","readonly")]
  val inputarea = taga "div" [("class","textareacontainer")] inarea          
  val outputarea = taga "div" [("class","textareacontainer")] outarea
  val execbutton = taga0 "input" [("type","button"),("value","Compile and Run")]
  val clearoutbutton = taga0 "input" [("type","button"),("value","Clear Output")]
  val clearinbutton = taga0 "input" [("type","button"),("value","Clear Input")]

  val outRef : (string -> unit) ref = ref (fn _ => raise Fail "outRef not initialized")
  fun out s = !outRef s

  val topelem = Js.documentElement Js.document

  fun appendToTop e = 
      let fun loop() =
              case Js.firstChild topelem of
                  SOME e => (Js.removeChild topelem e; loop())
                | NONE => ()
      in loop(); Js.appendChild topelem e
      end

  fun load scriptpath =
      appendToTop (taga0 "script" [("type","text/javascript"), 
                                   ("src", scriptpath)])

  val () = List.app load X.script_paths

  val head_tr =
      taga "tr" [("height","30px")]
          (taga "th" [("align","left"),("rowspan","2")]
                (tag "h2" ($X.application_title)) & 
                taga "td" [("align","right")] (tag "i" ($X.application_teaser)))      

  val comments_tr =
      taga "tr" [("height","30px")]
          (taga "td" [("align","right")]
                (tag "i" ($"Works on Google Chrome and Firefox")) & tag "td" ($""))

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

  type editor = 
       {get: unit -> string,
        set: string -> unit}

  fun mkEditor area : editor =
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
              "js/codemirror/dist/contrib/" ^ kind ^ "/css/" ^ kind ^ "colors.css"
          val properties =
              let open CodeMirror.EditorProperties
                  val t = empty()
              in textWrapping t false
               ; lineNumbers t false
               ; path t "js/codemirror/dist/js/"
               ; parserfiles t [tokenizefile,parsefile]
               ; stylesheets t [stylefile] 
               ; height t "100%"
               ; width t "100%"
               ; t
              end
          val ed = CodeMirror.newEditor {id=area, properties=properties}
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

  fun onload() =
      let
        val editor = mkEditor id_inarea
        fun whileSome f g =
            case f() of
              SOME x => (g x; whileSome f g)
            | NONE => ()
        fun clearoutarea () =
            (whileSome (fn() => Js.firstChild outarea)
                       (fn e => Js.removeChild outarea e);
             true)
        fun clearinputarea () =
            (#set editor ""; true)
        fun outfun s =
            (Js.appendChild outarea (Js.createTextNode s);
             scrollDown outarea)

        val () = outRef := outfun
        val () = X.onloadhook {out=out}
      in Js.installEventHandler execbutton Js.onclick (fn () => exec editor);
         Js.installEventHandler clearoutbutton Js.onclick clearoutarea;
         Js.installEventHandler clearinbutton Js.onclick clearinputarea
      end

  fun setWindowOnload (f: unit -> unit) : unit =
      let open JsCore infix ==>
      in exec1{arg1=("a", unit ==> unit),
               stmt="return window.onload=a;",
               res=unit} f
      end

  val () = setWindowOnload onload

end
