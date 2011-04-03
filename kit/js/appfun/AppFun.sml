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
end

functor AppFun(X : APP_ARG) : sig end =
struct

  open Js.Element
  infix &

  val id_inputarea = "inputarea"
  val inputarea = 
      taga "div" [("class","border")]
          (taga "textarea" [("id",id_inputarea),("cols","200"), ("rows","20")] ($X.initinput))
  val outarea = taga0 "textarea" [("cols","200"),("rows","20")]
  val execbutton = taga0 "input" [("type","button"),("value","Compile and Run")]
  val clearoutbutton = taga0 "input" [("type","button"),("value","Clear Output")]
  val clearinbutton = taga0 "input" [("type","button"),("value","Clear Input")]

  val outRef : (string -> unit) ref = ref (fn _ => raise Fail "outRef not initialized")
  fun out s = !outRef s

  val topelem = Js.documentElement Js.document

  fun appendToTop e = Js.appendChild topelem e

  fun load scriptpath =
      appendToTop (taga0 "script" [("type","text/javascript"), 
                                   ("src", scriptpath)])

  val () = List.app load X.script_paths

  val head_tr =
      tag "tr"
          (taga "th" [("align","left")]
                (tag "h2" ($X.application_title)) & 
                tag "td" ($""))      

  val comments_tr =
      tag "tr"
          (taga "td" [("align","left")]
                (tag "i" ($X.application_teaser)) &
                taga "td" [("align","right")]
                (tag "i" ($"Works on Google Chromium and Firefox 3.5.5")))

  val link_tr =
      tag "tr"
          (taga "td" [("align","left")] X.links &
                taga "td" [("align","right")] (clearinbutton & clearoutbutton & execbutton))

  val middle_tr =
      tag "tr"
          (taga "td" [("width","50%"),("height","100%")] inputarea &
                taga "td" [("width","50%")] outarea)

  val footer_tr =
      tag "tr"
      (taga "td" [("colspan","2"), ("padding","0")] X.footer)

  val heads =
      if false then
        taga "style" [("type","text/css")] ($("textarea {width:100%;height:100%;}")) &
             (taga0 "link" [("rel","stylesheet"),("type","text/css"),
                            ("href","js/codemirror/dist/css/docs.css")])
      else 
        taga0 "link" [("rel","stylesheet"),("type","text/css"),
                      ("href","js/codemirror/dist/css/docs.css")]
        
  val elem =
      tag "html"
      (tag "head" heads &
       tag "body"
       (tag "table"
        (head_tr &
         comments_tr &
         link_tr &         
         middle_tr &
         footer_tr
        )
       )
      )

  val _ = appendToTop elem

  fun mkEditor kind h area =
      let val tokenizefile =
              "../contrib/" ^ kind ^ "/js/tokenize" ^ kind ^ ".js"
          val parsefile =
              "../contrib/" ^ kind ^ "/js/parse" ^ kind ^ ".js"
          val stylefile =
              "js/codemirror/dist/contrib/" ^ kind ^ "/css/" ^ kind ^ "colors.css"
          val properties =
              let open CodeMirror.EditorProperties
                  val t = empty()
              in textWrapping t false
               ; lineNumbers t true
               ; path t "js/codemirror/dist/js/"
               ; parserfiles t [tokenizefile,parsefile]
               ; stylesheets t [stylefile] 
               ; height t h
               ; t
              end
      in CodeMirror.newEditor {id=area, properties=properties}
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

  fun exec editor =
      let val res = exec_print X.compute (CodeMirror.getCode editor)
      in out res;
         false
      end

  fun onload() =
      let
        val editor = mkEditor X.codemirror_module "500px" id_inputarea
        fun whileSome f g =
            case f() of
              SOME x => (g x; whileSome f g)
            | NONE => ()
        fun clearoutarea () =
            (whileSome (fn() => Js.firstChild outarea)
                       (fn e => Js.removeChild outarea e);
             true)
        fun clearinputarea () =
            (CodeMirror.setCode editor ""; true)
        fun outfun s =
            Js.appendChild outarea (Js.createTextNode s)
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
