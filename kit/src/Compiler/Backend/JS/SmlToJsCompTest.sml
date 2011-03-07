structure SmlToJsCompTest = struct
  open SmlToJsComp

  fun timeit f x =
      let val rt = Timer.startRealTimer()
        val res = f x
        val t = Timer.checkRealTimer rt
      in (res,t)
      end      

  fun getPage (url:string) : string =
      let val r = Js.XMLHttpRequest.new()
          val () = print "p1\n"
          val () = Js.XMLHttpRequest.openn r {method="POST",url=url,async=false}
          val () = print "p2\n"
          val () = Js.XMLHttpRequest.send r NONE
          val () = print "p3\n"
          val res = Js.XMLHttpRequest.response r
      in case res of
           SOME s => s
         | NONE => raise (Fail ("makeRequest.no response; state=" ^ 
                                 Int.toString (Js.XMLHttpRequest.state r)))
      end handle e => raise Fail ("getPage: " ^ General.exnMessage e)

  fun qq s = "'" ^ s ^ "'"

  open Js.Element
  infix &

  val init = 
      String.concatWith "\n"
      ["fun loop (n,acc) : IntInf.int =",
       "  if n = 0 then acc",
       "  else loop(n-1,n*acc)",
       "",
       "fun fac n =", 
       "  print (\"fac(\" ^ IntInf.toString n ^ \") = \" ^", 
       "         IntInf.toString (loop(n,1)) ^ \"\\n\")",
       "",
       "val () = List.app fac [10,20,30,40]"
      ]

  val id_smlarea = "smlarea"
  val smlarea = 
      taga "div" [("class","border")]
          (taga "textarea" [("id",id_smlarea),("cols","200"), ("rows","20")] ($init))
  val outarea = taga0 "textarea" [("cols","200"),("rows","20")]
  val execbutton = taga0 "input" [("type","button"),("value","Compile and Run")]
  val clearoutbutton = taga0 "input" [("type","button"),("value","Clear Output")]
  val clearinbutton = taga0 "input" [("type","button"),("value","Clear Input")]

  fun getElem id =
      case Js.getElementById Js.document id of
        SOME e => e
      | NONE => raise Fail ("failed to find " ^ qq id ^ " in doc")

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

  val basislibs = ["Initial","General","Option", "List", "ListPair",
                   "Vector", "VectorSlice", "Array", "ArraySlice", "Array2", "ByteTable", "ByteSlice", 
                   "StringCvt", "String2", "Substring", "Text", "Bool", "IntInfRep", 
                   "Word32", "Word8", "Word31", "Pack32Little", "Pack32Big", "Byte",                   
                   "Int32", "Int31",
                   "Math", "Real",
                   "IntInf",
                   "Time", "Random", "Path", "Date", "Timer", "TextIO",
                   "JsCore", "Js"
                  ]
  val envRef : Env.t option ref = ref NONE

  fun exnMsg (e:exn) : string = prim("execStmtJS", ("return e.toString()","e",e))

  val outRef : (string -> unit) ref = ref (fn _ => raise Fail "outRef not initialized")
  fun out s = !outRef s

  infix ++
  fun e ++ e' = Env.plus (e,e')
  fun load_env n =
      let val () = out "."
        val eb_s = JsCore.exec0{stmt="return " ^ n ^ "_sml_eb;",res=JsCore.string}()
      (* val () = out ("Unpickling " ^ n ^ "\n") *)
      in Pickle.unpickle Env.pu eb_s
      end handle ? => (out ("load_env problem: " ^ exnMsg ? ^ "\n"); raise ?)

  fun exec editor =
      let
        fun load_env_all() =
            case !envRef of
              SOME e => e
            | NONE => raise Fail "impossible: load_env_all"
(*
              let val e = foldl (fn (n,a) => a ++ load_env n) (Env.initial()) basislibs
              in envRef := SOME e
               ; e
              end
*)
        val timing = true
        fun printtime s t = 
            if timing then print ("[" ^ s ^ " time: " ^ Time.toString t ^ "]\n")
            else ()
        fun exec0 s =
            let val e = load_env_all()
                val ((e',mc),compiletime) = timeit compile (e,s)
                val _ = printtime "Compile" compiletime
            in
              let val ((),exectime) = timeit execute mc
              in print "\n"; 
                 printtime "Execution" exectime
              end handle ? => print ("Uncaught exception " ^ General.exnName ? ^ "\n") 
            end 
        val s = CodeMirror.getCode editor
        val res = exec_print exec0 s              
      in out res
      end

  fun load_envs e nil = (envRef := SOME e; out " Done]\n")
    | load_envs e (n::ns) = 
      (Js.setTimeout 0 (fn () => load_envs (e ++ load_env n) ns); 
       ())

  val path = ""

(*  val path = "./../../" *)

  val topelem = Js.documentElement Js.document

  fun appendToTop e = Js.appendChild topelem e

  fun load n =
      appendToTop (taga0 "script" [("type","text/javascript"), 
                                   ("src", path ^ "js/basis/MLB/Js/" ^ n ^ ".sml.o.eb.js")])

  val smltojs_logo_path = "js/smltojs_logo_color160.png"
  val contributed = $"Contributed by " & taga "a" [("href","http://www.elsman.com")] ($"Martin Elsman")
  val hosted = $"Hosted by " & taga "a" [("href","http://www.itu.dk")] ($"IT University of Copenhagen")
  val logo = taga "a" [("href","http://www.itu.dk/people/mael/smltojs")]
                  (taga0 "img" [("border","0"),("alt","SMLtoJs Logo"),("src",smltojs_logo_path)])

  val () = List.app load basislibs

  val head_tr =
      tag "tr"
          (taga "th" [("align","left")]
                (tag "h2" ($"SMLtoJs Prompt")) & 
                tag "td" ($""))      

  val comments_tr =
      tag "tr"
          (taga "td" [("align","left")]
                (tag "i" ($"Compile and Run your Standard ML programs in a Browser!!")) &
                taga "td" [("align","right")]
                (tag "i" ($"Works on Google Chromium and Firefox 3.5.5")))

  val link_tr =
      tag "tr"
          (taga "td" [("align","left")]
                ($"[" & 
                  taga "a" [("href","js/doc/str_idx.html"),("target","_blank")] 
                  ($"Structure Index") & 
                  ($" | " & 
                    (taga "a" [("href","js/doc/sig_idx.html"),("target","_blank")] 
                          ($"Signature Index") &
                          ($" | " & 
                            (taga "a" [("href","js/doc/id_idx.html"),("target","_blank")] 
                                  ($"Id Index") &
                                  ($"]")))))) &
                taga "td" [("align","right")] (clearinbutton & clearoutbutton & execbutton))

  val middle_tr =
      tag "tr"
          (taga "td" [("width","50%"),("height","100%")] smlarea &
                taga "td" [("width","50%")] outarea)

  val footer_tr =
      tag "tr"
      (taga "td" [("colspan","2"), ("padding","0")] 
       (taga "table" [("width","100%")]
             (tag "tr"
                  (taga "td" [("align","left")] contributed &
                        (taga "td" [("align","center")] hosted &
                              (taga "td" [("align","right")] logo))))))

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

  fun onload() =
      let
        val editor = mkEditor "sml" "500px" id_smlarea
        fun whileSome f g =
            case f() of
              SOME x => (g x; whileSome f g)
            | NONE => ()
        fun clearoutarea () =
            (whileSome (fn() => Js.firstChild outarea)
                       (fn e => Js.removeChild outarea e);
             true)
        fun clearsmlarea () =
            (CodeMirror.setCode editor ""; true)
        fun outfun s =
            Js.appendChild outarea (Js.createTextNode s)
        val () = outRef := outfun
        val () = out "[Loading Basis Library "
        val _ = load_envs (Env.initial()) basislibs
      in Js.installEventHandler execbutton Js.onclick (fn () => (exec editor; false));
         Js.installEventHandler clearoutbutton Js.onclick clearoutarea;
         Js.installEventHandler clearinbutton Js.onclick clearsmlarea
      end

  fun setWindowOnload (f: unit -> unit) : unit = 
      JsCore.exec1{arg1=("a",JsCore.==>(JsCore.unit,JsCore.unit)),
                   stmt="return window.onload=a;",
                   res=JsCore.unit} f

  val () = setWindowOnload onload

end
