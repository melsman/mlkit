structure SmlToJsCompTest = struct
  open SmlToJsComp

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

  fun htmlencode s : string =
      let fun enc #"<" = "&lt;"
	    | enc #">" = "&gt;"
	    | enc #"&" = "&amp;"
	    | enc #"\"" = "&quot;"
	    | enc c = String.str c
      in String.translate enc s 
      end
  fun pr_attr [] = ""
    | pr_attr attrs = " " ^ String.concatWith " " attrs
  fun tag t attr e = "<" ^ t ^ pr_attr attr ^ ">" ^ e ^ "</" ^ t ^ ">"
  fun qq s = "'" ^ s ^ "'"
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
  val smlarea = tag "textarea" ["id=" ^ qq(id_smlarea),"cols='75'", "rows='40'"] init 

  val id_outarea = "outarea"
  val outarea = tag "textarea" ["id=" ^ qq(id_outarea),"cols='75'", "rows='40'"] ""

  val id_execbutton = "execbutton"
  val execbutton = tag "input" ["type='button'", "id=" ^ qq(id_execbutton),"value='Compile and Run'"] ""

  fun getElem id =
      case Js.getElementById Js.document id of
        SOME e => e
      | NONE => raise Fail ("failed to find " ^ qq id ^ " in doc")

  fun out s =
      let (*val s = htmlencode s*)
        val e = getElem id_outarea
      in Js.appendChild e (Js.createTextNode s)
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

  infix ++
  fun e ++ e' = Env.plus (e,e')
  fun load_env n =
      let val () = out "."
        val eb_s = JsCore.exec0{stmt="return " ^ n ^ "_sml_eb;",res=JsCore.string}()
      (* val () = out ("Unpickling " ^ n ^ "\n") *)
      in #1(Pickle.unpickler Env.pu (Pickle.fromString eb_s))
      end handle ? => (out ("load_env problem: " ^ exnMsg ? ^ "\n"); raise ?)

  fun exec() =
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
        fun exec0 s =
            let val e = load_env_all()
                val (e',mc) = compile (e,s)
            in execute mc 
               handle ? => print ("Uncaught exception " ^ General.exnName ? ^ "\n") 
            end 
        val s = Js.value (getElem id_smlarea)
        val res = exec_print exec0 s              
      in out res
      end

  fun load_envs e nil = (envRef := SOME e; out " Done]\n")
    | load_envs e (n::ns) = 
      (Js.setTimeout 0 (fn () => load_envs (e ++ load_env n) ns); 
       ())

  val path = ""

(*  val path = "./../../" *)

  fun load n =
      print (tag "script" ["type='text/javascript'", "src='" ^ path ^ "js/basis/MLB/Js/" ^ n ^ ".sml.o.eb.js'"] "")

  val smltojs_logo_path = "js/smltojs_logo_color160.png"

  val () = List.app load basislibs

  val () = print "<html>"
  val () = print "<body>"
  val () = print "<table>"
  val () = print "<tr><th align='left'><h2>SMLtoJs Prompt</h2></th><td>&nbsp;<td></tr>"
  val () = print "<tr><td align='left'><i>Compile and Run your Standard ML programs in a Browser!</i></td><td align='right'><i>Works on Google Chrome and Firefox 3.5.5</i></td></tr>"
  val () = print "<tr>"           
  val () = print (tag "td" ["align='left'"] ("[" ^ (tag "a" ["href='js/doc/str_idx.html'","target='_blank'"] "Structure Index") ^ " | " ^ 
                               (tag "a" ["href='js/doc/sig_idx.html'","target='_blank'"] "Signature Index") ^ " | " ^ 
                               (tag "a" ["href='js/doc/id_idx.html'","target='_blank'"] "Id Index") ^ "]"))
  val () = print (tag "td" ["align='right'"] execbutton)
  val () = print "</tr>"
  val () = print (tag "tr" [] (tag "td" [] smlarea ^ tag "td" [] outarea))

  val contributed = "Contributed by <a href='http://www.elsman.com'>Martin Elsman</a>"
  val hosted = "Hosted by <a href='http://www.itu.dk'>IT University of Copenhagen</a>"
  val logo = 
      "<a href='http://www.itu.dk/people/mael/smltojs'><img border='0' alt='SMLtoJs Logo' src='" ^ 
      smltojs_logo_path ^ 
      "'/></a>"

  val () = 
      print (tag "tr" [] 
                 (tag "td" ["colspan='2'", "padding='0'"] 
                      (tag "table" ["width='100%'"]
                           (tag "tr" []
                                ((tag "td" ["align='left'"] contributed) ^
                                 (tag "td" ["align='center'"] hosted) ^ 
                                 (tag "td" ["align='right'"] logo))))))
  val () = print "</body>"
  val () = print "</html>"

  fun onload() =
      let 
        val () = out "[Loading Basis Library "
        val _ = load_envs (Env.initial()) basislibs
      in Js.installEventHandler (getElem id_execbutton) Js.onclick (fn () => (exec(); false))
      end

  fun setWindowOnload (f: unit -> unit) : unit = 
      JsCore.exec1{arg1=("a",JsCore.==>(JsCore.unit,JsCore.unit)),
                   stmt="return window.onload=a;",
                   res=JsCore.unit} f

  val () = setWindowOnload onload

end
