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
      [
(*       "infix ^",
       "fun (s : string) ^ (s' : string) : string = prim (\"concatStringML\", (s, s'))",
*)
       "val a = \"hello \" ^ \"world\"",
(*       "fun print (s:string) : unit = prim(\"printStringML\", s)", *)
       "val () = print a"]
  val id_smlarea = "smlarea"
  val smlarea = tag "textarea" ["id=" ^ qq(id_smlarea),"cols='100'", "rows='25'"] init 

  val id_outarea = "outarea"
  val outarea = tag "textarea" ["id=" ^ qq(id_outarea),"cols='100'", "rows='25'"] ""

  val id_execbutton = "execbutton"
  val execbutton = tag "input" ["type='button'", "id=" ^ qq(id_execbutton),"value='Execute'"] ""

  val () = print (tag "h1" [] "SmlToJs Prompt")
  val () = print (tag "h4" [] "Type in some Standard ML code")
  val () = print smlarea
  val () = print outarea
  val () = print "<br/>"
  val () = print execbutton

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

  val basislibs = ["Initial","General","Option","LIST"]
  val envRef : Env.t option ref = ref NONE

  fun exnMsg (e:exn) : string = prim("execStmtJS", ("return e.toString()","e",e))

  fun expensive() =
      let
        fun myloop (n,a) = if n < 0 then a
                           else myloop(n-1,a+2)
        val res = myloop(20000,0)
      in "Expensive: " ^ Int.toString res ^ "\n"
      end handle e => (exnMsg e ^ "\n")

  fun exec() =
      let
        infix ++
        fun e ++ e' = Env.plus (e,e')
        fun load_env n =
            let val () = print ("Loading " ^ n ^ "\n")
                val eb_s = JsCore.exec0{stmt="return " ^ n ^ "_sml_eb;",res=JsCore.string}()
                val () = print ("Unpickling " ^ n ^ "\n")
            in #1(Pickle.unpickler Env.pu (Pickle.fromString eb_s))
            end handle ? => (print ("load_env problem: " ^ exnMsg ? ^ "\n"); raise ?) 
        fun load_env_all() =
            case !envRef of
              SOME e => e
            | NONE =>
              let val e = foldl (fn (n,a) => a ++ load_env n) Env.initial basislibs
              in envRef := SOME e
               ; e
              end
        fun exec0 s =
            let val () = print (expensive())
                val e = load_env_all()
                val (e',mc) = compile (e,s)
            in execute mc 
               handle ? => print ("Uncaught exception " ^ General.exnName ? ^ "\n") 
            end 
        val s = Js.value (getElem id_smlarea)
        val res = exec_print exec0 s              
      in out res
      end

  val () = Js.installEventHandler (getElem id_execbutton) Js.onclick (fn () => (exec(); false))


  fun load n =
      print (tag "script" ["type='text/javascript'", "src='./../../js/basis/MLB/Js/" ^ n ^ ".sml.o.eb.js'"] "")

  val () = List.app load basislibs

end
