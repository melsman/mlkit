structure SmlToJsAppArg : APP_ARG = struct
  open SmlToJsComp

  open Js.Element
  infix &

  val codemirror_module = "sml"
  val application_title = "SMLtoJs Prompt"
  val application_teaser = "Compile and Run your Standard ML programs in a Browser!!"

  val logo_path = "js/smltojs_logo_color160.png"
  val logo = taga "a" [("href","http://www.itu.dk/people/mael/smltojs")]
                  (taga0 "img" [("border","0"),("alt","Logo"),("src",logo_path)])

  val contributed = $"Contributed by " & taga "a" [("href","http://www.elsman.com")] ($"Martin Elsman")
  val hosted = $"Hosted by " & taga "a" [("href","http://www.itu.dk")] ($"IT University of Copenhagen")

  val footer =
      taga "table" [("width","100%")]
           (tag "tr"
                (taga "td" [("align","left")] contributed &
                      (taga "td" [("align","center")] hosted &
                            (taga "td" [("align","right")] logo))))      

  val links = 
      $"[" & 
       taga "a" [("href","js/doc/str_idx.html"),("target","_blank")] 
       ($"Structure Index") & 
       ($" | " & 
         (taga "a" [("href","js/doc/sig_idx.html"),("target","_blank")] 
               ($"Signature Index") &
               ($" | " & 
                 (taga "a" [("href","js/doc/id_idx.html"),("target","_blank")] 
                       ($"Id Index") &
                       ($"]")))))

  val initinput = 
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

  fun timeit f x =
      let val rt = Timer.startRealTimer()
        val res = f x
        val t = Timer.checkRealTimer rt
      in (res,t)
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

  val script_paths = 
      let val path = ""  (*  val path = "./../../" *)
          fun scriptpath n = path ^ "js/basis/MLB/Js/" ^ n ^ ".sml.o.eb.js"
      in List.map scriptpath basislibs
      end

  val envRef : Env.t option ref = ref NONE

  fun exnMsg (e:exn) : string = prim("execStmtJS", ("return e.toString()","e",e))

  fun compute inputstring =
      let
        fun load_env_all() =
            case !envRef of
              SOME e => e
            | NONE => raise Fail "impossible: load_env_all"
        val timing = true
        fun printtime s t = 
            if timing then print ("[" ^ s ^ " time: " ^ Time.toString t ^ "]\n")
            else ()
        val e = load_env_all()
        val ((e',mc),compiletime) = timeit compile (e,inputstring)
        val _ = printtime "Compile" compiletime
      in
        let val ((),exectime) = timeit execute mc
        in print "\n"; 
           printtime "Execution" exectime
        end handle ? => print ("Uncaught exception " ^ General.exnName ? ^ "\n") 
      end 

  fun onloadhook {out : string -> unit} =
      let
        infix ++
        fun e ++ e' = Env.plus (e,e')
        fun load_env n =
            let val () = out "."
                val eb_s = JsCore.exec0{stmt="return " ^ n ^ "_sml_eb;",res=JsCore.string}()
            (* val () = out ("Unpickling " ^ n ^ "\n") *)
            in Pickle.unpickle Env.pu eb_s
            end handle ? => (out ("load_env problem: " ^ exnMsg ? ^ "\n"); raise ?)                            

        fun load_envs e nil = (envRef := (SOME e); out " Done]\n")
          | load_envs e (n::ns) = 
            (Js.setTimeout 0 (fn () => load_envs (e ++ load_env n) ns); ())
      in out "[Loading Basis Library ";
         load_envs (Env.initial()) basislibs
      end
end

structure SmlToJsCompTest = AppFun(SmlToJsAppArg)
