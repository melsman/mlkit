structure SmlToJsAppArg : APP_ARG = struct
  open SmlToJsComp

  open Js.Element
  infix &

  val codemirror_module = "sml"
  val application_title = "SMLtoJs Online"
  val application_logo = "smltojs_logo_transparent_small.png"

  val syntaxhighlight = true

  fun about () =
      let fun link t href () = taga "a" [("href",href), ("target","_blank")] ($t)
          val MartinElsman = link "Martin Elsman" "http://www.elsman.com"
          val SMLtoJs = link "SMLtoJs" "http://www.smlserver.org/smltojs"
          val Dojo = link "Dojo" "http://www.dojotoolkit.org"
          val CodeMirror = link "CodeMirror" "http://codemirror.net"
          val DropboxDataStoreAPI = link "Dropbox Datastore API" "https://www.dropbox.com/developers/datastore"
          val linkMLKitRep = link "Github MLKit Repository" "https://github.com/melsman/mlkit"
      in 
        tag "p"
            ($"This Standard ML IDE allows programmers to build client-based web " &
              $"applications using Standard ML. Use the File-menu to create " &
              $"and organize files. Use the documentation to the right for " &
              $"navigating the part of the Standard ML Basis Library " &
              $"available to the programmer. Additional libraries are " &
              $"supported, including libraries Js and JsCore for interacting " &
              $"with native JavaScript. Programs are compiled and executed " &
              $"using the top-level menu item Compile->Run. The print " &
              $"function can be used to output text to the Output tab in " &
              $"the lower part of the IDE. For a quick demonstration, load the Demo-file available from " &
              $"the File-menu.") &            
       tag "p"
           ($"Created files appear in the File Tree to the left. If you " &
             $"have a Dropbox account, your files may be kept in a special " &
             $"area of your Dropbox, visible only to you. The application " &
             $"is only requesting access to this special part of your Dropbox.") &           
       tag "h4" ($"A Note on Privacy") &
       tag "p"
          ($"The programs you keep in the file tree are not visible " &
            $"to anyone but you. Information about your programs and the " &
            $"programs themselves only leaves your web-browser for " &
            $"syncronizing with your Dropbox datastore. The Dropbox " &
            $"datastore is visible only to you.") &
       tag "h4" ($"Contributors") &
          tag "p"
             ($"The IDE is based on " & SMLtoJs() & 
               $", a Standard ML to JavaScript compiler. The IDE also uses the " & Dojo() & 
               $" framework as the basis for the IDE GUI widgets and the " & DropboxDataStoreAPI() & 
               $" for allowing users to store their data in Dropbox. The IDE also uses " & CodeMirror() & 
               $" as the foundation for the Standard ML editor features, including syntax-highligting (support provided by Ken Friis Larsen). " &
               $" The sources for this IDE are" &
               $" available by download from the Github MLKit repository and are distributed" &
               $" under the GPL2 license; some parts of the sources are also available under the MIT license." &
               $" For information about licenses, please consult the sources, which are available" & 
               $" from the " & linkMLKitRep() & $".")  &
          tag "p"
          ($"The IDE and SMLtoJs are written by " & MartinElsman() & $". For information about using " & SMLtoJs() & 
            $" in an offline setting, consult the " & SMLtoJs() & $" web site.") 
      end

  val demoinput = 
      SOME(
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
      ])

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
                   "JsCore", "Js", "Html", "Rwp", "XMLrpcClient", "dojo", "utest"
                  ]

  val script_paths = 
      let fun basispath n = "js/basis/MLB/Js/" ^ n ^ ".sml.o.eb.js"
          fun implpath n = "js/basis/MLB/Js/" ^ n ^ ".js"
      in List.map basispath basislibs @
         List.map implpath ["Array2-sml","ArraySlice-sml-code1","ArraySlice-sml-code3",
                            "VectorSlice-sml-code1","VectorSlice-sml-code3",
                            "ByteTable-sml-code11","ByteTable-sml-code14","ByteTable-sml-code16",
                            "ByteTable-sml-code17","ByteTable-sml-code20","ByteTable-sml-code22",
                            "ByteTable-sml-code3","ByteTable-sml-code5","ByteTable-sml-code6",
                            "ByteTable-sml-code9",
                            "ByteSlice-sml-code1","ByteSlice-sml-code10","ByteSlice-sml-code12",
                            "ByteSlice-sml-code3","ByteSlice-sml-code4","ByteSlice-sml-code6",
                            "ByteSlice-sml-code7","ByteSlice-sml-code9",
                            "Bool-sml", "Char-sml", "Byte-sml", "StrBase-sml", "Math-sml",
                            "Html-sml","Rwp-sml",
                            "Parsercomb-sml","XMLrpcClient-sml-code1",
                            "XMLrpcClient-sml-code2", "XMLrpcClient-sml-code3","utest-sml"]  
      (* modules that are not used by SMLtoJs itself are not loaded
       * and need to be mentioned here, so that they can be
       * explicitly loaded! *)
      end

  val envRef : Env.t option ref = ref NONE

  fun exnMsg (e:exn) : string = prim("execStmtJS", ("return e.toString()","e",e))

  fun compute f inputstring =
      let
        fun load_env_all() =
            case !envRef of
              SOME e => e
            | NONE => raise Fail "impossible: load_env_all"
        val timing = true
        val () = print ("[Compiling file " ^ f ^ "]\n")
        fun printtime s t = 
            if timing then print ("[" ^ s ^ " time: " ^ Time.toString t ^ "]\n")
            else ()
        val e = load_env_all()
        val ((e',mc),compiletime) = timeit compile (e,inputstring)
        val _ = printtime "Compile" compiletime
      in
        let val () = print "[Executing]\n"
            val ((),exectime) = timeit execute mc
        in print "\n"; 
           printtime "Execution" exectime
        end handle ? => print ("Uncaught exception " ^ General.exnName ? ^ "\n") 
      end 

  val computeLabel = "Compile->Run"

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

  val dropboxKey = SOME "384tq7rviyh4lrg"
  val fileExtensions = ["sml","sig","mlb","txt"]
  val rightPane =
      SOME(fn () => taga0 "iframe" [("src","js/doc/str_idx.html"),("style", "height:100%; width:100%; border:0;")])
end

structure SmlToJsCompTest = AppFun(SmlToJsAppArg)
