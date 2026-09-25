(* The UI and compiler share these callbacks without loading compiler code. *)
structure SmlToJsBridge = struct
  val compute : (string -> string -> unit) ref =
      ref (fn _ => fn _ => print "[Compiler is still loading. Please try again shortly.]\n")
  val initialize : ({out : string -> unit, ready : unit -> unit, failed : string -> unit} -> unit) ref =
      ref (fn _ => raise Fail "Compiler did not register its initializer")
end

structure SmlToJsAppArg : APP_ARG = struct
  open Js.Element
  infix &

  val codemirror_module = "sml"
  val application_title = "SMLtoJs Online"
  val application_logo = "smltojs_logo_transparent_small.png"

  val syntaxhighlight = true

  fun about () =
      let fun link t href () = taga "a" [("href",href), ("target","_blank")] ($t)
          val MartinElsman = link "Martin Elsman" "http://elsman.com"
          val SMLtoJs = link "SMLtoJs" "http://www.smlserver.org/smltojs"
          val Dojo = link "Dojo" "http://www.dojotoolkit.org"
          val CodeMirror = link "CodeMirror" "http://codemirror.net"
          val DropboxAPIv2 = link "Dropbox API v2" "https://www.dropbox.com/developers/documentation/http/overview"
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
              $"the lower part of the IDE. For a quick demonstration, load one " &
              $"of the Server examples from the file tree to the left.") &
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
            $"syncronizing with your Dropbox App folder. The Dropbox App-specific " &
            $"files are visible only to you.") &
       tag "h4" ($"Contributors") &
          tag "p"
             ($"The IDE is based on " & SMLtoJs() &
               $", a Standard ML to JavaScript compiler. The IDE also uses the " & Dojo() &
               $" framework as the basis for the IDE GUI widgets and the " & DropboxAPIv2() &
               $" for allowing users to store source files in Dropbox. The IDE also uses " & CodeMirror() &
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

  val script_paths = []
  val computeLabel = "Compile->Run"
  fun compute file source = (!SmlToJsBridge.compute) file source

  fun onloadhook {out : string -> unit} =
      let open JsCore infix ==>
          fun initialize () =
              (!SmlToJsBridge.initialize)
                  {out=out, ready=fn () => exec0
                      {stmt="window.smltojsLoading.ready();",res=unit} (),
                   failed=fn message => exec1
                      {stmt="window.smltojsLoading.failed(message);",
                       arg1=("message",string),res=unit} message}
      in exec2
          {stmt="window.smltojsLoading.start(initialize, report);",
           arg1=("initialize",unit ==> unit),
           arg2=("report",string ==> unit),res=unit} (initialize,out)
      end

  val dropboxKey = SOME "ybrud729cldjn66"
  val fileExtensions = ["sml","sig","mlb","txt"]
  val rightPane =
      SOME(fn () => taga0 "iframe" [("src","js/doc/str_idx.html"),("style", "height:100%; width:100%; border:0;")])
end

structure SmlToJsCompTest = AppFun(SmlToJsAppArg)
