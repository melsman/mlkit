
structure AppFunArg = struct
  open Js.Element
  infix &
  val codemirror_module = "sml"
  val application_title = "TestofAppFun Functor"
  val application_logo = "smltojs_logo_transparent_small.png" 
  val syntaxhighlight = true
  val about = $"This application..."
  val demoinput = SOME (String.concatWith "\n"
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

  fun compute s = print "compute called\n"
  fun onloadhook _ = ()
  val script_paths = []
  val dropboxKey = SOME "384tq7rviyh4lrg"
  val fileExtensions = ["sml","sig","mlb","txt"]
  val rightPane =
      SOME(fn () => taga0 "iframe" [("src","doc/str_idx.html"),("style", "height:100%; width:100%; border:0;")])
end

structure AppFunTest = AppFun(AppFunArg)
