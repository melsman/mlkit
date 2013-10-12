
structure AppFunArg = struct
  open Js.Element
  infix &
  val codemirror_module = "sml"
  val application_title = "Test of AppFun functor"
  val application_teaser = "Application teaser!"
  val syntaxhighlight = true
  val contributed = $"Contributed by someone"
  val hosted = $"Hosted by someone"

  val footer =
      taga "table" [("width","100%")]
           (tag "tr"
                (taga "td" [("align","left")] contributed &
                      (taga "td" [("align","right")] hosted)))

  val links = $"place links here"

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

  fun compute s = print "compute called\n"
  fun onloadhook _ = ()
  val script_paths = []

end

structure AppFunTest = AppFun(AppFunArg)
