
structure Benchmark = 
  struct
    type report = MemUsage.report
    fun process compile p : string * (string * string * report) option =
      case compile p
	of SOME (s,t) =>
	  let 
	    val out = t ^ ".out.txt"
	    val png = t ^ ".png"
	    val res = SOME (out, png, MemUsage.memUsage {cmd="./" ^ t,args=nil,out_file=out})
	      handle _ => NONE
	    val memusage = "memusage -t -T --title=" ^ t ^ " -p " ^ png ^ " ./" ^ t
	  in if OS.Process.system memusage = OS.Process.success then (s, res)
	     else (s, NONE)
	  end
	 | NONE => (p,NONE)

    fun tag t s = "<" ^ t ^ ">" ^ s ^ "</" ^ t ^ ">"
    fun tagAttr t a s = "<" ^ t ^ " " ^ a ^ ">" ^ s ^ "</" ^ t ^ ">"
    val TD = tag "TD"
    val TH = tag "TH"
    val TR = tag "TR"
    val B = tag "B"
    val I = tag "I"
    val H1 = tag "H1"
    val H2 = tag "H2"
    val TABLE = tagAttr "TABLE" "border=1"
    val BODY = tagAttr "BODY" "bgcolor=white"
    val HTML = tag "HTML"
    fun ppTime t = TD (Time.toString t) 
    fun ppMem n = TD (if n > 10000 then Int.toString(n div 1000) ^ "M"
		      else Int.toString n ^ "K")
      
    fun BenchTable opts (h,l) =
      let
	fun pp p opt v = 
	  if List.exists (fn opt' => opt=opt') opts then p v
	  else ""

	fun BenchLine (p, SOME (outfile, png, {count,rss,size,data,stk,exe,
					       real,user,sys})) = 
	  TR(TD(tagAttr "A" ("HREF=" ^ p) p) 
	     ^ pp ppMem  "rss"  rss 
	     ^ pp ppMem  "size" size 
	     ^ pp ppMem  "data" data 
	     ^ pp ppMem  "stk"  stk 
	     ^ pp ppMem  "exe"  exe 
	     ^ pp ppTime "real" real 
	     ^ pp ppTime "user" user 
	     ^ pp ppTime "sys"  sys 
	     ^ TD(tagAttr "A" ("HREF=" ^ outfile) "output")
	     ^ TD(tagAttr "A" ("HREF=" ^ png) "graph"))
	  | BenchLine (p, NONE) = 
	  let val sz = Int.toString(3 + length opts)
	  in TR(TD(tagAttr "A" ("HREF=" ^ p) p) ^ tagAttr "TD" ("COLSPAN=" ^ sz) " - ")
	  end
	val BenchHeader =
	  TR(TH "Program" 
	     ^ pp TH "rss"  "RSS" 
	     ^ pp TH "size" "Size" 
	     ^ pp TH "data" "Data" 
	     ^ pp TH "stk"  "Stk" 
	     ^ pp TH "exe"  "Exe"
	     ^ pp TH "real" "Real" 
	     ^ pp TH "user" "User" 
	     ^ pp TH "sys"  "Sys" 
	     ^ TH "Output" ^ TH "Graph")
      in
	H2 h ^ TABLE (concat (BenchHeader :: map BenchLine l)) ^ "<P>"
      end

    fun BenchPage p =
      HTML(BODY(H1 "KitBench Report" ^ p ^ "<HR>" ^ I "The KitBench Tool"))

    datatype compiler = MLKIT | MLKITGC | SMLNJ
    fun getCompileArgs (nil, comps, out) = NONE
      | getCompileArgs (s::ss , comps, out) = 
      case s
	of "-mlkit"   => getCompileArgs (ss, MLKIT::comps, out)
	 | "-mlkitgc" => getCompileArgs (ss, MLKITGC::comps, out)
	 | "-smlnj"   => getCompileArgs (ss, SMLNJ::comps, out)
	 | "-o" => 
	  (case ss
	     of (f::ss) => getCompileArgs (ss, comps, SOME f)
	      | _ => NONE)
	 | _ => SOME (s::ss, rev comps, out)

    fun getNameComp c =
      case  c 
	of MLKIT =>   ("ML Kit", CompileMLKIT.compile)
	 | MLKITGC => ("ML Kit GC", CompileMLKITGC.compile)
	 | SMLNJ =>   ("SML/NJ", CompileSMLNJ.compile)

    fun sourceFiles nil = nil
      | sourceFiles (input::inputs) =
      case OS.Path.ext input
	of NONE => raise Fail ("Missing extension on file " ^ input)
	 | SOME "sml" => input :: sourceFiles inputs
	 | SOME "pm" => input :: sourceFiles inputs 
	 | SOME "tst" => (case TestFile.parse input
			    of NONE => raise Fail "Parse Error"
			     | SOME (_,l) => 
			      let val ps = map (fn TestFile.SML(s,_) => s
			                         | TestFile.PM (s,_) => s) l
			      in ps @ sourceFiles inputs
			      end)
	 | SOME ext => raise Fail ("Unknown extension " ^ ext)

    fun main1 kitdir inputs c =
      let val (h,compile) = getNameComp c
	  val _ = print ("Parsing test-files\n")
	  val ps = sourceFiles inputs
	  val l = map (process (compile kitdir)) ps
      in BenchTable ["rss", "size", "data", "stk", "exe", "real", "user", "sys"] (h,l)
      end

    fun main kitdir (progname, args) =
      case getCompileArgs (args, nil, NONE)
	of NONE => 
	  (  print "USAGE: kitbench [OPTION]... FILE...\n"
	   ; print "OPTIONS:\n"
	   ; print "  -smlnj           Run SML/NJ on each test.\n"
	   ; print "  -mlkit           Run MLKIT on each test with region\n"
	   ; print "                   inference enabled.\n"
	   ; print "  -mlkitgc         Run MLKIT on each test with region\n"
	   ; print "                   inference and garbage collection enabled.\n"
	   ; print "  -o file          Write output to file ``file''."
	   ; print "FILES:\n"
	   ; print "  file.sml         Standard ML source file."
	   ; print "  file.pm          Project file."
	   ; print "  file.tst         Test file."
	   ; OS.Process.failure)
	 | SOME (inputs, cs, out) =>
	  let val ts = map (main1 kitdir inputs) cs 
	      fun withFile NONE f = f TextIO.stdOut
		| withFile (SOME s) f =
		let val os = TextIO.openOut s
		in (  f os
		    ; TextIO.closeOut os
		    ; print ("Wrote file ``" ^ s ^ "''\n"))
		  handle ? => (TextIO.closeOut os; raise ?)
		end
	  in 
	      withFile out (fn os => TextIO.output(os, BenchPage(concat ts)))
	    ; OS.Process.success
	  end

    fun install() =
      let val _ = print "\n ** Installing KitBench, a tool for benchmarking SML compilers **\n\n"
	  val srcPath = OS.FileSys.getDir()   (* assumes we are in kit/src/Tools/KitBench directory *)
	  val kitdir = OS.Path.mkCanonical (OS.Path.concat(srcPath, "../../.."))
	  val binPath = OS.Path.concat(kitdir, "bin")
	  val kitbenchPath = OS.Path.joinDirFile{dir=binPath, file="kitbench"}

	  val kitbenchPathImage = OS.Path.joinDirFile{dir=binPath, 
						      file="kitbench.x86-linux"}
	  val os = TextIO.openOut kitbenchPath
	  val _ = (TextIO.output(os, "sml @SMLload=" ^ kitbenchPathImage ^ " $*"); 
		   TextIO.closeOut os)
	  val _ = OS.Process.system("chmod a+x " ^ kitbenchPath)
	    handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ 
			       kitbenchPath ^ "' failed***\n");
			 OS.Process.failure)
      in SMLofNJ.exportFn(kitbenchPath,main kitdir)
      end

    val _ = install()
  end
