
structure Benchmark = 
  struct
    type report = MemUsage.report
    fun process compile p : string * (string * string * report) option =
      case compile p
	of SOME (s,t) =>
	  let 
	    val out = t ^ ".out.txt"
	    val png = t ^ ".png"
	    val res = SOME (out, png, MemUsage.memUsage {cmd=t,args=nil,out_file=out})
	      handle _ => NONE
	    val memusage = "memusage -t -T --title=" ^ t ^ " -p " ^ png ^ " " ^ t
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
      
    fun BenchLine (p, SOME (outfile, png, {count,rss,size,data,stk,exe,
					   real,user,sys})) = 
      TR(TD(tagAttr "A" ("HREF=" ^ p) p) ^ ppMem rss ^ ppMem size ^ ppMem data ^ ppMem stk ^ ppMem exe 
	 ^ ppTime real ^ ppTime user ^ ppTime sys ^ TD(tagAttr "A" ("HREF=" ^ outfile) "output")
	 ^ TD(tagAttr "A" ("HREF=" ^ png) "graph"))
      | BenchLine (p, NONE) = TR(TD(tagAttr "A" ("HREF=" ^ p) p) ^ tagAttr "TD" "COLSPAN=10" " - ")

    val BenchHeader =
      TR(TH "Program" ^ TH "VmRSS" ^ TH "VmSize" ^ TH "VmData" ^ TH "VmStk" ^ TH "VmExe"
	 ^ TH "Real" ^ TH "User" ^ TH "Sys" ^ TH "Output" ^ TH "Graph")

    fun BenchTable (h,l) =
      H2 h ^ TABLE (concat (BenchHeader :: map BenchLine l)) ^ "<P>"
      
    fun BenchPage p =
      HTML(BODY(H1 "KitBench Report" ^ p ^ "<HR>" ^ I "The KitBench Tool"))

    datatype compiler = MLKIT | MLKITGC | SMLNJ
    fun getCompileArg nil = NONE
      | getCompileArg (s::ss) = 
      case s
	of "MLKIT"   => SOME(MLKIT,ss)
	 | "MLKITGC" => SOME(MLKITGC,ss)
	 | "SMLNJ"   => SOME(SMLNJ,ss)
	 | _         => NONE
    fun getNameComp c =
      case  c 
	of MLKIT =>   ("ML Kit", CompileMLKIT.compile)
	 | MLKITGC => ("ML Kit GC", CompileMLKITGC.compile)
	 | SMLNJ =>   ("SML/NJ", CompileSMLNJ.compile)

    fun process_args (l,comps) =
      case getCompileArg l
	of SOME (comp,l) => process_args(l,comp::comps)
	 | NONE => (case l
		      of [testfile,out] => SOME(rev comps,testfile,out)
		       | _ => NONE)

    fun main1 testfile c =
      let val (h,compile) = getNameComp c
      in
	case TestFile.parse testfile
	  of NONE => raise Fail "Parse Error"
	   | SOME (_,l) => 
	    let val ps = map (fn TestFile.SML(s,_) => s
	                       | TestFile.PM (s,_) => s) l
		val l = map (process compile) ps      
	    in BenchTable(h,l)
	    end
      end

    fun main (progname, args) =
      case process_args (args,nil)
	of NONE => 
	  (  print "Usage: kitbench (MLKIT|MLKITGC|SMLNJ)* testfile outhtml\n"
	   ; OS.Process.failure)
	 | SOME (cs, testfile, out) =>
	  let 
	    val ts = map (main1 testfile) cs 
	    val os = TextIO.openOut out
	  in  TextIO.output(os, BenchPage(concat ts))
	    ; TextIO.closeOut os
	    ; print ("Wrote file ``" ^ out ^ "''\n")
	    ; OS.Process.success
	  end

    fun install() =
      let val _ = print "\n ** Installing KitBench, a tool for benchmarking SML compilers **\n\n"
	  val srcPath = OS.FileSys.getDir()   (* assumes we are in kit/src/Tools/KitBench directory *)
	  val binPath = OS.Path.mkCanonical (OS.Path.concat(srcPath, "../../../bin"))
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
      in SMLofNJ.exportFn(kitbenchPath,main)
      end

    val _ = install()
  end
