
structure Benchmark = 
  struct
    val progs = ["kitqsort.sml", "kkb36c.sml", "kittmergesort.sml"]
      
    type report = MemUsage.report
    fun process compile p : string * report option =
      case compile p
	of SOME t =>
	  let 
	    val out = t ^ ".bout"
	    val res = SOME (MemUsage.memUsage {cmd=t,args=nil,out_file=out})
	      handle _ => NONE
	  in (p, res)
	  end
	 | NONE => (p,NONE)

    fun tag t s = "<" ^ t ^ ">" ^ s ^ "</" ^ t ^ ">"
    fun tagAttr t a s = "<" ^ t ^ " " ^ a ^ ">" ^ s ^ "</" ^ t ^ ">"
    val TD = tag "TD"
    val TH = tag "TH"
    val TR = tag "TR"
    val B = tag "B"
    val H2 = tag "H2"
    val TABLE = tagAttr "TABLE" "border=1"
    val BODY = tagAttr "BODY" "bgcolor=white"
    val HTML = tag "HTML"
    fun ppTime t = TD (Time.toString t) 
    fun ppMem n = TD (if n > 10000 then Int.toString(n div 1000) ^ "M"
		      else Int.toString n ^ "K")
      
    fun BenchLine (p, SOME {count,rss,size,data,stk,exe,
			    real,user,sys}) = 
      TR(TD(B p) ^ ppMem rss ^ ppMem size ^ ppMem data ^ ppMem stk ^ ppMem exe 
	 ^ ppTime real ^ ppTime user ^ ppTime sys)
      | BenchLine (p, NONE) = TR(TD(B p) ^ tagAttr "TD" "colspan=8" " - ")

    val BenchHeader =
      TR(TH "Program" ^ TH "VmRSS" ^ TH "VmSize" ^ TH "VmData" ^ TH "VmStk" ^ TH "VmExe"
	 ^ TH "Real" ^ TH "User" ^ TH "Sys")

    fun BenchTable l =
      TABLE (concat (BenchHeader :: map BenchLine l))
      
    fun BenchPage l =
      HTML(BODY(H2 "BenchMark" ^ BenchTable l))

    datatype compiler = MLKIT | SMLNJ

    fun process_args ["MLKIT",testfile,out] = SOME (MLKIT,testfile,out)
      | process_args ["SMLNJ",testfile,out] = SOME (SMLNJ,testfile,out)
      | process_args _ = NONE

    fun main (progname, args) =
      case process_args args
	of NONE => (print "Usage: kitbench [MLKIT|SMLNJ] testfile outhtml\n"
		    ; OS.Process.failure)
	 | SOME (c, testfile, out) =>
	  let val compile =
	        case c
		  of MLKIT => CompileMLKIT.compile
		   | SMLNJ => CompileSMLNJ.compile
	  in case TestFile.parse testfile
	       of NONE => raise Fail "Parse Error"
		| SOME (_,l) => 
		 let val ps = map (fn TestFile.SML(s,_) => s
	                            | TestFile.PM _ => raise Fail "PM files not supported") l
		     val l = map (process compile) ps
		     val os = TextIO.openOut out
		 in  TextIO.output(os, BenchPage l)
		   ; TextIO.closeOut os
		   ; print ("Wrote file ``" ^ out ^ "''\n")
		   ; OS.Process.success
		 end
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
