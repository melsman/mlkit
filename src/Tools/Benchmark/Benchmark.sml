
structure Benchmark = 
  struct
    type report = MemUsage.report
    fun process compile (memusage:bool) (p:string,opts:string list) : string * (string * string * report) option =
      case compile p opts
	of SOME (s,t) =>
	  let 
	    val out = t ^ ".out.txt"
	    val png = t ^ ".png"
	    val res = SOME (out, png, MemUsage.memUsage {cmd="./" ^ t,args=nil,out_file=out})
	      handle _ => NONE
	  in 
	    if memusage then
	      let val cmd = "memusage -t -T --title=" ^ t ^ " -p " ^ png ^ " ./" ^ t
	      in if OS.Process.system cmd = OS.Process.success then (s, res)
		 else (s, NONE)
	      end
	    else (s, res)
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
      
    fun BenchTable opts (h,l,memusage) =
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
	     ^ (if memusage then TD(tagAttr "A" ("HREF=" ^ png) "graph") else ""))
	  | BenchLine (p, NONE) = 
	  let val sz = Int.toString(length opts + (if memusage then 3 else 2))
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
	     ^ TH "Output" ^ (if memusage then TH "Graph" else ""))
      in
	H2 h ^ TABLE (concat (BenchHeader :: map BenchLine l)) ^ "<P>"
      end

    fun BenchPage p =
      HTML(BODY(H1 "KitBench Report" ^ p ^ "<HR>" ^ I "The KitBench Tool"))

    fun readFlags ss =
      let
	fun concatWith d (nil) = ""
	  | concatWith d [s] = s
	  | concatWith d (s::ss) = s ^ d ^ concatWith d ss
	fun readFls (ss,acc) =
	  case ss
	    of ":"::ss => SOME (concatWith " " (rev acc),ss)
	     | s::ss => readFls(ss,s::acc)
	     | _ => NONE
      in
	case ss
	  of ":"::ss => readFls (ss,nil)
	   | _ => NONE
      end
    datatype compiler = MLKIT of string | SMLNJ | MLTON of string

    fun getCompileArgs (nil, comps, out) = NONE
      | getCompileArgs (s::ss , comps, out) = 
      case s
	of "-mlkit"   => 
	  (case readFlags ss
	     of SOME (flags,ss) => getCompileArgs (ss, MLKIT flags::comps, out)
	      | NONE => getCompileArgs (ss, MLKIT "" ::comps, out))
	 | "-smlnj"   => getCompileArgs (ss, SMLNJ::comps, out)
	 | "-mlton"   => 
	     (case readFlags ss
		of SOME (flags,ss) => getCompileArgs (ss, MLTON flags::comps, out)
		 | NONE => getCompileArgs (ss, MLTON "" :: comps, out))
	 | "-o" => 
	  (case ss
	     of (f::ss) => getCompileArgs (ss, comps, SOME f)
	      | _ => NONE)
	 | _ => SOME (s::ss, rev comps, out)

    fun getNameComp c =
      let fun with_flags "" s = s
	    | with_flags flags s = s ^ " [" ^ flags ^ "]"
      in
	case  c 
	  of MLKIT flags =>   {head=with_flags flags "ML Kit", 
			       compile=CompileMLKIT.compile, memusage=true, flags=flags}
	   | SMLNJ       =>   {head="SML/NJ", compile=CompileSMLNJ.compile, memusage=false, flags=""}
	   | MLTON flags =>   {head=with_flags flags "MLTON", 
			       compile=CompileMLTON.compile, memusage=false, flags=flags}
      end

    fun sourceFiles nil = nil
      | sourceFiles (input::inputs) =
      case OS.Path.ext input
	of NONE => raise Fail ("Missing extension on file " ^ input)
	 | SOME "sml" => (input,nil) :: sourceFiles inputs
	 | SOME "pm" => (input,nil) :: sourceFiles inputs 
	 | SOME "tst" => (case TestFile.parse input
			    of NONE => raise Fail "Parse Error"
			     | SOME (_,l) => 
			      let val ps = map (fn TestFile.SML p => p
			                         | TestFile.PM p => p) l
			      in ps @ sourceFiles inputs
			      end)
	 | SOME ext => raise Fail ("Unknown extension " ^ ext)

    fun main1 kitdir inputs c =
      let val {head,compile,memusage,flags} = getNameComp c
	  val _ = print ("Parsing test-files\n")
	  val ps = sourceFiles inputs
	  val l = map (process (compile kitdir flags) memusage) ps
      in BenchTable ["rss", "size", "data", "stk", "exe", "real", "user", "sys"] (head,l,memusage)
      end

    fun tokenize nil = nil
      | tokenize (s::ss) = 
      let  (* `:' should appear as separate items *)
	fun token #":" = true
	  | token _ = false
	fun tok (nil, nil, toks) = rev toks
	  | tok (nil, acc, toks) = rev (implode(rev acc)::toks)
	  | tok (c::cs, acc, toks) =
	  if token c then tok(cs,nil,str c :: implode(rev acc) :: toks)
	  else tok(cs,c::acc,toks)	  
      in tok (explode s, nil, nil) @ tokenize ss
      end

    fun main kitdir (progname, args) =
      let 
	  val _ = print "Args: ["
	  val _ = app (fn s => print (s ^ ",")) args
	  val _ = print "]\n"
	  val args = tokenize args
	  val _ = print "Args: ["
	  val _ = app (fn s => print (s ^ ",")) args
	  val _ = print "]\n"
      in
      case getCompileArgs (args, nil, NONE)
	of NONE => 
	  (  print "USAGE: kitbench [OPTION]... FILE...\n"
	   ; print "OPTIONS:\n"
	   ; print "  -smlnj                 Run SML/NJ on each test.\n"
	   ; print "  -mlton[:FLAG...FLAG:]  Run MLTON on each test.\n"
	   ; print "  -mlkit[:FLAG...FLAG:]  Run MLKIT on each test.\n"
	   ; print "  -o file                Write output to file ``file''.\n"
           ; print "FLAG options to -mlton and -mlkit are passed to\n"
	   ; print "  the compiler.\n"
	   ; print "FILES:\n"
	   ; print "  file.sml         Standard ML source file.\n"
	   ; print "  file.pm          Project file.\n"
	   ; print "  file.tst         Test file.\n"
	   ; print "\n"
	   ; print "EXAMPLE:\n"
	   ; print "  kitbench -mlkit:-dangle -scratch: -smlnj kkb36c.sml.\n"
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
