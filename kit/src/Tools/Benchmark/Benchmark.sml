
datatype compiler = MLKIT of string | SMLNJ | MLTON of string | SMLNJ_110_40
fun pr_compiler (c:compiler): string =
  let fun with_flags "" s = s
	| with_flags flags s = s ^ " [" ^ flags ^ "]"
  in case c
       of MLKIT flags => with_flags flags "MLKIT"
	| SMLNJ => "SMLNJ"
	| MLTON flags => with_flags flags "MLTON"
	| SMLNJ_110_40 => "SMLNJ-110.40"
  end

signature RESULT_MAP =
  sig
    datatype result = KB of int | TM of Time.time | STR of string
    val add : {compiler:compiler,
	       program:string,        (* compiler independent program name *)
	       entry:string,
	       result:result} -> unit
    val lookup : {compiler:compiler,program:string,entry:string} -> result option
  end

structure RM : RESULT_MAP =
  struct
    datatype result = KB of int | TM of Time.time | STR of string
    val map : {compiler:compiler, program:string, entry:string,result:result} list ref = ref nil
    fun add a : unit = map := a :: !map      
    fun lookup {compiler:compiler,program:string,entry:string} : result option = 
      case List.find (fn r => #compiler r = compiler 
		      andalso #program r = program 
		      andalso #entry r = entry) (!map)
	of SOME a => SOME(#result a)
	 | NONE => NONE
  end
    
structure Benchmark = 
  struct
    type report = MemUsage.report

    (* Added 2002-06-16, nh *)
    fun files_equal (s1,s2) =
      let fun open_file s = TextIO.openIn s 
	  val is1 = open_file s1
	  val is2 = open_file s2
	  fun close() = (TextIO.closeIn is1; TextIO.closeIn is2)
      in (TextIO.inputAll(is1) = TextIO.inputAll(is2) before (close()))
      end handle _ => false

    fun process compile (memusage:bool) (p:string,opts:string list) : string * (string * string * report) option =
      case compile p opts
	of SOME (s,t) =>
	  let 
	    val out = t ^ ".out.txt"
	    val png = t ^ ".png"
	    val res = SOME (out, png, MemUsage.memUsage {cmd="./" ^ t,args=nil,out_file=out})
	      handle _ => NONE
            (* Added 2002-06-16, nh *)
            (*val res = if files_equal(p^".out.ok",out) then res else NONE  Many files miss ok-files *)
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
    fun ppTimeB t = TD(B (Time.toString t) )
    fun ppMemB n = TD(B (if n > 10000 then Int.toString(n div 1000) ^ "M"
			 else Int.toString n ^ "K"))

    fun BenchTable opts (h,l,memusage) =
      let
	fun pp p opt v = 
	  if List.exists (fn opt' => opt=opt') opts then p v
	  else ""

	fun loc p : string =
	  if OS.Path.ext p = SOME "sml" then
	    let val is = TextIO.openIn p
	      fun read (n) =
		if TextIO.inputLine is = "" then n
		else read(n+1)
	    in (Int.toString(read 0) before TextIO.closeIn is)
	      handle _ => (TextIO.closeIn is; "&nbsp;")
	    end
	  else "&nbsp;"

	fun BenchLine (p, SOME (outfile, png, {count,rss,size,data,stk,exe,
					       real,user,sys})) = 
	  TR(TD(tagAttr "A" ("HREF=" ^ p) p) 
	     ^ TD (loc p)
	     ^ pp ppMemB  "rss"  rss 
	     ^ pp ppMem  "size" size 
	     ^ pp ppMem  "data" data 
	     ^ pp ppMem  "stk"  stk 
	     ^ pp ppMem  "exe"  exe 
	     ^ pp ppTimeB "user" user 
	     ^ pp ppTime "sys"  sys 
	     ^ pp ppTime "real" real 
	     ^ TD(tagAttr "A" ("HREF=" ^ outfile) "output")
	     ^ (if memusage then TD(tagAttr "A" ("HREF=" ^ png) "graph") else ""))
	     ^ "\n"
	  | BenchLine (p, NONE) = 
	  let val sz = Int.toString(length opts + (if memusage then 3 else 2))
	  in TR(TD(tagAttr "A" ("HREF=" ^ p) p) ^ TD(loc p) ^ tagAttr "TD" ("COLSPAN=" ^ sz) " - ") ^ "\n"
	  end
	val BenchHeader =
	  TR(TH "Program"
	     ^ TH "Lines"
	     ^ pp TH "rss"  "RSS" 
	     ^ pp TH "size" "Size" 
	     ^ pp TH "data" "Data" 
	     ^ pp TH "stk"  "Stk" 
	     ^ pp TH "exe"  "Exe"
	     ^ pp TH "user" "User" 
	     ^ pp TH "sys"  "Sys" 
	     ^ pp TH "real" "Real" 
	     ^ TH "Output" ^ (if memusage then TH "Graph" else "")) ^ "\n"
      in
	H2 h ^ "\n" ^ TABLE (concat (BenchHeader :: map BenchLine l)) ^ "<P>\n"
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

    fun getCompileArgs (nil, comps, out) = NONE
      | getCompileArgs (s::ss , comps, out) = 
      case s
	of "-mlkit"   => 
	  (case readFlags ss
	     of SOME (flags,ss) => getCompileArgs (ss, MLKIT flags::comps, out)
	      | NONE => getCompileArgs (ss, MLKIT "" ::comps, out))
	 | "-smlnj"   => getCompileArgs (ss, SMLNJ::comps, out)
         | "-smlnj-110.40"   => getCompileArgs (ss, SMLNJ_110_40::comps, out)
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
      let val head = pr_compiler c
      in case  c 
	   of MLKIT flags => {head=head, compile=CompileMLKIT.compile, memusage=true, flags=flags}
	    | SMLNJ       => {head=head, compile=CompileSMLNJ.compile, memusage=false, flags=""}
	    | SMLNJ_110_40=> {head=head, compile=CompileSMLNJ_110_40.compile, memusage=false, flags=""}
	    | MLTON flags => {head=head, compile=CompileMLTON.compile, memusage=false, flags=flags}
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

    fun add_result_map (c:compiler) (p,(_,NONE)) = ()
      | add_result_map (c:compiler) (p,(_,SOME (out,png,{count,rss,size,data,stk,exe,sys,user,real}))) =
      let fun add e r = RM.add{compiler=c,program=p,entry=e,result=r}
      in  add "rss" (RM.KB rss)
	; add "size" (RM.KB size)
	; add "data" (RM.KB data)
	; add "stk" (RM.KB stk)
	; add "exe" (RM.KB exe)
	; add "sys" (RM.TM sys)
	; add "user" (RM.TM user)
	; add "real" (RM.TM real)
      end

    fun main1 kitdir ps c =
      let val {head,compile,memusage,flags} = getNameComp c
	  val _ = print ("Parsing test-files\n")
	  val orig_l = map (fn p => (#1 p,process (compile kitdir flags) memusage p)) ps
	  val l = map #2 orig_l	    
	  val _ = app (add_result_map c) orig_l
      in BenchTable ["loc", "rss", "size", "data", "stk", "exe", "real", "user", "sys"] (head,l,memusage)
      end

    fun compare_section_entry (cs:compiler list) (programs:string list) (name:string,entry:string) =
      let val sz = length cs
	  val improvement_h = if sz = 2 then TH "Improvement (percent)" else ""
	  val table_h = TR (TH ("Program \\ " ^ entry) ^ concat (map (fn c => TH(pr_compiler c)) cs) ^
			    improvement_h)
	  fun pr_result r =
	    case r
	      of RM.TM t => Time.toString t
	       | RM.KB i => if i > 10000 then Int.toString (i div 1000) ^ "M"
			 else Int.toString i ^ "K"
	       | RM.STR s => s
	  fun pr_entry p c = 
	    case RM.lookup {compiler=c,program=p,entry=entry}
	      of SOME r => TD(pr_result r)
	       | NONE => TD "-"
	  fun percent_imp r1 r2 = TD(Real.fmt (StringCvt.FIX(SOME 1)) ((r1 - r2) * 100.0 / r1))
	  fun improvement p [c1,c2] =
	    ((case (RM.lookup {compiler=c1,program=p,entry=entry},
		    RM.lookup {compiler=c2,program=p,entry=entry})
		of (SOME (RM.KB kb1),SOME (RM.KB kb2)) => percent_imp (real kb1) (real kb2)
		 | (SOME (RM.TM tm1),SOME (RM.TM tm2)) => percent_imp (Time.toReal tm1) (Time.toReal tm2)
		 | _ => TD "-") handle _ => TD "_")
	    | improvement _ _ = ""
	    
	  fun line p = TR (TD (tagAttr "A" ("HREF=" ^ p) p) ^ concat (map (pr_entry p) cs) ^ improvement p cs)
	  val lines = concat(map line programs)
      in H2 ("Comparison of " ^ name) ^ TABLE (table_h ^ lines)
      end

    fun compare_section (cs:compiler list) (ps:(string*TestFile.opt list)list) : string =
      let val programs = map #1 ps
	  val entries = [("Memory Usage","rss"), ("Execution Time","user")]
	  val sections = map (compare_section_entry cs programs) entries
      in concat sections
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
	   ; print "  -smlnj-110.40          Run SML/NJ on each test (version 110.40).\n"
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
	  let val ps = sourceFiles inputs
	      val ts = map (main1 kitdir ps) cs 
	      val compare_sec = compare_section cs ps
	      fun withFile NONE f = f TextIO.stdOut
		| withFile (SOME s) f =
		let val os = TextIO.openOut s
		in (  f os
		    ; TextIO.closeOut os
		    ; print ("Wrote file ``" ^ s ^ "''\n"))
		  handle ? => (TextIO.closeOut os; raise ?)
		end
	  in 
	      withFile out (fn os => TextIO.output(os, BenchPage(concat ts ^ compare_sec)))
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
