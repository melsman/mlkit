
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
    type report = {mu_report:MemUsage.report,gc:Time.time,gcnum:int}
    fun add_report (x:report,y:report) : report =
        {mu_report=MemUsage.add_report(#mu_report x,#mu_report y),
         gc=Time.+(#gc x,#gc y),
         gcnum= #gcnum x + #gcnum y}
    fun div_report (x:report,n) : report =
        {mu_report=MemUsage.div_report(#mu_report x,n),
         gc=MemUsage.div_time(#gc x,n),
         gcnum= #gcnum x div n}
    val zero_report = {mu_report=MemUsage.zero_report,
                       gc=Time.zeroTime,
                       gcnum=0}

    (* Added 2002-06-16, nh *)
    fun files_equal (s1,s2) =
      let fun open_file s = TextIO.openIn s
	  val is1 = open_file s1
	  val is2 = open_file s2
	  fun close() = (TextIO.closeIn is1; TextIO.closeIn is2)
      in (TextIO.inputAll(is1) = TextIO.inputAll(is2) before (close()))
      end handle _ => false

    fun repeat {n,out_file,execute,cmd,args} =
        let val L = List.tabulate (n,fn i => i+1)
            val () = print ("Executing: " ^ cmd ^ " - ")
            val R = List.map (fn i =>
                                 (print (Int.toString i ^ ":");
                                  execute {cmd=cmd,
                                           args=args,
                                           out_file=out_file i})) L
            val () = print "\n"
            val total = List.foldl add_report zero_report R
            val avg = div_report (total, n)
        in avg
        end

    fun readFile f =
        let val is = TextIO.openIn f
        in let val s = TextIO.inputAll is
               val () = TextIO.closeIn is
           in s
           end handle X => (TextIO.closeIn is; raise X)
        end

    fun read_stderr f =
        let val s = readFile f
        in if String.isPrefix "[GC(" s then
             let val s = String.extract(s,4,NONE)
                 val gc = case Real.fromString s of
                              SOME r => Time.fromMicroseconds (Int.toLarge(Real.floor (r * 1000.0)))
                            | NONE => Time.zeroTime
                 val gcnum = case String.tokens (fn c => c = #":") s of
                                 _ :: s :: _ => (case Int.fromString s of
                                                     SOME d => d
                                                   | NONE => 0)
                               | _ => 0
             in (gc,gcnum)
             end
           else (Time.zeroTime,0)
        end

    fun execute {cmd,args,out_file} : report =
        let val eout_file = out_file ^ ".stderr"
            val my_report = MemUsage.memUsage {cmd=cmd,args=args,out_file=out_file, eout_file=SOME eout_file}
            val (gc,gcnum) = read_stderr eout_file
        in {mu_report=my_report,
            gc=gc,
            gcnum=gcnum}
        end

    fun process compile (memusage:bool) (p:string,opts:string list) : string * (string * string * report) option =
      case compile p opts
	of SOME (s,t) =>
	  let
	    val out = t ^ ".out.1.txt"
            val png = t ^ ".png"
	    val res = SOME (out, png, repeat {n=10,
                                              out_file=fn i => t ^ ".out." ^ Int.toString i ^ ".txt",
                                              execute=execute,
                                              cmd="./" ^ t,
                                              args=["-report_gc"]})
	      handle _ => NONE
            (* Added 2002-06-16, nh *)
            (*val res = if files_equal(p^".out.ok",out) then res else NONE  Many files miss ok-files *)
	  in
	    if memusage then
	      let val cmd = "memusage -t -T --title=" ^ t ^ " -p " ^ png ^ " ./" ^ t
	      in if OS.Process.isSuccess(OS.Process.system cmd) then (s, res)
		 else (s, NONE)
	      end
	    else (s, res)
	  end
	 | NONE => (p,NONE)

    fun tag t s = "<" ^ t ^ ">" ^ s ^ "</" ^ t ^ ">"
    fun tagAttr t a s = "<" ^ t ^ " " ^ a ^ ">" ^ s ^ "</" ^ t ^ ">"
    val TD = tag "td"
    val TH = tag "th"
    val TR = tag "tr"
    val THEAD = tag "thead"
    val TBODY = tag "tbody"
    val B = tag "b"
    val I = tag "i"
    val H1 = tag "h1"
    val H2 = tag "h2"
    val TABLE = tagAttr "table" "class='table'"
    fun BODY s = tag "body" (tagAttr "div" "class='container-fluid'" s)
    val HEAD = tag "head"
    fun CSSLINK s = tagAttr "link" ("href='" ^ s ^ "' rel='stylesheet'") ""
    fun HTML e = "<!DOCTYPE html>\n" ^ tagAttr "html" "lang='en'" e
    fun ppTime t = TD (Time.toString t)
    fun ppMem n = TD (if n > 10000 then Int.toString(n div 1000) ^ "M"
		      else Int.toString n ^ "K")
    fun ppTimeB t = TD(B (Time.toString t) )
    fun ppMemB n = TD(B (if n > 10000 then Int.toString(n div 1000) ^ "M"
			 else Int.toString n ^ "K"))
    fun ppInt n = TD(B(Int.toString n))
    fun BenchTable opts (h,l,memusage) =
      let
	fun pp p opt v =
	  if List.exists (fn opt' => opt=opt') opts then p v
	  else ""

	fun loc p : string =
	  if OS.Path.ext p = SOME "sml" then
	    let val is = TextIO.openIn p
	      fun read (n) =
		  case TextIO.inputLine is of
		      NONE => n
		    | SOME _ => read(n+1)
	    in (Int.toString(read 0) before TextIO.closeIn is)
	      handle _ => (TextIO.closeIn is; "&nbsp;")
	    end
	  else "&nbsp;"

	fun BenchLine (p, SOME (outfile, png, {mu_report={count,rss,size,data,stk,exe,
					                  real,user,sys},
                                               gc,gcnum})) =
	  TR(TD(tagAttr "A" ("HREF=" ^ p) p)
	     ^ TD (loc p)
	     ^ pp ppMemB  "rss"  rss
	     ^ pp ppMem  "size" size
	     ^ pp ppMem  "data" data
	     ^ pp ppMem  "stk"  stk
	     ^ pp ppMem  "exe"  exe
	     ^ pp ppTime "user" user
	     ^ pp ppTime "sys"  sys
	     ^ pp ppTimeB "real" real
	     ^ pp ppTime "gc" gc
	     ^ pp ppInt "gcnum" gcnum
	     ^ TD(tagAttr "A" ("HREF=" ^ outfile) "output")
	     ^ (if memusage then TD(tagAttr "A" ("HREF=" ^ png) "graph") else ""))
	     ^ "\n"
	  | BenchLine (p, NONE) =
	  let val sz = Int.toString(length opts + (if memusage then 3 else 2))
	  in TR(TD(tagAttr "A" ("HREF=" ^ p) p) ^ TD(loc p) ^ tagAttr "TD" ("COLSPAN=" ^ sz) " - ") ^ "\n"
	  end
	val BenchHeader =
            THEAD(
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
	     ^ pp TH "gc"   "GC"
	     ^ pp TH "gcnum" "GC#"
	     ^ TH "Output" ^ (if memusage then TH "Graph" else ""))) ^ "\n"
      in
	H2 h ^ "\n" ^ TABLE (BenchHeader ^ TBODY(concat (map BenchLine l))) ^ "\n"
      end

    fun BenchPage p =
	let val dat = Date.toString(Date.fromTimeLocal (Time.now()))
	in HTML(HEAD(CSSLINK "benchstyle.css" ^
                     CSSLINK "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css") ^
                BODY(H1 ("KitBench Report - " ^ dat) ^ p ^ "<HR>" ^ I "The KitBench Tool"))
	end

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
	   of MLKIT flags => {head=head, compile=CompileMLKIT.compile, memusage=(*true*)false, flags=flags}
	    | SMLNJ       => {head=head, compile=CompileSMLNJ.compile, memusage=false, flags=""}
	    | SMLNJ_110_40=> raise Fail "Not implemented" (* {head=head, compile=CompileSMLNJ_110_40.compile, memusage=false, flags=""} *)
	    | MLTON flags => {head=head, compile=CompileMLTON.compile, memusage=false, flags=flags}
      end

    fun sourceFiles nil = nil
      | sourceFiles (input::inputs) =
      case OS.Path.ext input
	of NONE => raise Fail ("Missing extension on file " ^ input)
	 | SOME "sml" => (input,nil) :: sourceFiles inputs
	 | SOME "mlb" => (input,nil) :: sourceFiles inputs
	 | SOME "tst" => (case TestFile.parse input
			    of NONE => raise Fail "Parse Error"
			     | SOME (_,l) =>
			      let val ps = map (fn TestFile.SML p => p
			                         | TestFile.MLB p => p) l
			      in ps @ sourceFiles inputs
			      end)
	 | SOME ext => raise Fail ("Unknown extension " ^ ext)

    fun add_result_map (c:compiler) (p,(_,NONE)) = ()
      | add_result_map (c:compiler) (p,(_,SOME (out,png,{mu_report,gc,gcnum}:report))) =
        let val {count,rss,size,data,stk,exe,sys,user,real} = mu_report
            fun add e r = RM.add{compiler=c,program=p,entry=e,result=r}
        in  add "rss" (RM.KB rss)
	  ; add "size" (RM.KB size)
	  ; add "data" (RM.KB data)
	  ; add "stk" (RM.KB stk)
	  ; add "exe" (RM.KB exe)
	  ; add "sys" (RM.TM sys)
	  ; add "user" (RM.TM user)
	  ; add "real" (RM.TM real)
	  ; add "gc" (RM.TM gc)
	  ; add "gcnum" (RM.STR (Int.toString gcnum))
        end

    fun main1 kitdir ps c =
      let val {head,compile,memusage=memusage_p,flags} = getNameComp c
	  val _ = print ("Parsing test-files\n")
	  val orig_l = map (fn p => (#1 p,process (compile kitdir flags) memusage_p p)) ps
	  val l = map #2 orig_l
	  val _ = app (add_result_map c) orig_l
      in BenchTable ["loc", "rss", "size", "data", "stk", "exe", "real", "user", "sys", "gc", "gcnum"]
                    (head,l,memusage_p)
      end

    local
	  val pr_result =
	   fn RM.TM t => Time.toString t
   	    | RM.KB i => if i > 10000 then Int.toString (i div 1000) ^ "M"
			 else Int.toString i ^ "K"
	    | RM.STR s => s
	  fun pr_impr r1 r2 = Real.fmt (StringCvt.FIX(SOME 3)) (r2 / r1)
          val pr_improvement =
           fn (RM.TM t0,RM.TM t) => pr_impr (Time.toReal t0) (Time.toReal t)
            | (RM.KB i0,RM.KB i) => pr_impr (real i0) (real i)
            | _ => "-"
	  fun pr_entry pr_pair cs entry p c =
	      case RM.lookup {compiler=c,program=p,entry=entry} of
                  SOME r =>
                  let val c0 = case cs of nil => raise Fail "compare_section_entry.impossible"
                                        | c0 :: _ => c0
                      val impr =
                          case RM.lookup {compiler=c0,program=p,entry=entry} of
                              SOME r0 => pr_improvement (r0,r)
                            | NONE => "-"
                  in pr_pair(pr_result r,impr)
                  end
	        | NONE => pr_pair ("-","-")
    in
      fun compare_section_entry (cs:compiler list) (programs:string list) (name:string,entry:string) =
          let val table_h = TR (TH ("Program \\ " ^ entry) ^ concat (map (fn c => TH(pr_compiler c) ^
                                                                                  TH "Impr") cs))
              fun pr_pair (r,i) = TD r ^ TD i
	      fun line p = TR (TD (tagAttr "A" ("HREF=" ^ p) p) ^
                               concat (map (pr_entry pr_pair cs entry p) cs))
	      val lines = concat(map line programs)
          in H2 ("Comparison of " ^ name) ^ TABLE (table_h ^ lines)
          end
      fun compare_section_entry_simple (cs:compiler list) (programs:string list) (name:string,entry:string) =
          let val table_h = "metric a1 a2 a3\n"
              fun pr_pair (r,i) = " " ^ i ^ " "
	      fun line p = OS.Path.base p ^ " " ^ concat (map (pr_entry pr_pair cs entry p) cs) ^ "\n"
	      val lines = concat(map line programs)
          in H2 ("Simple Comparison of " ^ name) ^ tag "pre" (table_h ^ lines)
          end

      fun compare_section_entry_both cs programs p =
          compare_section_entry cs programs p ^ compare_section_entry_simple cs programs p
    end

    fun compare_section (cs:compiler list) (ps:(string*TestFile.opt list)list) : string =
      let val programs = map #1 ps
	  val entries = [("Memory Usage","rss"), ("Execution Time","real")]
	  val sections = map (compare_section_entry_both cs programs) entries
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
	   ; print "  file.mlb         ML Basis file.\n"
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
(*
    fun arch_os() =
      case SMLofNJ.SysInfo.getHostArch() ^ "-" ^ SMLofNJ.SysInfo.getOSName()
	of "X86-Linux" => "x86-linux"
         | "HPPA-HPUX" => "hppa-hpux"
	 | "X86-BSD" => "x86-bsd"
	 | s => s
    fun install() =
      let val _ = print "\n ** Installing KitBench, a tool for benchmarking SML compilers **\n\n"
	  val binPath = OS.Path.concat(kitdir, "bin")
	  val kitbenchPath = OS.Path.joinDirFile{dir=binPath, file="kitbench"}

	  val kitbenchPathImage = OS.Path.joinDirFile{dir=binPath,
						      file="kitbench." ^ arch_os()}
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
*)
  end

structure Main : sig end =
struct
    val srcPath = OS.FileSys.getDir()   (* assumes we are in kit/src/Tools/KitBench directory *)
    val kitdir = OS.Path.mkCanonical (OS.Path.concat(srcPath, "../../.."))
    val res = Benchmark.main kitdir (CommandLine.name(), CommandLine.arguments())
    val _ = OS.Process.exit(res)
end
