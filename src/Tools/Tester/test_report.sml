
structure TestReport : TEST_REPORT =
  struct
    
    type runtime_line = {name: string, ok: bool, exesize: string,
			 exesize_stripped: string, size: int, rss: int, 
			 data: int, stk: int, exe: int,
			 real: Time.time, user: Time.time, sys: Time.time}

    val compout_lines : {name: string, match: bool option, success_as_expected: bool} list ref = ref nil
    val comptime_lines : {name: string, entries: (string * Time.time) list} list ref = ref nil
    val runtime_lines : runtime_line list ref = ref nil
    val runtime_bare_lines : (string * bool) list ref = ref nil
    val profile_lines : (string * bool) list ref = ref nil
    val gc_lines : (string * bool) list ref = ref nil
    val gengc_lines : (string * bool) list ref = ref nil
    val gc_prof_lines : (string * bool) list ref = ref nil
    val gengc_prof_lines : (string * bool) list ref = ref nil
    val tags_lines : (string * bool) list ref = ref nil
    val tags_prof_lines : (string * bool) list ref = ref nil
    val log_lines : string list ref = ref nil

    fun reset () = (runtime_lines := nil; runtime_bare_lines := nil; 
		    compout_lines := nil; comptime_lines := nil; 
		    profile_lines := nil; log_lines := nil;
		    gc_lines := nil; gc_prof_lines := nil;
		    gengc_lines := nil; gengc_prof_lines := nil;
		    tags_lines := nil; tags_prof_lines := nil)
      
    fun add_runtime_line l = runtime_lines := l :: !runtime_lines
    fun add_runtime_bare_line l = runtime_bare_lines := l :: !runtime_bare_lines
    fun add_profile_line l = profile_lines := l :: !profile_lines
    fun add_gc_line l = gc_lines := l :: !gc_lines
    fun add_gengc_line l = gengc_lines := l :: !gengc_lines
    fun add_gc_profile_line l = gc_prof_lines := l :: !gc_prof_lines
    fun add_gengc_profile_line l = gengc_prof_lines := l :: !gengc_prof_lines
    fun add_tags_line l = tags_lines := l :: !tags_lines
    fun add_tags_profile_line l = tags_prof_lines := l :: !tags_prof_lines
    fun add_comptime_line l = comptime_lines := l :: !comptime_lines
    fun add_compout_line l = compout_lines := l :: !compout_lines
    fun add_log_line l = log_lines := l :: !log_lines

    fun read_all s =
      let val is = TextIO.openIn s
      in let val res = TextIO.inputAll is
	 in TextIO.closeIn is; res
	 end handle _ => (TextIO.closeIn is; "could not read file `" ^ s ^ "'\n")
      end handle _ => ("could not open or close file `" ^ s ^ "'\n")

    fun kitversion kitexe =
      if OS.Process.isSuccess(OS.Process.system (kitexe ^ " --version > KITversion")) then
	read_all "KITversion"
      else "failure while executing `" ^ kitexe ^ " --version'\n"

    (* Get entry for a UNIX environment variable *)
    fun machine() = case OS.Process.getEnv "HOSTNAME"
                    of SOME x => x
                     | NONE => "localhost"

    type comptime_table_line = {name:string, total:Time.time, entries: (string * Time.time * real) list}
    type comptime_table = comptime_table_line list

    fun process_comptime_line({name: string, entries: (string * Time.time) list}) : comptime_table_line =
      let val total = foldl Time.+ Time.zeroTime (map #2 entries)
	  fun pct t = 100.0 * (Time.toReal t) / (Time.toReal total)
      in {name=name, total=total, entries = map (fn (s,t) => (s,t,pct t)) entries}
      end

    (*invariant: compilation of all programs results in the same entries, in the same order *)
    fun split_comptime_table(table: comptime_table) : comptime_table list =
      let fun split n [] acc = (rev acc, [])
	    | split n (l as e::es) acc = if n <= 0 then (rev acc, l)
					 else split (n-1) es (e::acc)
	  val oksize=6
	  fun split_table [] = ([],[])
	    | split_table ({name,total,entries}::rest) =
	    let val (table_lines, table_lines') = split_table rest 
	        val (entries, entries') = split oksize entries []
	    in ({name=name,total=total,entries=entries}::table_lines,
		{name=name,total=total,entries=entries'}::table_lines')
	    end

	  fun size_table ([]:comptime_table) = ~1
	    | size_table (e::_) = List.length (#entries e)
	    
	  fun loop (t, acc) = if size_table t > oksize then 
	                        let val (t,rest) = split_table t
				in loop(rest, t::acc)
				end
			      else rev(t::acc)
      in loop(table, [])
      end

    fun split_lines xx = 
      let val l = length xx
	  val ld3 = l div 3
      in if l < 6 then (xx,[],[])
	 else let val xx1 = List.take(xx,ld3)
		  val rest = List.drop(xx, ld3)
		  val xx2 = List.take(rest,ld3)
		  val xx3 = List.drop(rest,ld3)
	      in (xx1,xx2,xx3)
	      end
      end 

    fun pr_ok true = "ok"
      | pr_ok false = "err"

    fun tag t s = "<" ^ t ^ ">" ^ s ^ "</" ^ t ^ ">"
    fun verb n = tag "tt" n

    fun export {errors:int, testfile_string: string, kitexe: string} : unit =
      let val htmlfile = "test_report.html"
	  val os = TextIO.openOut htmlfile
	  fun out s = TextIO.output(os,s)
	  fun outln s = out(s^"\n")

          fun outpar s = tag "p" s
          fun begintable () = outln("<table border=1>")
          fun endtable () = outln("</table>")
	  fun begintag t = outln("<" ^ t ^ ">")
	  fun endtag t = outln("</" ^ t ^ ">")
	  fun section s = outln(tag "h2" s)
	  fun subsection s = outln(tag "h3" s)

          fun tdc s = "<td align='center'>" ^ s ^ "</td>"
          fun thl s = "<th align='left'>" ^ s ^ "</th>"

	  fun outresult 0 = outpar "No errors were found during the test."
	    | outresult 1 = outpar "I found one error; see the Test Log section for details." 
	    | outresult n = outpar ("I found " ^ Int.toString n ^ " errors; see the Test Log Section for details.")

	  fun header() =
	    (outln "<html><head></head><body>";
	     outln "<h1>ML Kit Test Report</h1>";
	     outln "<i>By The ML Kit Tester</i>")

	  fun abstract() =
	    let val kitexe' = concat (map (fn #"_" => "\\_" | a => str a) (explode kitexe))
	    in
	    (section "Abstract";
	     outpar ("This test report is generated by the ML Kit Tester, a program for finding \
	             \bugs in the ML Kit, for finding inefficiencies in the ML Kit compiler, and \
	             \for benchmarking generated executables. All tests were executed on " ^ 
                     tag "b" (machine()) ^ ".");
	     outpar ("Here is the output from executing " ^ tag "tt" (kitexe' ^ " --version") ^ ":");
	     begintag "code";
	     out (kitversion kitexe);
	     endtag "code";
	     outresult errors)
	    end

	  fun compout_section [] = ()
	    | compout_section (l : {name:string, success_as_expected:bool,match:bool option} list) =
	    let 
	      val header = tag "tr" (thl "Source" ^ tag "th" "Compare" ^ tag "th" "Success as expected") 
		
	      fun pr_ok_opt NONE = "--"
		| pr_ok_opt (SOME ok) = pr_ok ok
	      fun line {name,match,success_as_expected} = 
		  tag "tr" (tag "td" (verb name) ^ tdc (pr_ok_opt match) ^ tdc (pr_ok success_as_expected))

	    in section "Comparison of Compiler Messages";
	       outpar "This section compares compiler messages (e.g., elaboration results) with \
	              \expected compiler messages (column ``Compare''). The column ``Success as expected'' shows if \
	              \compilation succeeded or failed as expected.";
	       outln "";
	       begintable();
	       outln header;
	       app (outln o line) l;
	       endtable()
	    end

	  fun entriesOfL (nil:comptime_table,(_,acc)) = acc
	    | entriesOfL ({entries,...}::l,(n,acc)) =
	      let val n' = length entries
	      in entriesOfL(l,
			    if n' > n then (n',entries)
			    else (n,acc))
	      end

	  fun comptime_section [] = ()
	    | comptime_section(lines: {name: string, entries: (string * Time.time) list} list) =
	    let val table : comptime_table = map process_comptime_line lines
		val tables : comptime_table list = split_comptime_table table		  
		fun mktable [] = ()
		  | mktable (l as {entries,...}::l') =
		  let val entries = entriesOfL (l',(length entries,entries))
		      val header = foldl (fn (s,a) => a ^ tag "th" s) (thl "Source" ^ tag "th" "Total") (map #1 entries)
		    fun entry (s, t, r) = Time.fmt 2 t ^ "/" ^ Real.fmt (StringCvt.FIX (SOME 1)) r
		    fun line {name,total,entries} =
		      foldl (fn (e,a) => a ^ tdc (entry e)) (tag "td" (tag "tt" name) ^ tdc (Time.fmt 2 total)) entries
		  in 
		    begintable();
		    outln (tag "tr" header);
		    app (outln o tag "tr" o line) l;
		    endtable()
		  end
	    in
	      section "Timings of the Compiler";
	      outpar "This section shows timings for the Kit compiler. \
	             \The times measured are the times used by different phases of the Kit for compiling \
	             \the source programs. \
	             \Timings are written (<i>time</i>/<i>pct</i>), where <i>time</i> is the user \
	             \time (garbage collection time excluded) in seconds and <i>pct</i> is the percentage \
	             \of time used in the phase compared to the time used for all the measured phases (the Total column).";
	      app mktable tables
	    end

	  fun execution_section	[] = ()
	    | execution_section (l : runtime_line list) =
	    let fun stack [] = ""
		  | stack [e] = e
		  | stack (e::l) = "$\\!\\!$\\begin{tabular}{c}" ^ foldl (fn (s,a) => a ^ " \\\\" ^ s) e l ^ "\\end{tabular}$\\!\\!$"
                val headers = ["Source", "Ok", "Exec size", "Vm Size", "Vm RSS", "Vm Data", "Vm Stk", "Vm Exe", "real time", "user time", "sys time"]
                val header = String.concat (map (tag "th") headers)

		fun report i = (* i is in kilobytes *)
		  if i > 10000 then Int.toString (i div 1000) ^ "M"
		  else Int.toString i ^ "K"

		fun line {name: string, ok: bool, exesize: string,
			  exesize_stripped: string, size: int, rss: int, 
			  data:int, stk:int, exe: int,
			  real: Time.time, user: Time.time, sys: Time.time} =
		  (tag "td" (verb name) ^ tdc (pr_ok ok) ^ 
		   tdc exesize_stripped ^ tdc (report size) ^ tdc (report rss) ^ 
		   tdc (report data) ^ tdc (report stk) ^ tdc (report exe) ^
		   tdc (Time.toString real) ^ tdc (Time.toString user) ^ tdc (Time.toString sys))
	    in 
	      section "Measurements of Executables";
	      outpar "This section shows static and dynamic properties of the generated executable files. \
	             \Sizes of executables are in bytes (measured after symbols from object files are stripped). \
	             \Column <tt>Vm Size</tt> shows the total size \
	             \of the process &mdash; including text, data, and stack. Column <tt>Vm RSS</tt> \
	             \shows the resident set-size of the process, which is the total amount \
	             \of physical memory used by the task \
	             \Memory sizes and execution timings are found by reading periodically from \
	             \the <tt>proc</tt> filesystem, mounted on <tt>/proc</tt>. \
	             \The <b>Ok</b> column shows if the output from running the executable \
	             \equals the expected output.";
	      begintable();
	      outln (tag "tr" header);
	      app (outln o tag "tr" o line) l;
	      endtable()
	    end
	  
	  local
	    val header = thl "Source" ^ tag "th" "Ok"
	    fun line (name, ok) = tag "td" (verb name) ^ tdc (pr_ok ok)
	    fun table [] = ()
	      | table l = (begintable();
			   outln (tag "tr" header);
			   app (outln o tag "tr" o line) l;
			   endtable())
	  in
	    fun exe_output_section [] = ()
	      | exe_output_section (l:(string * bool) list) =
	      let 
		val (l1,l2,l3) = split_lines l
	      in 
		section "Comparison of Output from Executables";
		outpar "This section shows if the output from execution equals the expected output. \
		       \Entries for executables that are measured in Section <b>Measurements of Executables</b> \
		       \(if one such section exists) are not shown here. ";
		table l1;
		table l2;
		table l3
	      end

	    fun profile_section [] = ()
	      | profile_section (l:(string * bool) list) =
	      let 
		val (l1,l2,l3) = split_lines l
	      in 
		section "Profiling";
		outpar "This section shows tests of the compiler with profiling enabled. \
		       \See Section <b>Log File</b> for details of errors.";
		table l1;
		table l2;
		table l3
	      end

	    fun gc_section [] = ()
	      | gc_section (l:(string * bool) list) =
	      let
		val (l1,l2,l3) = split_lines l
	      in
		section "Garbage Collection";
		outpar "This section shows tests of the compile with garbage collection enabled. \
		       \See Section <p>Log File</b> for details of errors.";
		table l1;
		table l2;
		table l3
	      end

	    fun gc_prof_section [] = ()
	      | gc_prof_section (l:(string * bool) list) =
	      let
		val (l1,l2,l3) = split_lines l
	      in
		section "Garbage Collection and Profiling";
		outpar "This section shows tests of the compile with garbage collection and profiling enabled. \
		       \See Section <b>Log File</b> for details of errors.";
		table l1;
		table l2;
		table l3
	      end

	    fun gengc_section [] = ()
	      | gengc_section (l:(string * bool) list) =
	      let
		val (l1,l2,l3) = split_lines l
	      in
		section "Generational Garbage Collection";
		outpar "This section shows tests of the compile with generational garbage collection enabled. \
		       \See Section <b>Log File</b> for details of errors.";
		table l1;
		table l2;
		table l3
	      end

	    fun gengc_prof_section [] = ()
	      | gengc_prof_section (l:(string * bool) list) =
	      let
		val (l1,l2,l3) = split_lines l
	      in
		section "Generational Garbage Collection and Profiling";
		outpar "This section shows tests of the compile with generational garbage collection and profiling enabled. \
		       \See Section <b>Log File</b> for details of errors.";
		table l1;
		table l2;
		table l3
	      end

	    fun tags_section [] = ()
	      | tags_section (l:(string * bool) list) =
	      let
		val (l1,l2,l3) = split_lines l
	      in
		section "Tagging";
		outpar "This section shows tests of the compile with tagging enabled (garbage collection disabled). \
		       \See Section <b>Log File</b> for details of errors.";
		table l1;
		table l2;
		table l3
	      end

	    fun tags_prof_section [] = ()
	      | tags_prof_section (l:(string * bool) list) =
	      let
		val (l1,l2,l3) = split_lines l
	      in
		section "Tagging and Profiling";
		outpar "This section shows tests of the compile with tagging and profiling enabled (garbage collection disabled). \
		       \See Section <b>Log File</b> for details of errors.";
		table l1;
		table l2;
		table l3
	      end
	  end
 
	  fun testfile_section() =
	    (section "Test File";
	     outpar "Here is the test file for this test report:";
	     begintag "code";
	     outln (String.translate (fn #"\n" => "<br/>\n" | c => str c) testfile_string);
	     endtag "code")

	  fun logsection (lines : string list) =
	    (section "Test Log";
	     outpar "Here is the log for the test:";
	     begintag "code";
	     app (fn s => outln(s ^ "<br/>")) lines;
	     endtag "code")

      in
	header(); 
	abstract(); 
	compout_section(rev(!compout_lines));
	comptime_section(rev(!comptime_lines));
	execution_section(rev(!runtime_lines));
	exe_output_section(rev(!runtime_bare_lines));
	profile_section(rev(!profile_lines));
	tags_section(rev(!tags_lines));
	tags_prof_section(rev(!tags_prof_lines));
	gc_section(rev(!gc_lines));
	gc_prof_section(rev(!gc_prof_lines));
	gengc_section(rev(!gc_lines));
	gengc_prof_section(rev(!gc_prof_lines));
	testfile_section();
	logsection (rev(!log_lines)); 
	endtag "body"; 
        endtag "html";
	TextIO.closeOut os
      end
  end
