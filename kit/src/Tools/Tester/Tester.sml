
signature TESTER = 
  sig 
    val main : string * string list -> OS.Process.status
  end

structure Tester : TESTER =
  struct
    val _ = SMLofNJ.Internals.GC.messages false;
  
    fun files_equal (s1,s2) =
      let fun open_file s = TextIO.openIn s 
	  val is1 = open_file s1
	  val is2 = open_file s2
	  fun close() = (TextIO.closeIn is1; TextIO.closeIn is2)
      in (TextIO.inputAll(is1) = TextIO.inputAll(is2) before (close()))
      end handle _ => false

    fun equal_to_okfile s = files_equal(s,s^".ok")

    local
      val error_counter = ref 0
    in
      fun reset_error_counter() = error_counter:=0
      val msglog = ref TextIO.stdOut
      fun msg s = (TextIO.output(!msglog,s ^ "\n"); TextIO.flushOut (!msglog); print (s ^ "\n");
		   TestReport.add_log_line s)
      fun msg' s = (TextIO.output(!msglog,s ^ "\n"); TextIO.flushOut (!msglog); print (s ^ "\n"))
      fun msgOk s = msg (" ok: " ^ s)
      fun msgErr s = (error_counter := !error_counter + 1; msg (" ERR: " ^ s))
      fun msgErrors () = 
	if !error_counter = 0 then msg "\nTEST SUCCEEDED; there were no errors."
	else if !error_counter = 1 then msg "***TEST FAILED: there was 1 error."
	else msg ("***TEST FAILED: there were " ^ Int.toString (!error_counter) ^ " errors.")
      fun noOfErrors() = !error_counter
    end

    fun size_of_file filename =
      Int.toString ((OS.FileSys.fileSize filename) div 1024) ^ "K"
      handle _ => (msgErr("failed to obtain size of file `" ^ filename ^ "'"); "err")

    fun process_entry (filepath, opts) =
      let
	val _ = msg ("Processing file `" ^ filepath ^ "'")
	val _ = OS.Process.system "rm -f -r PM"     (* first delete PM directory *)
	fun opt t = List.exists (fn a => a=t) opts
	val recover : unit -> unit = 
	  let val memdir = OS.FileSys.getDir()
	  in fn () => OS.FileSys.chDir memdir
	  end
	val {dir, file} = OS.Path.splitDirFile filepath
	val _ = if dir="" then () else OS.FileSys.chDir dir
	val compile_command_base = "kit -logtofiles " ^ 
	  (if opt TestFile.NoBasisLib then "-nobasislib " else "") ^
          (if opt TestFile.NoOptimiser then "-nooptimiser " else "") ^
	  (if opt TestFile.TimeCompiler then "-timings " else "") ^
          (if opt TestFile.CompareCompilerLogs then "-reportfilesig " else "")

	val compile_command = compile_command_base ^ file	  
	val compile_command_prof = compile_command_base ^ "-prof " ^ file
	val compile_command_gc = compile_command_base ^ "-gc " ^ file
	val compile_command_gc_prof = compile_command_base ^ "-gc -prof " ^ file

	fun maybe_compare_complogs success =
	  let fun success_as_expected() =
	        if opt TestFile.ExpectCompileTimeError then 
		  if success then (msgErr "unexpected compile time success"; false)
		  else (msgOk "expected compile time failure"; true)
		else 
		  if success then (msgOk "expected compile time success"; true)
		  else (msgErr "unexpected compile time failure"; false)
	  in
	    if opt TestFile.CompareCompilerLogs then
	      let val match = if equal_to_okfile (file ^ ".log") then (msgOk "log equal to log.ok"; true)
			      else (msgErr "log not equal to log.ok"; false)
	      in TestReport.add_compout_line {name=filepath, match=SOME match,
					      success_as_expected=success_as_expected()}
	      end
	    else if success_as_expected() then ()
		 else TestReport.add_compout_line {name=filepath, match=NONE,
						   success_as_expected=false}
	  end

	fun maybe_report_comptimes() =
	  if opt TestFile.TimeCompiler then
	    let val entries = CompilerTimings.from_file "KITtimings"
	    in TestReport.add_comptime_line {name=filepath, entries=entries}
	    end handle CompilerTimings.ReadTimingsError s => (msgErr s; ())
	  else ()

	val exe_file = "runexe"
	fun rename_and_run() =
	  if OS.Process.system ("mv run " ^ exe_file) = OS.Process.success then
	    let 
	      fun test_output () =
		if equal_to_okfile (file ^ ".out") then
		  (msgOk "out equal to out.ok"; true)
		else (msgErr "out not equal to out.ok"; false)
	    in 
	      if opt TestFile.TimeExecutable then
		let val {max_mem_size,max_res_size,real,user,sys} =
		       MemTime.memtime {msg=msg',program=exe_file,outputfile=file ^ ".out"}
		    val ok = test_output()
		    val exesize = size_of_file exe_file
		    val exesize_stripped = 
		      if OS.Process.system ("strip " ^ exe_file) = OS.Process.success then
			size_of_file exe_file
		      else (msgOk ("the command `strip " ^ exe_file ^ "' failed"); "N/A")
		in
		  TestReport.add_runtime_line{name=filepath,ok=ok,exesize=exesize, 
					      exesize_stripped=exesize_stripped, 
					      max_mem_size=max_mem_size, 
					      max_res_size=max_res_size,real=real,
					      user=user,sys=sys}

		end handle MemTime.Crash s => (msgErr (exe_file ^ " failure: " ^ s); 
					       TestReport.add_runtime_bare_line(filepath,false))
	      else
		if OS.Process.system (exe_file ^ " > " ^ file ^ ".out") = OS.Process.success then
		  TestReport.add_runtime_bare_line(filepath,test_output())
		else (msgErr (exe_file ^ " failure");
		      TestReport.add_runtime_bare_line(filepath,false))
	    end
	  else (msgErr "rename (mv) failure";
		TestReport.add_runtime_bare_line(filepath,false))

	fun maybe_trywith(file,out_file, outok_file, test_option, compile_command, add_line_testreport, exe_cmd_args) =
	  let
	    fun test_output () =
	      if files_equal (file^out_file, file^outok_file) then
		(msgOk (out_file ^ " equal to " ^ outok_file); true)
	      else (msgErr (out_file ^ " not equal to " ^ outok_file); false)
	  in
	    if test_option() then
	      (msg' (" executing command `" ^ compile_command ^ "'");
	       if OS.Process.system compile_command = OS.Process.success then
		 if OS.Process.system ("mv run " ^ exe_file) = OS.Process.success then
		   if OS.Process.system (exe_file ^ " " ^ exe_cmd_args ^ " > " ^ file^out_file) = OS.Process.success then
		     add_line_testreport (filepath,test_output())
		   else (msgErr "run failure";
			 add_line_testreport (filepath,false))
		 else (msgErr "rename (mv) failure";
		       add_line_testreport (filepath,false))
	       else (msgErr "compile failure";
		     add_line_testreport (filepath,false)))
	    else ()
	  end

	fun maybe_trywithprof() = maybe_trywith(file,
						".outp",
						".out.ok",
						fn () => opt TestFile.Profiling,
						compile_command_prof,
						TestReport.add_profile_line, 
						"")
(*	fun maybe_trywithprof() =
	  let
	    val outp_file = file ^ ".outp"
	    val outok_file = file ^ ".out.ok"
	    fun test_output () =
	      if files_equal (outp_file, outok_file) then
		(msgOk "outp equal to out.ok"; true)
	      else (msgErr "outp not equal to out.ok"; false)
	  in
	    if opt TestFile.Profiling then
	      (msg' (" executing command `" ^ compile_command_prof ^ "'");
	       if OS.Process.system compile_command_prof = OS.Process.success then
		 if OS.Process.system ("mv run " ^ exe_file) = OS.Process.success then
		   if OS.Process.system (exe_file ^ " > " ^ outp_file) = OS.Process.success then
		     TestReport.add_profile_line (filepath,test_output())
		   else (msgErr "run failure";
			 TestReport.add_profile_line (filepath,false))
		 else (msgErr "rename (mv) failure";
		       TestReport.add_profile_line (filepath,false))
	       else (msgErr "compile failure";
		     TestReport.add_profile_line (filepath,false)))
	    else ()
	  end 20/04/1999, Niels*)

	fun maybe_trywithgc() = maybe_trywith(file,
					      ".outgc",
					      ".out.ok",
					      fn () => opt TestFile.GC,
					      compile_command_gc,
					      TestReport.add_gc_line,
					      " -silent_gc ")
	fun maybe_trywithgcprof() = maybe_trywith(file,
						  ".outgcp",
						  ".out.ok",
						  fn () => opt TestFile.GC andalso opt TestFile.Profiling,
						  compile_command_gc_prof,
						  TestReport.add_gc_profile_line,
						  " -silent_gc ")
	fun maybe_trywithtags() = maybe_trywith(file,
						".outtags",
						".out.ok",
						fn () => opt TestFile.Tags,
						compile_command_gc,
						TestReport.add_tags_line,
						" -disable_gc -silent_gc ")
	fun maybe_trywithtagsprof() = maybe_trywith(file,
						    ".outtagsp",
						    ".out.ok",
						    fn () => opt TestFile.Tags andalso opt TestFile.Profiling,
						    compile_command_gc_prof,
						    TestReport.add_tags_profile_line,
						    " -disable_gc -silent_gc ")
      in
	msg' (" executing command `" ^ compile_command ^ "'");
        if OS.Process.system compile_command = OS.Process.success then
	  (maybe_compare_complogs true; 
	   maybe_report_comptimes();
	   rename_and_run();
	   maybe_trywithprof();
	   maybe_trywithgc();
	   maybe_trywithgcprof();
	   maybe_trywithtags();
	   maybe_trywithtagsprof())
	else
	  maybe_compare_complogs false;
	recover()
      end

    fun process_args [testfile] = SOME testfile
      | process_args _ = NONE

    fun print_usage progname = print("\nusage: " ^ progname ^ " testfile\n")

    fun main (progname, args) =
      case process_args args
	of SOME testfile =>
	  let val log = "TESTmessages"
	    val _ = reset_error_counter()
	    val _ = TestReport.reset()
	    val _ = MemTime.bin_directory := OS.Path.joinDirFile{dir=OS.FileSys.getDir(),file="bin"}
	  in (msglog:=TextIO.openOut(log);
	      case TestFile.parse testfile
		of NONE => OS.Process.failure
		 | SOME (testfile_string,entries) => 
		  let val entries = map (fn TestFile.SML entry => entry
		                          | TestFile.PM entry => entry) entries
		  in app process_entry entries;
		    msgErrors();
		    TestReport.export {errors=noOfErrors(),testfile_string=testfile_string};
		    if noOfErrors() = 0 then OS.Process.success else OS.Process.failure
		  end) before (TextIO.closeOut (!msglog))
	  end
	 | NONE => (print_usage progname; OS.Process.failure)

    fun install() =
      let val _ = print "\n ** Installing Kit Tester, a tool for testing the Kit **\n\n"

	  val kit_src_tools_tester_path = OS.FileSys.getDir()   (* assumes we are in kit/src/Tools/Tester directory *)
	  val kit_bin_path = OS.Path.mkCanonical (OS.Path.concat(kit_src_tools_tester_path, "../../../bin"))
	  val kit_bin_kittester_path = OS.Path.joinDirFile{dir=kit_bin_path, file="kittester"}

          fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())
	  fun kittester_image() =
	    case arch_os()
	      of ("X86", "Linux") => "kittester.x86-linux"
	       | ("HPPA", "HPUX") => "kittester.hppa-hpux"
	       | ("SPARC", "Solaris") => "kittester.sparc-solaris"
	       | ("SUN", "OS4") => "unknown"
	       | _ => "unknown"
	  val kit_bin_kittester_image_path = OS.Path.joinDirFile{dir=kit_bin_path, file=kittester_image()}
	  val os = TextIO.openOut kit_bin_kittester_path
	  val _ = (TextIO.output(os, "sml @SMLload=" ^ kit_bin_kittester_image_path ^ " $*"); TextIO.closeOut os)
	  val _ = OS.Process.system("chmod a+x " ^ kit_bin_kittester_path)
	    handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ kit_bin_kittester_path ^ "' failed***\n");
			 OS.Process.failure)
      in SMLofNJ.exportFn(kit_bin_kittester_path,main)
      end

    val _ = install()

  end

(*
fun mk() = (OS.FileSys.chDir "/home/disk07/mael/kit/src/Tools/Tester/";
	    CM.make())

val cd = OS.FileSys.chDir
fun cdsrc() = cd "/home/disk07/mael/kit/src/Tools/Tester/"
fun cdtest() = cd "/home/disk07/mael/kit/test/"
*)