
signature TESTER =
  sig
    val main : string * string list -> OS.Process.status
  end

structure Tester : TESTER =
  struct
    val log = "TESTmessages"
    val logdirect = ref false

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
      val dotcounter = ref 0
      fun pr_dot () =
          if !dotcounter < 60 then
            ( dotcounter := !dotcounter + 1
            ; print ".")
          else ( dotcounter := 1
               ; print "\n.")
    in
      fun reset_error_counter() = error_counter:=0
      val msglog = ref TextIO.stdOut
      fun msg0 s = (TextIO.output(!msglog,s ^ "\n"); TextIO.flushOut (!msglog);
		   TestReport.add_log_line s)
      fun msg s = (msg0 s; pr_dot())
      fun msgp s = (msg0 s; print ("\n" ^ s ^ "\n"); dotcounter := 0)
      fun msg' s = (TextIO.output(!msglog,s ^ "\n"); TextIO.flushOut (!msglog); pr_dot())
      fun msgOk s = msg (" ok: " ^ s)
      fun msgErr s = (error_counter := !error_counter + 1; msgp (" ERR: " ^ s))
      fun msgErrors () =
	if !error_counter = 0 then msgp "TEST SUCCEEDED; there were no errors."
	else if !error_counter = 1 then msgp "***TEST FAILED: there was 1 error."
	else msgp ("***TEST FAILED: there were " ^ Int.toString (!error_counter) ^ " errors.")
      fun noOfErrors() = !error_counter
    end

    fun size_of_file filename =
      Position.toString ((OS.FileSys.fileSize filename) div 1024) ^ "K"
      handle _ => (msgErr("failed to obtain size of file `" ^ filename ^ "'"); "err")

    fun concatWith sep l = (* not as String.concatWith! *)
	let fun cw (nil,acc) = concat(rev acc)
	      | cw (x::xs,acc) = cw(xs, sep::x::acc)
	in cw (l,nil)
	end

    fun process_entry flags (filepath, opts, kitexe) =
      let
	val _ = msg ("Processing file `" ^ filepath ^ "'")
	val _ = OS.Process.system "rm -f -r MLB"     (* first delete MLB directories *)
	fun opt t = List.exists (fn a => a=t) opts
	val recover : unit -> unit =
	  let val memdir = OS.FileSys.getDir()
	  in fn () => OS.FileSys.chDir memdir
	  end
	val {dir, file} = OS.Path.splitDirFile filepath
	val _ = if dir="" then () else OS.FileSys.chDir dir
	val compile_command_base = kitexe ^
          (if !logdirect then " " else " --log_to_file ") ^
	  (if opt "nobasislib" then "-no_basislib " else "") ^
	  (if opt "tc" (*Time Compiler*) then "--timings " else "") ^
          (if opt "ccl" (*Compare Compiler Logs*) then "--report_file_sig " else "")
	       ^ concatWith " " flags

	val compile_command = compile_command_base ^ file

	fun maybe_compare_complogs success =
	  let fun success_as_expected() =
	        if opt "ecte" (*Expect Compile Time Error*) then
		  if success then (msgErr ("unexpected compile time success for " ^ file); false)
		  else (msgOk "expected compile time failure"; true)
		else
		  if success then (msgOk "expected compile time success"; true)
		  else (msgErr ("unexpected compile time failure for " ^ file) ; false)
	  in
	    if opt "ccl" (*Compare Compiler Logs*) then
	      let val match = if equal_to_okfile (file ^ ".log") then (msgOk "log equal to log.ok"; true)
			      else (msgErr ("compile log " ^ file ^ ".log not equal to " ^ file ^ ".log.ok"); false)
	      in TestReport.add_compout_line {name=filepath, match=SOME match,
					      success_as_expected=success_as_expected()}
	      end
	    else if success_as_expected() then ()
		 else TestReport.add_compout_line {name=filepath, match=NONE,
						   success_as_expected=false}
	  end

	fun maybe_report_comptimes() =
	  if opt "tc" (*Time Compiler*) then
	    let val entries = CompilerTimings.from_file "KITtimings"
	    in TestReport.add_comptime_line {name=filepath, entries=entries}
	    end handle CompilerTimings.ReadTimingsError s => (msgErr s; ())
                     | Time.Time => (msgErr "Time raised by maybe_report_comptimes"; ())
	  else ()

	val exe_file = "./runexe"
	val runargs = nil (*["-heap_to_live_ratio", "2.5"]*)
	fun rename_and_run(suffix, out_file, outok_file) =
	  if OS.Process.isSuccess(OS.Process.system ("mv run " ^ exe_file)) then
	    let
              val file_label = filepath ^suffix
   	      fun test_output () =
  	        if files_equal (file^out_file, file^outok_file) then
		  (msgOk (out_file ^ " equal to " ^ outok_file); true)
	        else (msgErr (file^out_file ^ " not equal to " ^ file ^ outok_file); false)
	    in
	      if opt "tx" (*Time Executable*) then
		let val _ = msg' ("    executing target program: " ^ exe_file)
	 	    val {count,size,rss,data,stk,exe,real,user,sys} =
		      MemUsage.memUsage {cmd=exe_file,args=runargs,out_file=file ^ out_file (*".out"*),eout_file=NONE}
		    val ok = test_output()
		    val exesize = size_of_file exe_file
		    val exesize_stripped =
		      if OS.Process.isSuccess(OS.Process.system ("strip " ^ exe_file)) then
			size_of_file exe_file
		      else (msgOk ("the command `strip " ^ exe_file ^ "' failed"); "N/A")
		in
		  TestReport.add_runtime_line{name=file_label,ok=ok,exesize=exesize,
					      exesize_stripped=exesize_stripped,
					      size=size,data=data,
					      rss=rss,stk=stk,exe=exe,
					      real=real,user=user,sys=sys}

		end handle Fail s => (msgErr (exe_file ^ " failure (" ^ file_label ^ "): " ^ s);
				      TestReport.add_runtime_bare_line(file_label,false))
                         | Time.Time =>
                                     (msgErr ("Time raised during execution of " ^ exe_file ^ " (" ^ file_label ^ ")");
				      TestReport.add_runtime_bare_line(file_label,false))
	      else
		let val res = OS.Process.system (exe_file ^ " > " ^ file ^ out_file ^ " 2>&1" (*".out"*))
		in
		  if (not(opt "ue" (*Uncaught Exception*) ) andalso OS.Process.isSuccess res)
		    orelse (opt "ue" (*Uncaught Exception*) andalso not(OS.Process.isSuccess res)) then
		      TestReport.add_runtime_bare_line(file_label,test_output())
		  else (msgErr (exe_file ^ " failure ("  ^ file_label ^ ")");
			TestReport.add_runtime_bare_line(file_label,false))
		end
	    end
	  else (msgErr "rename (mv) failure";
		TestReport.add_runtime_bare_line(filepath,false))
      in
	msg' (" executing command `" ^ compile_command ^ "'");
        if OS.Process.isSuccess(OS.Process.system (compile_command ^
                                                   (if !logdirect then " > " ^ file ^ ".log"
                                                    else " >> ./" ^ log))) then
	  (maybe_compare_complogs true;
	   maybe_report_comptimes();
	   rename_and_run(" ri ",".out",".out.ok")
          )
	else
	  maybe_compare_complogs false;
	recover()
      end

    fun process_args (kitexe::"--logdirect"::testfile::flags) =
        (logdirect := true; SOME (kitexe,testfile,flags))
      | process_args (kitexe::testfile::flags) = SOME (kitexe,testfile,flags)
      | process_args _ = NONE

    fun print_usage progname = print("\nUsage: kittester mlkit [--logdirect] testfile [OPTION...]\n\
				     \  mlkit: path to executable MLKit\n\
                                     \  --logdirect: use direct logging without -log_to_file\n\
				     \  testfile: path to test file.\n\
				     \  OPTION... are passed on to the MLKit for each\n\
				     \    compilation.\n")

    fun main (progname, args) =
        case process_args args of
            SOME (kitexe,testfile,flags) =>
	    let val _ = (reset_error_counter())
                        handle Time.Time => (print "bad time4\n" ; raise Fail "bad")
	        val _ = (TestReport.reset())
                        handle Time.Time => (print "bad time5\n" ; raise Fail "bad")
	    in (msglog:=TextIO.openOut(log);
	        case TestFile.parse testfile
		 of NONE => OS.Process.failure
		  | SOME (testfile_string,entries) =>
		    let val entries = map (fn TestFile.SML (filepath,opt) => (filepath,opt,kitexe)
		                          | TestFile.MLB (filepath,opt) => (filepath,opt,kitexe)) entries
		    in (app (process_entry flags) entries)
                       handle Time.Time => (print "bad time2\n" ; raise Fail "bad2") ;
		       msgErrors();
		       (TestReport.export {errors=noOfErrors(),testfile_string=testfile_string, kitexe=kitexe})
                       handle Time.Time => (print "bad time1\n" ; raise Fail "bad1")
                       ;
                         (*		    if noOfErrors() = 0 then OS.Process.success else OS.Process.failure *)
		         OS.Process.success   (* to make make work! mael 2001-10-22 *)
		    end) before (TextIO.closeOut (!msglog))
	    end
	  | NONE => (print_usage progname; OS.Process.failure)

(*
    fun install() =
      let val _ = print "\n ** Installing KitTester, a tool for testing the MLKit **\n\n"

	  val kit_src_tools_tester_path = OS.FileSys.getDir()   (* assumes we are in kit/src/Tools/Tester directory *)
	  val kit_bin_path = OS.Path.mkCanonical (OS.Path.concat(kit_src_tools_tester_path, "../../../bin"))
	  val kit_bin_kittester_path = OS.Path.joinDirFile{dir=kit_bin_path, file="kittester"}

(*          fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())
	  fun kittester_image() =
	    case arch_os()
	      of ("X86", "Linux") => "kittester.x86-linux"
	       | ("X86", "BSD") => "kittester.x86-bsd"
	       | ("HPPA", "HPUX") => "kittester.hppa-hpux"
	       | ("SPARC", "Solaris") => "kittester.sparc-solaris"
	       | ("SUN", "OS4") => "unknown"
	       | _ => "unknown" *)
(*
	  val kit_bin_kittester_image_path = OS.Path.joinDirFile{dir=kit_bin_path, file=kittester_image()}
	  val os = TextIO.openOut kit_bin_kittester_path
	  val _ = (TextIO.output(os, "sml @SMLload=" ^ kit_bin_kittester_image_path ^ " $*"); TextIO.closeOut os)
	  val _ = OS.Process.system("chmod a+x " ^ kit_bin_kittester_path)
	    handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ kit_bin_kittester_path ^ "' failed***\n");
			 OS.Process.failure)
      in SMLofNJ.exportFn(kit_bin_kittester_path,main)
*)
      in ()
      end *)


    val _ = (main(CommandLine.name (), CommandLine.arguments ()))
            handle Time.Time => (print "bad time\n" ; raise Fail "bad")
                 | IO.Io{function, name, cause} => raise Fail (function ^ ", " ^ name ^ ", " ^ (General.exnMessage cause))
  end
