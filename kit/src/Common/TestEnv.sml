
signature TEST_ENV =
sig
  val test : unit -> unit
end


functor TestEnv(structure TestInfo: TEST_INFO
		structure Flags: FLAGS
		structure Manager: MANAGER
		structure Basics: BASICS
		structure Timing: TIMING) : TEST_ENV =
  struct

    structure String = Edlib.String
    structure StringParse = Edlib.StringParse
    structure List = Edlib.List
    structure ListSort = Edlib.ListSort
    structure Int = Edlib.Int
    structure Bool = Edlib.Bool

    exception Crash_test of string
    open TestInfo

    (*********************************************************************************)
    (* Global values in this module which should NOT change from version to version. *)
    (*********************************************************************************)

    val test_env_directory = Flags.lookup_string_entry "test_env_directory"               (* Should _not_ be changed. *)
    val kit_version = Flags.lookup_string_entry "kit_version"                             (* Should _not_ be changed. *)
    fun source_directory () = OS.Path.concat(!test_env_directory, "Sources")              (* Should _not_ be changed. *)
    fun bin_directory () = OS.Path.concat(!test_env_directory,"bin")                      (* Should _not_ be changed. *)
    fun kit_script_directory () = OS.Path.concat(!test_env_directory,"KitScripts")        (* Should _not_ be changed. *)
    fun new_version_dir () = OS.Path.concat(!test_env_directory, !kit_version)            (* Should _not_ be changed. *)
    val target_directory = new_version_dir                                                (* Should _not_ be changed. *)
    val kit_architecture = Flags.lookup_string_entry "kit_architecture"                   (* Should _not_ be changed. *)

    fun test_report_filename () = OS.Path.concat(new_version_dir(), "test_report" ^ !kit_version)  (* Should _not_ be changed. *)
    fun test_report_dvi () = OS.Path.concat(new_version_dir(), "test_report" ^ !kit_version ^ ".dvi") (* Should _not_ be changed. *)
    val test_report_stream   = ref TextIO.stdOut                                                (* Should _not_ be changed. *)
    val test_report : string list ref = ref []                                            (* Should _not_ be changed. *)
    val test_log_report : string list ref = ref []                                        (* Should _not_ be changed. *)

    val test_log_stream = ref TextIO.stdOut                                                     (* Should _not_ be changed. *)

    (* ---------------------------------------------------------------------- *)
    (*    Adding dynamic flags.                                               *)
    (* ---------------------------------------------------------------------- *)

    val _ = Flags.add_string_to_menu (["Test environment"], "test_log",
				      "Test-log file name", ref "std_out")
    val _ = map (fn (x,y,r) => Flags.add_flag_to_menu (["Test environment"],x,y,r)) 
      [("acceptance_test", "acceptance test", ref true),
       ("performance_test", "performance test", ref false)]

    val test_log_string = Flags.lookup_string_entry "test_log"
    val acceptance_test_flag = Flags.lookup_flag_entry "acceptance_test"
    val performance_test_flag = Flags.lookup_flag_entry "performance_test"

    (*********************************)
    (* System functions.             *)
    (*********************************)

    fun pr s = TextIO.output (TextIO.stdOut, s)

    fun shorten_string s n =
      if String.size s > n then
	"..." ^ (String.truncL (n-3) s)
      else s

    local
      fun shorten_filename' filename =
	let
	  val groups = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890`-=~!@#$%^&*()_+|][{};'.,<>?:"
	  val singles = "/"
	  val preserveGroups = true
	  val preserveSingles = true
	  val arg = {groups = groups, 
		     singles = singles, 
		     preserveGroups = preserveGroups, 
		     preserveSingles = preserveSingles}
	  val dir_list = StringParse.words arg filename
	  val arg' = {groups = groups, 
		      singles = singles, 
		      preserveGroups = true, 
		      preserveSingles = true}
	  val test_env_directory_list = StringParse.words arg' (!test_env_directory)
	  val test_dir_name = 
	    case List.rev test_env_directory_list
	      of "/"::name::rest => name
	       | name::rest => name
	       | [] => ""
	  fun dropUntil p [] = []
	    | dropUntil p (x::xs) = 
	        if p x then 
		  dropUntil p xs
		else
		  (x::xs)
	  val res_list = dropUntil (fn s => s <> test_dir_name) dir_list
	in
	  List.foldL (fn str => fn baseStr => baseStr ^ str) ".../" res_list 
	end
      handle _ => raise Crash_test "Error in shorten_filename. "
    in
      fun shorten_filename filename =
	shorten_filename' filename
    end

    fun get_filename s =
      let
	val groups = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890`-=~!@#$%^&*()_+|][{};'.,<>?:"
	val singles = "/"
	val preserveGroups = true
	val preserveSingles = false
	val arg = {groups = groups, 
		   singles = singles, 
		   preserveGroups = preserveGroups, 
		   preserveSingles = preserveSingles}
      in
	List.last(StringParse.words arg s)
	handle List.Empty s => raise Crash_test ("Error in get_filename: " ^ s)
      end

    (* Logs *)
    val new_line = "\n      : "
    local
      fun add_to_log s = test_log_report := (!test_log_report @ [s])
    in
      fun ok_log s = TextIO.output(!test_log_stream, "OK    : " ^ s ^ "\n")
      fun error_log s = TextIO.output(!test_log_stream, "ERROR : " ^ s ^ "\n")
      fun ok_log_report s = (ok_log s;
			     add_to_log ("OK    : " ^ s))
      fun error_log_report s = (error_log s;
				add_to_log ("ERROR : " ^ s))
    end

    (* Return true if file or directory d exists. *)
    fun exists_file d = OS.FileSys.access (d,[])
      handle _ => raise Crash_test ("Error in trying to see if file " ^ d ^ " exists.")

    (* Return true if directory exists *)
    fun exists_directory d = OS.FileSys.isDir d 
      handle _ => raise Crash_test ("Error in trying to see if directory " ^ d ^ " exists.")

    (* Return working directory as a string *)
    fun get_working_directory () = OS.FileSys.getDir ()
      handle _ => raise Crash_test ("Cannot get working directory.")

    (* Return size of file in bytes. *)
    fun size_of_file filename =
      OS.FileSys.fileSize filename
      handle _ => raise Crash_test ("Error in size_of_file " ^ ((*shorten_filename*) filename))

    (* Create directory d, with specified access rights. *)
    val access_rights = 505
    fun create_dir d = OS.FileSys.mkDir (d (*,access_rights*))
      handle _ => raise Crash_test("Cannot create directory: " ^ d ^ ".")

    (* Check for directory d, and if exists, then continue. *)
    (* If the directory does not exists, then create it.    *)
    fun ensure_dir_exists d =
      if exists_file d then 
	ok_log ("File or directory " ^ (shorten_filename d) ^ " exists and will be used.")
      else
	(ok_log_report ("Directory " ^ (shorten_filename d) ^ " will be created.");
	 create_dir d;
	 ())


    (* Execute shell command and return the result code. *)
    fun execute_shell_command command =
      let val status = OS.Process.system command
      in if status = OS.Process.success then ()
	 else raise Crash_test ("Error no. " ^ Int.string status
				^ " when executing shell command " ^ command ^ ".")
      end handle OS.SysErr(s,_) =>
	          raise Crash_test ("Error in executing shell command " ^ command ^ "."
				    ^ new_line ^ "Reported error: " ^ s)
               | _ =>
		    raise Crash_test ("Unknown exception when executing shell command "
				      ^ command ^ ".")

    (* time_command times the command.     *)
    (* The time information is read from a *)
    (* temporary file.                     *)
    type timings = {max_mem_size: string,
		    max_res_size: string,
		    real : string,
		    user : string,
		    sys  : string}

    (* This function uses shell script memtime located in the *)
    (* Target directory. In future, it should be replaced by  *)
    (* ML code.                                               *)
    fun memtime program outputfile : timings= 
      let
	val tempfile = OS.Path.joinDirFile{dir=target_directory(), file="timex.temp"}
	val shell_command =
	  case !kit_architecture 
	    of "HPUX" =>
	      let (* We use shell script for HPUX. *)
		val memtime_exe = OS.Path.joinDirFile{dir=bin_directory(), file="memtime_hpux"}
		val _ = ok_log_report ("Executing target program: " ^ new_line ^ "memtime_hpux -f " ^ 
				       (shorten_filename outputfile) ^ new_line ^
				       "         -o " ^ (shorten_filename tempfile) ^ " " ^ program)
	      in
		(memtime_exe ^ " -f " ^ outputfile ^ " -o " ^ tempfile ^ " " ^ program)
	      end
	     | "SUN_OS4" =>
	      let (* We use shell script for SUN_OS4. *)
		val memtime_exe = OS.Path.joinDirFile{dir=bin_directory(), file="memtime_sun_os4"}
		val temp = OS.Path.joinDirFile{dir=target_directory(), file="time.temp"}
		val _ = ok_log_report ("Executing target program: " ^ new_line ^ "memtime_sun_os4 -f " ^ 
				       shorten_filename outputfile ^ new_line
				       ^ "         -o " ^ (shorten_filename tempfile) ^ new_line
				       ^ "         -t " ^ (shorten_filename temp) ^ " " ^ program)
	      in
		(memtime_exe ^ " -f " ^ outputfile ^ " -o " ^ tempfile ^ " -t " ^ temp ^ " " ^ program)
	      end
	     | _ => raise Crash_test ("Error in memtime, does not know architecture: " ^ !kit_architecture)

	val _ = execute_shell_command shell_command

	val input_stream = TextIO.openIn (tempfile)
	fun input_value name_of_value =
	  let
	    val input_line = String.skipSpaces (String.dropR "\n" (TextIO.inputLine (input_stream)))
	    val start = String.size name_of_value
	    val finish = String.size input_line
	    val _ = 
	      if (String.extract 0 start input_line) <> name_of_value then
		raise Crash_test ("Error in memtime, value name " ^ name_of_value ^ 
				  " does not match value in input file " ^ (String.extract 0 start input_line))
	      else
		()
	    val input_line = String.extract start finish input_line
	  in
	    String.skipSpaces input_line
	  end

	val max_mem_size = input_value "MAX MEM SIZE:"
	val max_res_size = input_value "MAX MEM RES:"
	val real = input_value "Real time:"
	val user = input_value "User time:"
	val sys  = input_value "System time:"
	val _ = TextIO.closeIn input_stream
      in
	{max_mem_size = max_mem_size, max_res_size = max_res_size, 
	 real = real, user = user, sys = sys}
      end
    handle IO.Io {name=s,...} => raise Crash_test ("Error in memtime: " ^ s)

    (* Get the Unix environment and return the value of one env. variable. *)
    fun get_env_var env_name = 
      case OS.Process.getEnv env_name
	of SOME s => s 
	 | NONE => "Environment name " ^ env_name ^ " does not exist."
    
    (* Delete file filename if it exists. *)
    (* Otherwise do nothing.              *)
    (* Be careful with this function (-f) *)
    fun delete_file filename =
      execute_shell_command ("rm -f " ^ filename)

    (*mv: rename file*)
    fun mv filename1 filename2 =
          execute_shell_command ("mv " ^ filename1 ^ " " ^ filename2)

    (*create_dir_and_maybe_rename_old d = Create directory d.
     If d already exists, rename it to something else first.*)

    local
      fun new_name d = if exists_file d then new_name (d ^ "-") else d
      fun rename d =
          let val d' = new_name d
	  in
	    (ok_log_report
	       ("\n\nThe directory\n\n        "
		^ d ^ "\n\nalready exists.  I will rename it to\n\n        "
	        ^ d' ^ "\n\n");
	     mv d d')
	  end
(*
      fun remove_trailing_slashes s =
	    (case rev (explode s) of
	       #"/" :: ss => implode (rev ss)
	     | _ => s)
*)
    in
      fun create_dir_and_maybe_rename_old d =
	    let (* val d = remove_trailing_slashes d *)
	    in
	      (if exists_file d then rename d else ();
	       ok_log_report ("Directory " ^ shorten_filename d
			      ^ " will be created.");
	       create_dir d;
	       ())
	    end
    end (*local*) 

    (* Compare two files, and return the first line     *)
    (* in which there is a mitch match.                 *)
    (* Are the two files alike, then MATCH is returned. *)
    datatype diff = MATCH
                  | DIFF of int
                  | ERROR of string

    fun diff file1 file2 =
      let
	val file1_stream = TextIO.openIn(file1) 
	val file2_stream = TextIO.openIn(file2)

	fun close_files () =
	  (TextIO.closeIn file1_stream;
	   TextIO.closeIn file2_stream)

	fun match line = 
	  let
	    val c1 = TextIO.inputN(file1_stream, 1)
	    val c2 = TextIO.inputN(file2_stream, 1)
	    val line' = 
	      if c1 = "\n" then
		line+1
	      else
		line
	  in
	    if c1 = c2 then
	      if TextIO.endOfStream(file1_stream) andalso
		TextIO.endOfStream(file2_stream) then
		(close_files ();
		 MATCH)
	      else
		match line'
	    else
	      (close_files ();
	       DIFF line')
	  end
      in
	match 1
      end handle IO.Io {name=s,...} => ERROR "IO error in diff, maybe cannot open file."

    (* Change working directory. *)
    fun change_directory d = 
      OS.FileSys.chDir d 
      handle _ => raise Crash_test ("Error in change to directory: " ^ d)

    fun read_script script_name = 
      (Flags.read_script script_name;
       ok_log_report ("Reading script: " ^ (shorten_filename script_name)))
      handle Flags.ParseScript _ => raise Crash_test ("Error in reading script file: " ^ ((*shorten_filename*) script_name))

    fun read_file filename =
      let
	val _ = ok_log ("Reading file : " ^ filename)
	val in_stream = TextIO.openIn (filename)
	fun counter (n, lines) = 
	  if TextIO.endOfStream(in_stream) then
	    (n, List.rev lines)
	  else
	    counter (n+1, (String.dropR "\n" (TextIO.inputLine in_stream))::lines)
	val result = counter (0, [])
	val _ = TextIO.closeIn in_stream
      in
	result
      end
    handle _ => (error_log_report ("Error in read_file: " ^ filename);
		 (0, []))

    (****************************************************)
    (* LaTeX and other layout functions                 *)
    (****************************************************)

    fun fix2 str =
      let
	val str_list = List.rev (explode str)

	fun remove_dec [] = []
	  | remove_dec (d1 :: #"." :: rest) = List.rev (d1 :: #"." :: rest)
	  | remove_dec (d1 :: d2 :: #"." :: rest) = List.rev (d1 :: d2 :: #"." :: rest)
	  | remove_dec (d1 :: rest) = remove_dec rest
      in
	implode (remove_dec str_list)
      end

    fun split_sentence_into_words s =
      let
	val groups = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890`-=~!@#$%^&*()_+|][{};'.,<>?:"
	val singles = " "
	val preserveGroups = true
	val preserveSingles = false
	val arg = {groups = groups, 
		   singles = singles, 
		   preserveGroups = preserveGroups, 
		   preserveSingles = preserveSingles}
      in
	StringParse.words arg s
      end

    fun reset_test_report () = (test_report := [];
				test_log_report := [])

    fun add_line_test_report line =
      test_report := (line :: (!test_report))

    fun add_lines_test_report lines =
      List.map (fn line => add_line_test_report line) lines

    fun add_error_test_report header error_msg =
      add_lines_test_report 
        [header,
	 "\\begin{quote}",
	 error_msg,
	 "\\end{quote}"]

    fun open_test_report () =
      TextIO.openOut (test_report_filename () ^ ".tex")
      handle IO.Io {name=s,...} => (error_log ("Can not open test report: " ^ test_report_filename () ^ ".tex");
		      raise Crash_test s)

    fun close_test_report () =
      TextIO.closeOut (!test_report_stream)

    local
      fun convert_line_to_latex line =
        concat (List.map (fn ch => if ch = #"_" then "\\_" else str ch) (explode line))

      fun output_line line = TextIO.output((!test_report_stream), ((convert_line_to_latex line) ^ "\n"))

      fun output_line_verbatim line = TextIO.output((!test_report_stream), (line ^ "\n"))

      fun init_test_report () = 
	List.map (fn str => output_line str) 
	  ["\\documentclass[a4paper,10pt,oneside]{article}",
	   "\\textwidth 190mm",
	   "\\textheight 248mm",
	   "\\topmargin -1.0cm",      
	   "\\evensidemargin -8.4mm",
	   "\\oddsidemargin -8.4mm",
	   "\\title{ML Kit Test Report \\\\ for \\\\ ML Kit version " ^ !kit_version ^ "}",
	   "\\author{Author: The ML Kit Test Environment}", 
	   "\\date{\\today}",
	   "\\begin{document}",
	   "\\maketitle",
	   "\\abstract{This test report is generated by the test environment in the ML Kit. " ^ 
	   "All tests has been executed on " ^ (get_env_var "HOST") ^ ".}",
	   "\\tableofcontents"]

      fun finish_test_report () = 
	List.map (fn str => output_line str) 
	   ["\\end{document}"]	  

      fun export_log () =
	List.map (fn str => output_line_verbatim str)
	  (["\\newpage",
	    "\\section{Log file}",
	    "\\begin{footnotesize}",
	    "\\begin{verbatim}"] @
           (!test_log_report) @
	   ["\\end{verbatim}",
	    "\\end{footnotesize}"])

      fun export_latex_report' report =
	(init_test_report();
	 List.map (fn str => output_line str) (report);
	 export_log();
	 finish_test_report();
	 close_test_report ();
	 ok_log ("Test report exported to file: " ^ shorten_filename (test_report_filename ()) ^ ".tex"))
    in
      fun export_test_report () = export_latex_report' (List.rev (!test_report))
    end

    datatype layout = CENTER
                    | LEFT
                    | RIGHT
    type table = {Columns : int,                           (* Number of columns for check only.                         *)
                  Header : (string list * layout) list,    (* List with a list of headers for each column.              *)
                  Rows : string list list} ref             (* List with rows each containing COLUMNS number of strings. *)

    fun mk_table c h = ref {Columns = c,
			    Header = h,
			    Rows = []}

    fun insert_row table row =
      case table of 
	(ref {Columns, Header, Rows}) => table := {Columns = Columns, 
						   Header = Header, 
						   Rows = (row :: Rows)}

    fun normalize_table table =
      case table of 
	(ref {Columns, Header, Rows}) => {Columns = Columns, 
					  Header = Header, 
					  Rows = (List.rev Rows)}

    local 
      (* Header = [ ([Header_1_1,..., Header_1_N],layout), ..., ([Header_M_1, ..., Header_M_H],layout) ] *)
      (* Rows   = [ [Row_1_1,   ..., Row_1_M],             ..., [Row_Q_1,    ..., Row_Q_M]             ] *)
      fun log_table' {Columns, Header:(string list * layout) list, Rows} =
	let
	  val headers = List.map #1 Header
	  val layouts = List.map #2 Header

	  fun check_header header = (List.size header) = Columns

	  fun check_rows [] = true
	    | check_rows (xs::xss) = 
	    if (List.size xs) <> Columns then
	      false
	    else
	      check_rows xss
	      
	  fun calc_width w [] = w
	    | calc_width w (xs::xss) =
	    calc_width (List.foldL (fn s => (fn w => if (String.size s) > w then (String.size s) else w)) w xs) xss
	    
	  val column_width = calc_width 0 (Rows @ headers)
	    
	  fun make_field LEFT   s = String.padR " " column_width s
	    | make_field CENTER s = String.padC " " column_width s
	    | make_field RIGHT  s = String.padL " " column_width s
	    
	  val horizontal_column_line = String.padR "-" column_width ""
	  fun horizontal_line 0 res = res ^ "+"
	    | horizontal_line n res = horizontal_line (n-1) (res ^ "+" ^ horizontal_column_line)
	    
	  fun make_row [] [] s = s ^ "|"
	    | make_row (x::xs) (l::ls) s = make_row xs ls (s ^ "|" ^ (make_field l x))
	    | make_row _ _ _ = raise Crash_test "Error in log_table: make_row"

	  fun make_header [] s = s ^ "|"
	    | make_header (x::xs) s = make_header xs (s ^ "|" ^ (make_field CENTER x))

	  local
	    fun head_height [] h = h
	      | head_height (xs::xss) h = head_height xss (Int.max h (List.size xs))
		
	    fun make_headers' 0 headers rest = List.rev headers
	      | make_headers' n headers rest =
	          let
		    val header' = List.map (fn xs => case xs of [] => "" | (x'::xs') => x') rest
		    val rest'   = List.map (fn xs => case xs of [] => [] | (x'::xs') => xs') rest
		  in
		    make_headers' (n-1) (header' :: headers) rest'
		  end
	  in
	    fun make_headers headers = make_headers' (head_height headers 0) [] headers
	  end
	
	in
	  if check_rows Rows andalso 
	     check_header Header then
	    (ok_log (horizontal_line Columns "");
	     map (fn row => ok_log (make_header row "")) (make_headers headers);
	     ok_log (horizontal_line Columns "");
	     map (fn row => ok_log (make_row row layouts "")) Rows;
	     ok_log (horizontal_line Columns "");
	     ())
	  else
	    raise Crash_test "Error in size of LaTeX table."
	end
    in
      fun log_table table = log_table' (normalize_table table)
    end

    local
      (* Header = [ ([Header_1_1,..., Header_1_N],layout), ..., ([Header_M_1, ..., Header_M_H],layout) ] *)
      (* Rows   = [ [Row_1_1,   ..., Row_1_M],             ..., [Row_Q_1,    ..., Row_Q_M]             ] *)
      fun take 0 acc rest      = List.rev acc
	| take n acc []        = raise (Crash_test "take")
	| take n acc (r::rest) = take (n-1) (r::acc) rest
      fun drop 0 rest      = rest
	| drop n []        = raise (Crash_test "drop")
	| drop n (r::rest) = drop (n-1) rest
      fun split_table max_no_of_columns (table as {Columns, Header:(string list * layout) list, Rows}) tables = 
	if Columns <= (max_no_of_columns+1) then
	  List.rev (table :: tables)
	else
	  (case Header 
	     of (h::hs) => split_table max_no_of_columns {Columns=Columns-max_no_of_columns,
							  Header=h::(drop max_no_of_columns hs):(string list * layout) list,
							  Rows=List.map (fn row => 
									 case row
									   of (r::rs) => r::(drop max_no_of_columns rs)
									    | _ => raise (Crash_test "split_table.calc arg1")) Rows} 
	                               ({Columns=max_no_of_columns+1,
					 Header=h::(take max_no_of_columns [] hs):(string list * layout) list,
					 Rows = List.map (fn row =>
							  case row 
							    of (r::rs) => r::(take max_no_of_columns [] rs)
							     | _ => raise (Crash_test "split_table.calc arg2")) Rows} :: tables)
	      | _ => raise (Crash_test "split_table"))

      fun latex_table' {Columns, Header:(string list * layout) list, Rows} =
	let
	  fun check_header () = (List.size Header) = Columns

	  fun check_rows [] = true
	    | check_rows (xs::xss) = 
  	        if (List.size xs) <> Columns then
		  false
		else
		  check_rows xss

	  val headers = List.map #1 Header
	  val layouts = List.map #2 Header

	  local
	    fun pp_layout LEFT = "l"
	      | pp_layout CENTER = "c"
	      | pp_layout RIGHT = "r"
	  in
	    fun layout []      res = res ^ "|"
	      | layout (l::ls) res = layout ls (res ^ "|" ^ (pp_layout l))
	  end

	  fun init_table () = 
	    add_lines_test_report 
	      ["\\begin{center}",
	       "\\begin{tabular}{" ^ (layout layouts "") ^ "} \\hline"]

	  fun finish_table () = 
	    add_lines_test_report
	      ["\\end{tabular}",
	       "\\end{center}"]

	  fun make_head b a [] = "$" ^ b ^ a ^ "$"                                   (* Stackrel has to be in math mode. *)
	    | make_head b a (h :: []) = "$" ^ b ^ "\\mbox{" ^ h ^ "}" ^ a ^ "$"
	    | make_head b a (h :: hs) = make_head (b ^ "\\stackrel{\\mbox{" ^ h ^ "}}{") (a ^ "}") hs

	  fun make_row [] s make_item = add_line_test_report (s ^ "\\\\ \\hline")
	    | make_row [x] s make_item = add_line_test_report (s ^ (make_item x) ^ "\\\\ \\hline")
	    | make_row (x::xs) s make_item = make_row xs (s ^ (make_item x) ^ "&") make_item

	  fun compile_table rows =
	    (init_table();
	     make_row headers "" (make_head "" "");
	     List.map (fn row => make_row row "" (fn x => x)) rows;
	     finish_table();
	     ())

	  val max_no_of_rows = 55
	  fun split_rows Rows =
	    let
	      val s = List.size Rows
	    in
	      if s > max_no_of_rows then
		(List.extract 0 max_no_of_rows Rows) :: (split_rows (List.extract max_no_of_rows s Rows))
	      else
		[Rows]
	    end
	in
	  if check_rows (Rows) andalso check_header () then
	    List.apply compile_table (split_rows Rows)
	  else
	    raise Crash_test "Error in size of LaTeX table."
	end
    in
      fun latex_table table = 
	(List.map latex_table' (split_table 7 (normalize_table table) []);
	 ())
    end

    fun latex_test_report () =
      let
	val orig_dir = get_working_directory()
	val _ = change_directory (new_version_dir ())
	val _ = execute_shell_command ("latex2e " ^ test_report_filename () ^ ".tex")
	val _ = execute_shell_command ("latex2e " ^ test_report_filename () ^ ".tex")
	val _ = delete_file (test_report_filename () ^ ".aux")
	val _ = delete_file (test_report_filename () ^ ".log")
	val _ = delete_file (test_report_filename () ^ ".toc")
	val _ = change_directory orig_dir
      in
	ok_log ("Test report are compiled into " ^ shorten_filename (test_report_dvi ()) ^ ".")
      end

    (****************************************************)
    (* Functions used to compile the ML source programs *)
    (****************************************************)

    fun reset() = (Manager.reset();
		   Basics.Ident.reset())

    (* This function compiles a project and leaves a file "run" in the target directory. *)
    (* This file can then be executed.                                                   *)

    (* Because SML/NJ's runtime system starts with `run' we have problems fetching
     * memory information from the top program, hence we rename the generated `run'
     * file to run_project. *)

    fun evalProjects project_name =
      (Manager.build project_name;
       OS.FileSys.rename{old="run", new="run_project"})
      handle OS.SysErr _ => raise Crash_test "evalProjects. Problem with renaming of run file."

    fun gen_input_str NONE = ""
      | gen_input_str (SOME input_str) = " < " ^ input_str ^ " "

    fun gen_input_str_memtime NONE = ""
      | gen_input_str_memtime (SOME input_str) = " " ^ input_str ^ " "

    (****************************************************)
    (* Functions used by the ACCEPTANCE test.           *)
    (****************************************************)
    local
      fun add_acceptance_strategy_test_report (ACCEPTANCE_STRATEGY {kit_script, runtime_system, strategy_name, 
								    comment, exec_opt, old_dir}) =
	add_lines_test_report
          ["\\subsection{" ^ strategy_name ^ "}",
	   "The strategy is defined by: ",
	   "\\begin{quote}",
	   "\\begin{tabular}{ll}",
	   "Kit Script: & " ^ kit_script ^ "\\\\",
	   "Runtime system: & " ^ (shorten_string (runtime_system ()) 50) ^ "\\\\",
	   "Comment: & \\parbox{12cm}{" ^ comment ^ "}\\\\",
	   "Exec options: & \\parbox{12cm}{" ^ exec_opt ^ "}\\\\",
	   "Older version: & " ^ (case old_dir of
				    NONE   => "No older version defined."
				  | SOME v => (*(shorten_string v 50)*) v) ^ "\\\\",
	   "\\end{tabular}",
	   "\\end{quote}"]
	| add_acceptance_strategy_test_report _ =
	  raise Crash_test "Function add_acceptance_strategy_test_report called with wrong strategy."

      fun new_acceptance_dir () = OS.Path.concat(new_version_dir (),"Acceptance")

      fun acceptance_test_strategy (strategy as (ACCEPTANCE_STRATEGY {kit_script, runtime_system, strategy_name, 
								      old_dir, exec_opt, ...})) =
	(let
	   val old_version_dir = case old_dir of
	     NONE   => ""
	   | SOME v => v

	   val acceptance_table_files = mk_table 2 [(["Source"],LEFT), (["Result from diff"],LEFT)]
	   val acceptance_table_projects = mk_table 2 [(["Source"],LEFT), (["Result from diff"],LEFT)]
	   val _ = ok_log_report ("Starting new strategy: " ^ strategy_name)
	    
	   val new_compile_strategy_dir = OS.Path.concat(new_acceptance_dir(), strategy_name)
	   val _ = create_dir new_compile_strategy_dir

	   val _ = add_acceptance_strategy_test_report strategy

	   fun acceptance_test_file (filename,input_to_file) =
	     let
	       val filepath = OS.Path.joinDirFile{dir=source_directory(), file=filename}
	       val unitname = OS.Path.base filename
	       val _ = reset()
	       val _ = ok_log_report ("Compiling ML source file: " ^ filename ^ ".")
	       val _ = Manager.comp filepath
		       handle X => (TextIO.output(TextIO.stdOut, "something happened.");raise Crash_test "Error: Compile Error")
	       val _ = ok_log_report ("Compiled " ^ filename ^ new_line)

	       val exe_file = unitname ^ ".exe"		
	       val new_out_datafile = OS.Path.joinDirFile{dir=new_compile_strategy_dir, file= unitname ^ ".out"}
	       val shell_command = (exe_file ^ " " ^ exec_opt ^ 
				    (gen_input_str input_to_file) ^
				    " > " ^ new_out_datafile)
	       val _ = ok_log_report ("Executing target program: " ^ new_line ^ exe_file ^ " " ^ exec_opt ^ " > " ^
				      (shorten_filename new_out_datafile) ^ ".")
	       val _ = execute_shell_command shell_command (* If error it will raise Crash_test. *)
		
	       val old_out_datafile = OS.Path.joinDirFile{dir=old_version_dir, file=unitname ^ ".out"}
	       val table_entry_name = filename
	       val _ = 
		 case old_dir of
		   NONE => insert_row acceptance_table_files [table_entry_name, "No difference computed."]
		 | SOME v => 
		     case diff new_out_datafile old_out_datafile of
		       ERROR s => insert_row acceptance_table_files [table_entry_name, s]
		     | MATCH   => insert_row acceptance_table_files [table_entry_name, "Match ok."]
		     | DIFF n  => insert_row acceptance_table_files [table_entry_name, "Match error on line " ^ 
								     (Int.string n) ^ "."]
	     in
	       ()
	     end
	   handle Crash_test s => (error_log_report ("Error in acceptance_test_file: " ^ new_line ^ s ^ new_line ^
						     "Continue with next file.");
				   insert_row acceptance_table_files [filename, s ^ " Filename: " ^ filename])

	   fun acceptance_test_project (project_name,input_to_project) =
	     let
	       val _ = change_directory (source_directory())
	       val _ = reset()

	       val _ = ok_log_report ("Compiling project " ^ project_name ^ ".")
	       val _ = evalProjects project_name handle X => 
		 (TextIO.output(TextIO.stdOut, "something happened.");raise Crash_test "Error: Compile Error")
		
	       val new_out_datafile = OS.Path.joinDirFile{dir=new_compile_strategy_dir, file= OS.Path.base project_name ^ ".out"}
	       val shell_command = ("run_project " ^ exec_opt ^ 
				    (gen_input_str input_to_project) ^
				    " > " ^ new_out_datafile)
	       val _ = ok_log_report ("Executing target program: " ^ new_line ^ "run_project " ^ exec_opt ^ 
				      " > " ^ shorten_filename new_out_datafile ^ ".")
	       val _ = execute_shell_command shell_command (* If error it will raise Crash_test. *)
		
	       val old_out_datafile = OS.Path.joinDirFile{dir=old_version_dir, file=OS.Path.base project_name ^ ".out"}
	       val table_entry_name = project_name
	       val _ = 
		 case old_dir of
		   NONE => insert_row acceptance_table_projects [table_entry_name, "No difference computed."]
		 | SOME v => 
		     case diff new_out_datafile old_out_datafile of
		       ERROR s => insert_row acceptance_table_projects [table_entry_name, s]
		     | MATCH   => insert_row acceptance_table_projects [table_entry_name, "Match ok."]
		     | DIFF n  => insert_row acceptance_table_projects [table_entry_name, "Match error on line " ^ 
									(Int.string n) ^ "."]
	     in
	       ()
	     end
	   handle Crash_test s => (error_log_report ("Error in acceptance_test_project: " ^ new_line ^ s ^
						     new_line ^ "Continuing with next file.");
				   insert_row acceptance_table_projects [project_name, s ^ " Projectname: " ^ project_name])


	   val script_name = OS.Path.joinDirFile{dir=kit_script_directory(), file=kit_script}
	   val _ = read_script script_name

	   val _ = change_directory (target_directory ())

           (* Acceptance test on files. *)
	   val _ = add_lines_test_report ["\\subsubsection{Acceptance test on single source files}"]
	   val _ = map acceptance_test_file (acceptance_suite_files ())
	   val _ = log_table acceptance_table_files
	   val _ = latex_table acceptance_table_files

           (* Acceptance test on projects. *)
	   val _ = add_lines_test_report ["\\subsubsection{Acceptance test on projects}"]
	   val _ = map acceptance_test_project (acceptance_suite_projects ())
	   val _ = log_table acceptance_table_projects
	   val _ = latex_table acceptance_table_projects
	    
	 in
	   ()
	 end
       handle Crash_test s => (add_error_test_report "Error in acceptance_test_strategy: " 
			                             (s ^ ": Continue with next strategy.");
			       error_log_report ("Error in acceptance_test_strategy: " ^ new_line ^ s ^ 
						 new_line ^ "Continue with next strategy.")))
	| acceptance_test_strategy _ = raise Crash_test "Function acceptance_test_strategy called with wrong strategy."
    in
      (* This function performs the acceptance test. *)
      fun acceptance_test () = 
	let
	  val _ = add_lines_test_report
	        ["\\newpage",
		 "\\section{Acceptance Test}",
		 "This section contains the result of the acceptance test. Any error will be reported in the",
		 "rightmost column of the test tables. The following sections contains the result of",
		 "each strategy applied to the acceptance test."]

	  val _ = ok_log_report (new_line ^ "*************Now starting ACCEPTANCE test.***************")
	    
	  val _ = ensure_dir_exists (new_acceptance_dir ())
	    
	  val _ = map acceptance_test_strategy (acceptance_strategies ())
	    
	in
	  ()
	end
    end (* local acceptance test. *)

    (*********************************************)
    (* Functions used by the PERFORMANCE test.   *)
    (*********************************************)
    local
      fun add_performance_strategy_test_report (PERFORMANCE_STRATEGY {kit_script, runtime_system, strategy_name,
								      show_compiler_timings, comment}) =
	add_lines_test_report
          ["\\subsection{" ^ strategy_name ^ "}",
	   "The strategy is defined by: ",
	   "\\begin{quote}",
	   "\\begin{tabular}{ll}",
	   "Kit Script: & " ^ kit_script ^ "\\\\",
	   "Runtime system: & " ^ shorten_string (runtime_system ()) 50 ^ "\\\\",
	   "Show compile timings: & " ^ (Bool.string show_compiler_timings) ^ "\\\\",
	   "Comment: & \\parbox{12cm}{" ^ comment ^ "}\\\\",
	   "\\end{tabular}",
	   "\\end{quote}"]
	| add_performance_strategy_test_report _ = 
	  raise Crash_test "Function add_performance_strategy_test_report called with wrong strategy."

      fun new_target_program_dir () = OS.Path.concat(new_version_dir(),"PerformanceTest")

      fun performance_test_strategy (strategy as (PERFORMANCE_STRATEGY {kit_script, runtime_system, strategy_name,
									show_compiler_timings, ...})) =
	(let
	   val performance_table_files = mk_table 8 [(["Source"],LEFT), 
						     (["Size", "of", "exec."],RIGHT),
						     (["Size", "of", "stripped", "exec."],RIGHT),
						     (["Max", "mem.", "size"],RIGHT),
						     (["Max", "res.", "size"],RIGHT),
						     (["Real", "time"],RIGHT),
						     (["User", "time"],RIGHT), 
						     (["System", "time"],RIGHT)]

	   val performance_table_projects = mk_table 8 [(["Source"],LEFT), 
							(["Size", "of", "exec."],RIGHT),
							(["Size", "of", "stripped", "exec."],RIGHT),
							(["Max", "mem.", "size"],RIGHT),
							(["Max", "res.", "size"],RIGHT),
							(["Real", "time"],RIGHT),
							(["User", "time"],RIGHT), 
							(["System", "time"],RIGHT)]
	     
	   val timings = ref []
	     
	   val _ = ok_log_report ("Starting new strategy: " ^ strategy_name)
	    
	   val new_compile_strategy_dir = OS.Path.concat(new_target_program_dir(), strategy_name)
	   val _ = create_dir new_compile_strategy_dir

	   val _ = add_performance_strategy_test_report strategy

	   fun performance_test_file (filename,input_to_file) =
	     let
	       val filepath = OS.Path.joinDirFile{dir=source_directory(), file= filename}
	       val unitname = OS.Path.base filename
	       val _ = reset()
(*	       val log_filename = new_compile_strategy_dir ^ filename ^ ".log" *)
	                     (*source_directory () ^ filename ^ ".log"16/09/1996, Niels*)
	       val _ = ok_log_report ("Compiling ML source file: " ^ filename ^ ".")

	       val _ = (Manager.comp filepath;
			timings := (!timings) @ (Timing.get_timings())) 
		                  (* We only get timings, if the compilation has completed. *)
                       handle IO.Io {name=msg,...} => (raise Crash_test("Error: Compile Error:" ^ msg))
                       | X => 
		         (raise Crash_test "Error: Compile Error")
		     
	       val _ = ok_log_report ("Compiled " ^ filename ^ new_line)
	       val exe_file = unitname ^ ".exe"		
	       val new_out_datafile = OS.Path.joinDirFile{dir=new_compile_strategy_dir, file= unitname ^ ".out"}
	       val (max_mem_size: string, max_res_size: string,
		    real : string, user : string, sys  : string) =
		 let
		   val res = memtime (exe_file ^ " " ^ (gen_input_str_memtime input_to_file)) new_out_datafile
		 in
		   (#max_mem_size res, #max_res_size res, 
		    #real res, #user res, #sys res)
		 end

	       val size_of_exec = (Int.string ((size_of_file exe_file) div 1024)) ^ "K"
		 
	       val size_of_stripped_exec = (execute_shell_command ("strip " ^ exe_file);
					    (Int.string ((size_of_file exe_file) div 1024)) ^ "K")

	       val _ = insert_row performance_table_files [filename, size_of_exec, size_of_stripped_exec, 
							   max_mem_size, max_res_size, real, user, sys]
	     in
	       ()
	     end
	   handle Crash_test s => (error_log_report ("Error in performance_test_file: " ^ s ^ new_line ^ 
						     "Continue with next file.");
				   insert_row performance_table_files [filename, "Error", "Error", 
								       "Error", "Error", "Error", "Error", "Error"])

	   fun performance_test_project (project_name,input_to_project) =
	     let
	       val _ = change_directory (source_directory())
	       val _ = reset()

	       val _ = ok_log_report ("Compiling ML project " ^ project_name ^ ".")
	       val _ = 
		 let
		   val _ = evalProjects project_name
		   val new_timings = List.map (fn (s,list) => (project_name (* ^":"^(get_filename(s)) *),list)) (Timing.get_timings())
		 in
		   timings := (!timings) @ new_timings
		 end
	       handle X => (TextIO.output(TextIO.stdOut, "something happened.");raise Crash_test "Error: Compile Error")
		
	       val exe_file =  "run_project"
	       val new_out_datafile = OS.Path.joinDirFile{dir=new_compile_strategy_dir, file=OS.Path.base project_name ^ ".out"}
	       val (max_mem_size: string, max_res_size: string,
		    real : string, user : string, sys  : string) =
		 let
		   val res = memtime (exe_file ^ (gen_input_str_memtime input_to_project)) new_out_datafile
		 in
		   (#max_mem_size res, #max_res_size res, 
		    #real res, #user res, #sys res)
		 end
	       val size_of_exec = (Int.string ((size_of_file exe_file) div 1024)) ^ "K"
	       val size_of_stripped_exec = (execute_shell_command ("strip " ^ exe_file);
					    (Int.string ((size_of_file exe_file) div 1024)) ^ "K")
	       val _ = insert_row performance_table_projects [project_name, size_of_exec, size_of_stripped_exec, max_mem_size, max_res_size, real, user, sys]
	     in
	       ()
	     end
	   handle Crash_test s => (error_log_report ("Error in performance_test_project: " ^ new_line ^ s ^ new_line ^ "Continue with next file.");
				   insert_row performance_table_projects [project_name, "Error", "Error", "Error", "Error", "Error", "Error", "Error"])

	   fun show_timings() =
	     if show_compiler_timings then
	       (add_lines_test_report 
		["\\subsubsection{Compiler Timings -- " ^ strategy_name ^ "}"];
		(if List.isEmpty (!timings) then
		   add_line_test_report("No files compiled.")
		 else
		   let
(*old		     fun procent_time (SML_NJ.Timer.TIME{sec=sec1, usec=usec1}, SML_NJ.Timer.TIME{sec=sec2, usec=usec2}) =
		       ((real sec1) + (real usec1)/1000000.0) / ((real sec2) + (real usec2)/1000000.0) * 100.0
old*)
		     fun procent_time (t1, t2) = (Time.toReal t1 / Time.toReal t2) * 100.0
		     (* Layout of timings: [(filename, [{name,non_gc,system,gc,wallclock}])] *)
		     val _ = timings := (List.rev (!timings))
		     val headers = List.map (fn timing => #name(timing)) (#2(List.nth 0 (!timings)))
		     val headers = (["Source"],LEFT) :: (["Total"],RIGHT) :: (List.map (fn h =>  (split_sentence_into_words h,RIGHT)) headers)
		     val timing_table = mk_table (List.size headers) headers
		     val table_entries = List.foldL (fn (source_name, timing_list) => 
						     (fn acc => 
						      let
							val t = List.map #non_gc timing_list
							val totalTime = List.foldL (fn t => (fn total => Time.+(t,total))) 
							                  Time.zeroTime t
							val total = fix2 (Time.toString totalTime)
						      in
							(get_filename(source_name) :: total :: (List.map (*(fix2 o Time.toString)*)
										  (fn time => ((fix2 o Time.toString) time)^"/"^((fix2 o Real.toString) (procent_time(time,totalTime))))
										  t)) :: acc 
						      end)) [] (!timings)
		     val _ = List.apply (fn table_entry => insert_row timing_table table_entry) table_entries
		   in
		     add_lines_test_report
		     ["The timings shown is the time used by the ML Kit to compile the source programs",
		      "excluding garbage collection.",
		      "All timings are written (\\emph{time}/\\emph{pct}), where \\emph{time} is ",
	              "the time in seconds and \\emph{pct} is the fraction of time used in the phase in percent ",
		      "of the total compilation time."];
		     log_table timing_table;
		     latex_table timing_table
		   end))
	     else
	       ()

	   val script_name = OS.Path.joinDirFile{dir=kit_script_directory(), file=kit_script}
	   val _ = read_script script_name

	   val _ = change_directory (target_directory ())

	   (* Performance test on files. *)
	   val _ = add_lines_test_report ["\\subsubsection{Performance test on single source files}"]
	   val _ = timings := []
	   val _ = map performance_test_file performance_suite_files
	   val _ = log_table performance_table_files
	   val _ = latex_table performance_table_files
	   val _ = show_timings()

 	   (* Performance test on projects. *)
	   val _ = add_lines_test_report ["\\subsubsection{Performance test on projects}"]
	   val _ = timings := []
	   val _ = map performance_test_project performance_suite_projects
	   val _ = log_table performance_table_projects
	   val _ = latex_table performance_table_projects
	   val _ = show_timings()

	 in
	   ()
	 end
       handle Crash_test s => (add_error_test_report "Error in performance_test_strategy: " (s ^ ": Continue with next strategy.");
			       error_log_report ("Error in performance_test_strategy: " ^ s ^ new_line ^ "Continue with next strategy.")))
	| performance_test_strategy _ = raise Crash_test "PERFORMANCE STRATEGY called with wrong strategy."

    in
      (* This function performs the performance test. *)
      fun performance_test () = 
	let
	  val _ = add_lines_test_report
	        ["\\newpage",
		 "\\section{Performance Test}",
		 "This section contains the result of the performance test.",
		 "Execution times are in seconds.",
		 "Sizes of the executables are in bytes.",
		 "The following sections contains the result of each strategy applied to the performance test.",
		 "Column ``Max mem. size'' shows the total size of the process in kilobytes; this includes text, data, and stack.",
		 "Column ``Max res. size'' shows the resident size of the process in kilobytes; the resident size information is, at best, an approximate value.",
		 "A table entry ``Unknown'' in the ``Max mem. size'' and ``Max res. size'' columns means that",
		 "the target program had finished execution before program top noticed the target program.",
		 "Memory sizes and execution timings are found using the unix top and timex commands"]

	  val _ = ok_log_report (new_line ^ "*************Now starting PERFORMANCE test.***************")
	    
	  val _ = ensure_dir_exists (new_target_program_dir ())
	    
	  val _ = map performance_test_strategy (performance_strategies ())
	    
	in
	  ()
	end
    end (* local performance test. *)

    (*********************************************)
    (* Top level functions controlling the test. *)
    (*********************************************)
    local      
      (* This function checks that the test directory exists, 
         and a sub directory for this test can be created.   *)

      fun dir_test () =
	let
	  val _ = reset_test_report ()
	  val _ = ok_log_report (new_line ^ "*************Now starting ordinary directory test.***************")

	  val _ = if exists_directory (!test_env_directory) then
              	    ok_log_report("Test directory " ^ shorten_filename (!test_env_directory) ^ " exists.")
		  else
		    raise Crash_test ("Test directory: " ^ shorten_filename (!test_env_directory) ^ " does not exists.")

	  val _ = if exists_directory (source_directory ()) then
	            ok_log_report("Source directory " ^ shorten_filename (source_directory ()) ^ " exists.")
		  else
		    raise Crash_test ("Source directory: " ^ shorten_filename (source_directory ()) ^ " does not exists.")


	  val _ = if exists_directory (bin_directory ()) then
	            ok_log_report("Bin directory " ^ shorten_filename (bin_directory ()) ^ " exists.")
		  else
		    raise Crash_test ("Bin directory: " ^ shorten_filename (bin_directory ()) ^ " does not exists.")

(*KILL 17/10/1997 18:42. tho.:
	  val _ = 
	    if exists_file (new_version_dir ()) then 
	      ok_log_report  ("File or directory " ^ shorten_filename (new_version_dir ()) ^ " exists," ^ 
			      new_line ^ "and will be used by the test environment.")
	    else
	      (ok_log_report ("Directory " ^ shorten_filename (new_version_dir ()) ^ " will be created.");
	       ensure_dir_exists (new_version_dir ());
	       ())
*)
	  val _ = create_dir_and_maybe_rename_old (new_version_dir ())

	  val _ = test_report_stream := open_test_report()

	in
	  ()
	end
    in
      fun test () = 
	let
	  val _ = test_log_stream := (if (!test_log_string) = "std_out" then
				        TextIO.stdOut
				      else 
					TextIO.openOut(!test_log_string))

	  val _ = ok_log ("Using logfile: " ^ (!test_log_string))
	  val orig_dir = get_working_directory()

	  fun close_logfile () =
	    if (!test_log_string) <> "std_out" then
	      TextIO.closeOut (!test_log_stream)
	    else
	      ()
	in
	  let
	    (* Record state before we change any flags. *)
	    val state = Flags.get_state()

	    val _ = dir_test ()

	    val _ = add_lines_test_report []

	    val _ = if !acceptance_test_flag then
                      acceptance_test ()
		    else
		      ()

	    val _ = if !performance_test_flag then
                      performance_test ()
		    else
		      ()

	    val _ = export_test_report()
	    val _ = latex_test_report()

	    (* Reset state again. *)
	    val _ = Flags.reset_state state
	  in
	    (change_directory orig_dir; 
	     ok_log_report "Test finished.";
	     close_logfile ();
	     ())
	  end
        handle 
	  Crash_test s => (change_directory orig_dir; 
			   error_log s;
			   error_log "Test finished abnormally";
			   close_logfile ();
			   ())
	end
    end (* Local for test *)
    val _ = Flags.test_ref := test ;
              (*hack to allow fun `test' to appear on the interact menu*)
  end (* Struct *)




