signature SCS_FILE =
  sig

    (* [encodeFileNameUnix filename] returns filename with all "/"
        turned into "+" and "." turned into "%" which will prevent
        some unwanted behaviour on Unix file systems. *)
    val encodeFileNameUnix : string -> string

    (* [save source path] saves source into the file represented as path.
       If the directory path does not exist, then it is created. Raises ScsError.Fail
       on error. An already existing file i erased *)
    val save : quot -> string -> unit

    (* [mk_dir dir] creates directory dir - sub-directories are created if they do not
       already exist. Does nothing if dir already exists. Raises ScsError.Fail on error. *)
    val mkDir : string -> unit

    (* [uniqueFile dir] returns a filename (excluding dir) that is
       unique in directory dir. There is no guarantee that the file
       will remain unique (i.e., somebody else can create the file
       after the call to this function) Raises ScsError.Fail on error
       *)
    val uniqueFile : string -> string

   (* [ppFilesize size_in_bytes] return a string with the filesize
       pretty printed in bytes, kilobytes, megabytes or gigabytes
       depending on its size. *) 
    val ppFilesize : int -> string

    (* [systemCmd cmd] executes cmd and pipes the result into an
        temporary file. The content of that file is then read and
        returned. Raises Fail on error. *)
    val systemCmd : string -> string

    (* [cp source_file target_file] copies source_file into
        target_file. Raises Fail on error. *)
    val cp : string -> string -> unit

    (* [deleteAllFilesInDir dir] deletes all files in dir *)
    val deleteAllFilesInDir : string -> unit
  end

structure ScsFile :> SCS_FILE =
  struct

    fun encodeFileNameUnix file = 
      String.translate (fn #"/" => "+" | #"." => "%" | c => str c) file

    fun pp_syserr (s,NONE) = s
      | pp_syserr (s,SOME syserr) = s ^ " (Os.SysErr: " ^ (OS.errorMsg syserr) ^ ")"

    fun mkDir (dir:string) : unit =
      (List.foldl (fn (arc,acc) => 
		   let val d = acc^"/"^arc
		     in 
		       if FileSys.access (d,[]) then
			 if FileSys.isDir d
			   then d
			 else
			   ScsError.raiseError `ScsFile.mkDir ^dir : file exits but is not a directory`
		       else (FileSys.mkDir d; d)
		   end
		   handle OS.SysErr s => ScsError.raiseError `ScsFile.mkDir ^dir : ^(pp_syserr s)`)
      "" (#arcs(Path.fromString dir));())

    fun save source path =
      let
	val _ = mkDir (Path.dir path)
	val texstream = TextIO.openOut path
      in
	TextIO.output (texstream,Quot.toString source);
	TextIO.closeOut texstream
      end

    local 
      fun uniqueFile_ dir (c: int) : string =
	if c > 10 then raise ScsError.raiseError `ScsFile.uniqueFile ^dir : can't create unique file`
	else
	  let val is = Random.rangelist (97,122) (8, Random.newgen())
  	    val f = "file" ^ implode (map Char.chr is)
	  in 
	    if FileSys.access(dir ^ "/" ^ f,[]) then uniqueFile_ dir (c+1)
	    else f
	  end
	handle OS.SysErr s => ScsError.raiseError `ScsFile.uniqueFile ^dir : ^(pp_syserr s)`
    in
      fun uniqueFile dir = uniqueFile_ dir 10
    end

    fun ppFilesize size_in_bytes =
      let
	val ppReal = ScsReal.toString (ScsLogin.user_lang()) 1
	val b = Real.fromInt size_in_bytes
      in
	if b < 1024.0 then
	  Int.toString size_in_bytes ^ "b" 
	else
	  let
	    val kb = b / 1024.0
	  in
	    if kb < 1024.0 then
	      ppReal kb ^ "Kb"
	    else
	      let 
		val mb = kb / 1024.0
	      in
		if mb < 1024.0 then
		  ppReal mb ^ "Mb"
		else
		  let
		    val gb = mb / 1024.0
		  in
		    ppReal gb ^ "Gb"
		  end
	      end
	  end
      end

    fun systemCmd cmd =
      let
	val dir = ScsConfig.scs_tmp()
	val res_tmp_file = dir ^ "/" ^ (uniqueFile dir)
	val cmd = cmd ^ " > " ^ res_tmp_file
      in
	if Process.system cmd = Process.success then
	  let
	    val is = TextIO.openIn res_tmp_file
	    val res = TextIO.inputAll is
	    val _ = TextIO.closeIn is
	    val _ = FileSys.remove res_tmp_file
	  in
	    res
	  end
	else
	  raise Fail (Quot.toString `ScsFile.systemCmd: Can't execute system command: ^cmd`)
      end
    handle _ => raise Fail (Quot.toString `ScsFile.systemCmd: Can't execute system command: ^cmd`)

    fun cp source_file target_file =
      let
	val cmd = Quot.toString `^(ScsConfig.scs_cp()) ^(source_file) ^(target_file)`
      in
	if Process.system cmd = Process.success then
	  ()
	else
	  raise Fail (Quot.toString `ScsFile.cp: can't copy file ^(source_file) into ^(target_file)`)
      end

    fun deleteAllFilesInDir dir =
      let
	val ds = FileSys.openDir dir
	val _ = FileSys.chDir dir
	fun loop () =
	  case FileSys.readDir ds of
	    "" => ()
	  
	  | f  => ( (if FileSys.isDir f then () else FileSys.remove f);loop () )
      in
	loop();
	FileSys.closeDir ds
      end
  end
