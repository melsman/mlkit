signature SCS_FILE =
  sig
    (* [save source path] saves source into the file represented as path.
       If the directory path does not exist, then it is created. Raises ScsError.Fail
       on error. An already existing file i erased *)
    val save : quot -> string -> unit

    (* [mk_dir dir] creates directory dir - sub-directories are created if they do not
       already exist. Does nothing if dir already exists. Raises ScsError.Fail on error. *)
    val mkDir : string -> unit

    (* [uniqueFile dir] returns a filename (excluding dir) that is unique in directory dir. There is no guarantee that
       the file will remain unique (i.e., somebody else can create the file after the call to this function)
       Raises ScsError.Fail on error *)
    val uniqueFile : string -> string
  end

structure ScsFile :> SCS_FILE =
  struct
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
			   ScsError.raiseError `ScsFile.mkDir ^dir : file exits but it not a directory`
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
  end