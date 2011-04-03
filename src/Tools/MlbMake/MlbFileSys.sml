
structure MlbFileSys :> MLB_FILESYS =
  struct
    fun fromFile (filename:string) =
	let val is = TextIO.openIn filename 
	    (*val _ = TextIO.output(TextIO.stdOut, "opened " ^filename^"\n")*)
	    val s = TextIO.inputAll is handle E => (TextIO.closeIn is; raise E)
	in TextIO.closeIn is; s
	end
	
    fun err s = print ("\nError: " ^ s ^ ".\n\n"); 
    fun error (s : string) = (err s; raise Fail "error")	    
    fun errors (ss:string list) = (app err ss; raise Fail "error")
    fun quot s = "'" ^ s ^ "'"

    local
      val lib_list = ref [] : string list ref
      fun init () = lib_list := [(*case OS.Process.getEnv "PWD" of NONE =>*)
                                 OS.FileSys.getDir() (*| SOME d => d *)]
      fun conditionalInit () = case !lib_list of [] => init () | _ => ()
      fun getDir d =
          case List.getItem (List.rev d) of
	    SOME(h,r) => List.foldl (fn (x,y) => OS.Path.concat (y,x)) h r
          | NONE => error "getDir:impossible"
            
    in
      fun getCurrentDir () = (conditionalInit () ; getDir (!lib_list)) handle OS.Path.Path => error "getCurrentDir.Path"
      fun change_dir p : {cd_old : unit -> unit, file : string} =
	  let val {dir,file} = OS.Path.splitDirFile p
	  in if dir = "" then {cd_old = fn()=>(),file=file}
	     else let val _ = conditionalInit ()
	              val _ = OS.FileSys.chDir dir
		      (*    val _ = print ("Changed dir to: " ^ dir ^ "\n") *)
		      val old_dir = !lib_list
		      val _ = if OS.Path.isAbsolute dir
			      then lib_list := [dir]
			      else lib_list := dir :: old_dir
		  in {cd_old=fn()=>(OS.FileSys.chDir (getDir old_dir) handle OS.Path.Path => error "getDir.Path"
				  (* ; print ("Changed dir to: " ^ (getDir old_dir) ^ "\n") *)
				  ; lib_list := old_dir), file=file}
		  end handle OS.SysErr _ => error ("I cannot access directory " ^ quot dir)
	  end
    end
		 
    type unique = SysWord.word * SysWord.word
    fun cmp ((a,b),(c,d)) = 
	case SysWord.compare (a,c) of 
	    LESS => LESS
	  | GREATER => GREATER
	  | EQUAL => SysWord.compare (b,d)

    fun unique link f = 
	let val s = if link then Posix.FileSys.lstat f else Posix.FileSys.stat f
	in (Posix.FileSys.inoToWord(Posix.FileSys.ST.ino s),
	    Posix.FileSys.devToWord(Posix.FileSys.ST.dev s))
	end handle OS.SysErr(err,_) =>
                   raise Fail ("Failed to find info for file: " ^ f ^ "\nDetails: " ^ err ^ 
                               "\nCurrent directory: " ^ OS.FileSys.getDir())
  end

