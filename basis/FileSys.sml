(* FileSys -- 1995-06-16, 1995-09-25, 1996-05-01, 1996-10-13 *)

(* The preliminary OS structure defined here is 
 * hidden by a redeclaration later.. *)

structure OS = 
  struct
    type syserror = int
    exception SysErr of string * syserror option
    fun isNull (s : string) = prim("__is_null", s) : bool

    val _ = prim ("sml_setFailNumber", (SysErr ("as",NONE) : exn, 2 : int)) : unit
    fun errorMsg (err : int) : string = prim("sml_errormsg", err)
    fun errorName (err : int) : string =
         let
           val s = prim("sml_errorName", err : int) : string 
         in
           if isNull s
           then raise Fail ("OS.errorName: " ^ Int.toString err ^ " not a valid error number")
           else
             let 
               val a = String.map Char.toLower (Byte.unpackStringVec(Word8VectorSlice.slice((Byte.stringToBytes s),1,NONE)))
             in if a = "2big" then "toobig" else a
             end
         end
    fun syserror (err : string) : syserror option = 
         let
           val err = if err = "toobig" then "E2BIG" else "E" ^ (String.map Char.toUpper err)
           val s = prim("@sml_syserror", err : string) : int 
         in
           if s = ~1 then NONE else SOME s
         end
  end

structure FileSys : OS_FILE_SYS =
  struct

    (* The type of directory structures, as handled by the OS: *)
    type dirstruct_ = int          (* handle to directory structure; untagged abstract type --
				    * do not test values of this type for equality.. *)

    type file_id = {dev:int,ino:int}  (* with ordering *)
    infix <<
    fun ({dev,ino} : file_id) << ({dev=dev',ino=ino'} : file_id) : bool = 
      dev < dev' orelse (dev = dev' andalso ino < ino')

    (* Primitives from Runtime/IO.c -- raise Fail on error *)
    val failexn = Initial.FileSys.filesys_fail

    fun chdir_ (s : string) : unit =                     prim("sml_chdir", (s, failexn))
    fun remove_ (s : string) : unit =                    prim("sml_remove", (s, failexn))
    fun rename_ (s1 : string, s2 : string) : unit =      prim("sml_rename", (s1, s2, failexn))
    fun access_ (s : string, i : int) : bool =           prim("sml_access", (s, i, failexn))
    fun getdir_ () : string =                            prim("sml_getdir", failexn) 
    fun isdir_ (s : string) : bool =                     prim("sml_isdir", (s,failexn))
    fun mkdir_ (s : string) : unit =                     prim("sml_mkdir", (s, failexn))
    fun tmpnam_ (c: int) : string =
      if c > 10 then raise failexn
      else
	let val is = Random.rangelist (97,122) (8, Random.newgen())
  	    val f = "/tmp/file" ^ implode (map Char.chr is)
	in if access_(f,0) then tmpnam_(c+1)
	   else f
	end
    fun modtime_ (s : string) : real =                   prim("sml_modtime", (s, failexn))
    fun rmdir_ (s : string) : unit =                     prim("sml_rmdir", (s, failexn))
    fun settime_ (s : string, r : real) : unit =         prim("sml_settime", (s,r,failexn))
    fun filesize_ (s : string) : int =                   prim("sml_filesize", (s, failexn))
    fun opendir_ (s : string) : dirstruct_ =             prim("sml_opendir", (s, failexn))
    fun readdir_ (d : dirstruct_) : string =             prim("sml_readdir", d)
    fun rewinddir_ (d : dirstruct_) : unit =             prim("sml_rewinddir", d)
    fun closedir_ (d : dirstruct_) : unit =              prim("sml_closedir", (d, failexn))
    fun errno_ () : OS.syserror =                        prim("sml_errno", ())
    fun errormsg_ (err : OS.syserror) : string =         prim("sml_errormsg", err)
    fun mkerrno_ (i : int) : OS.syserror =               prim("id", i)
    fun islink_ (s : string) : bool =                    prim("sml_islink", (s, failexn))
    fun readlink_ (s : string) : string =                prim("sml_readlink", (s, failexn))
    fun realpath_ (s : string) : string =                prim("sml_realpath", (s, failexn))
    fun devinode_ (s : string) : file_id =               prim("sml_devinode", (s, failexn))
    fun int_to_word_ (i : int) : word =                  prim("id", i)

    fun isNull (s : string) = prim("__is_null", s) : bool


    fun formatErr mlOp (SOME operand) reason =
	mlOp ^ " failed on `" ^ operand ^ "': " ^ reason
      | formatErr mlOp NONE reason =
	mlOp ^ " failed: " ^ reason

    (* Raise SysErr from ML function *)
    fun raiseSysML mlOp operand reason =
	raise OS.SysErr (formatErr mlOp operand reason, NONE)

    (* Raise SysErr with OS specific explanation if errno <> 0 *)
    fun raiseSys mlOp operand reason =
	let val errno = errno_ ()
	in
	    if errno = 0 then raiseSysML mlOp operand reason
	    else raise OS.SysErr
		(formatErr mlOp operand (OS.errorMsg errno),
		 SOME (mkerrno_ errno))
	end

    type dirstream  = dirstruct_ option ref;
    datatype access_mode = A_READ | A_WRITE | A_EXEC;

    fun access (path, perm) =
	let fun mem p = if List.exists (fn q => p=q) perm then 1 else 0
	    val permcode = mem A_READ + 2 * mem A_WRITE + 4 * mem A_EXEC
	in
	    (access_ (path, permcode))
	    handle Fail s => raiseSys "access" (SOME path) s
	end;

    fun getDir () = (getdir_ ()) handle Fail s => raiseSys "getDir" NONE s;

    fun isDir p = (isdir_ p) handle Fail s => raiseSys "isDir" (SOME p) s;

    fun mkDir p = (mkdir_ p) handle Fail s => raiseSys "mkDir" (SOME p) s;

    fun chDir p = (chdir_ p) handle Fail _ => raiseSys "chDir" (SOME p) "chdir";

    fun mosmlFullPath p =
	let val links = ref 0
	    fun incrlink () =
		if !links < 30 then links := !links + 1
		else raise Fail "Too many symbolic links encountered"
	    open Path
	    fun expand p =
		let val {vol, arcs, isAbs} = Path.fromString p
		    val root = if isAbs then vol ^ "/" else vol
		in mkCanonical (List.foldl followlink root arcs) end
	    and followlink (a, p) =
		let val file = concat(p, a)
		in
		    if islink_ file then
			(incrlink();
			 expand(mkAbsolute{path=readlink_ file, relativeTo=p}))
		    else
			file
		end
	in
	    (expand(mkAbsolute{path=p, relativeTo=getDir()}))
	    handle Fail s => raiseSys "fullPath" (SOME p) s
	end;

    fun fullPath p =
      (realpath_ p)
      handle Fail "realpath not supported" => mosmlFullPath p
	   | Fail s => raiseSys "fullPath" (SOME p) s

    fun isLink p = (islink_ p) handle Fail s => raiseSys "isLink" (SOME p) s

    fun readLink p = (readlink_ p) handle Fail s => raiseSys "readLink" (SOME p) s

    fun fileId p : file_id = (devinode_ p) handle Fail s => raiseSys "fileId" (SOME p) s

    fun hash ({dev,ino} : file_id) = int_to_word_(19 * dev + ino)

    fun compare (fid1 : file_id, fid2) =
	if fid1 << fid2 then LESS
	else if fid2 << fid1 then GREATER
	else EQUAL

    fun realPath p =
	if Path.isAbsolute p then fullPath p
	else Path.mkRelative{path=fullPath p, relativeTo=getDir()};

    fun rmDir p = (rmdir_ p) handle Fail s => raiseSys "rmDir" (SOME p) s;

    fun tmpName () = (tmpnam_ 0) handle Fail s => raiseSys "tmpName" NONE s

    fun modTime p =
	(Time.fromReal (modtime_ p))
	handle Fail s => raiseSys "modTime" (SOME p) s;

    fun fileSize p =
	(filesize_ p)
	handle Fail s => raiseSys "fileSize" (SOME p) s;

    fun remove p =
	(remove_ p)
	handle Fail _ => raiseSys "remove" (SOME p) "unlink";

    fun rename {old, new} =
	(rename_ (old, new))
	handle Fail _ => raiseSys "rename" (SOME old) "rename";

    fun setTime (path, time) =
	let val tsec = Time.toReal (case time of NONE => Time.now() | SOME t => t)
	in
	    (settime_ (path, tsec))
	    handle Fail s => raiseSys "setTime" (SOME path) s
	end;

    fun openDir path =
	(ref (SOME (opendir_ path)))
	handle Fail s => raiseSys "openDir" (SOME path) s;

    fun readDir (ref NONE) =
	raiseSysML "readDir" NONE "Directory stream is closed"
      | readDir (arg as ref (SOME dstr)) =
	let val e' = readdir_ dstr
      val e = if isNull e' then NONE else SOME e'
	in
    Option.join(Option.map(fn entry => 
	    if entry <> Path.parentArc andalso entry <> Path.currentArc then
		SOME entry
	    else
		readDir arg) e)
	end

    fun rewindDir (ref NONE) =
	raiseSysML "rewindDir" NONE "Directory stream is closed"
      | rewindDir (ref (SOME dstr)) = rewinddir_ dstr;

    fun closeDir (ref NONE) = ()
      | closeDir (r as ref (SOME dstr)) =
	(r := NONE; closedir_ dstr);
  end
