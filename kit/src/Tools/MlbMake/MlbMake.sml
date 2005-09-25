structure Listsort =
  struct
    fun sort ordr xs =
      let 
	fun merge []      ys = ys 
	  | merge xs      [] = xs
	  | merge (x::xs) (y::ys) =
	  if ordr(x, y) <> GREATER then x :: merge xs (y::ys)
	  else y :: merge (x::xs) ys
	fun mergepairs l1  []              k = [l1]
	  | mergepairs l1 (ls as (l2::lr)) k =
	  if k mod 2 = 1 then l1::ls
	  else mergepairs (merge l1 l2) lr (k div 2)
        fun nextrun run []      = (run, [])
          | nextrun run (xs as (x::xr)) =
	  if ordr(x, List.hd run) = LESS then (run, xs)
	  else nextrun (x::run) xr
        fun sorting []      ls r = List.hd(mergepairs [] ls 0)
          | sorting (x::xs) ls r =
	  let val (revrun, tail) = nextrun [x] xs
	  in sorting tail (mergepairs (List.rev revrun) ls (r+1)) (r+1) 
	  end
      in sorting xs [] 0 
      end
  end	

signature MLB_PLUGIN =
  sig
     val compile : {verbose:unit->bool} 
	 -> {basisFiles: string list, 
	     source: string, 
	     namebase: string,     (* for uniqueness of type names, etc *)
	     target: string, 
	     flags: string} 
	 -> unit
     val link : {verbose:unit->bool} 
	 -> {target: string, 
	     lnkFiles: string list, 
	     lnkFilesScripts: string list, 
	     flags: string} 
	 -> unit

     val mlbdir : unit -> string
     val objFileExt : unit -> string (* e.g., .o *)
     val maybeSetRegionEffectVarCounter : int -> bool
  end

structure MlbUtil =
    struct
	fun pp_list sep nil = ""
	  | pp_list sep [x] = x 
	  | pp_list sep (x::xs) = x ^ sep ^ pp_list sep xs
	    
	fun quot s = "'" ^ s ^ "'"
	    
	local
	    fun err s = print ("\nError: " ^ s ^ ".\n\n"); 
	in
	    fun error (s : string) = (err s; raise Fail "error")	    
	    fun errors (ss:string list) = 
		(app err ss; raise Fail "error")
	end
    
	fun vchat0 (verbose:unit -> bool) s = 
	    if verbose() then print (" ++ " ^ s ^ "\n") 
	    else ()
		
	fun system verbose cmd : unit = 
	    (vchat0 verbose ("Executing command: " ^ cmd) ;
	     let 
		 val status = OS.Process.system cmd
		     handle _ => error ("Command failed: " ^ quot cmd)
	     in if status = OS.Process.failure then
		 error ("Command failed: " ^ quot cmd)
		else ()
	     end
	     )
    end
	
(* [mlkit-mlb flags sources.mlb] Flags not recognized by mlkit-mlb are
 * sent to the compiler for each invocation. A directory MLB is
 * constructed for storing compilation and link information. *)

(* Idea: different combinations of "behavioral" options (e.g., 
 * -gc,-prof) could result in information being stored in different
 * subdirectories to MLB. *)


functor MlbMake(structure P: MLB_PLUGIN
		val verbose : unit->bool
		val oneSrcFile : string option ref)
    : sig val build : {flags:string,mlbfile:string,target:string} -> unit 
      end =
struct
    structure MlbProject = MlbProject()

    (* -------------------------------------------
     * Some operations on directories and files
     * ------------------------------------------- *)

    fun vchat s = MlbUtil.vchat0 verbose s

    local
	fun fileFromSmlFile smlfile ext =
	    let val {dir,file} = OS.Path.splitDirFile smlfile
		infix ##
		val op ## = OS.Path.concat
	    in dir ## P.mlbdir () ## (file ^ ext)
	    end
    in
	fun objFileFromSmlFile smlfile =
	    fileFromSmlFile smlfile (P.objFileExt())

	fun lnkFileFromSmlFile smlfile = 
	    objFileFromSmlFile smlfile ^ ".lnk"

	fun ebFileFromSmlFile smlfile = 
	    objFileFromSmlFile smlfile ^ ".eb"

	fun depFileFromSmlFile smlfile =
	    fileFromSmlFile smlfile ".d"

	fun revFileFromSmlFile smlfile = (* used when profiling is enabled for threading
					  * region/effect variable counter through 
					  * compilation. *)
	    fileFromSmlFile smlfile ".rev"
    end

    fun maybe_create_dir d : unit = 
      if OS.FileSys.access (d, []) handle _ => MlbUtil.error ("I cannot access directory " ^ MlbUtil.quot d) then
	if OS.FileSys.isDir d then ()
	else MlbUtil.error ("The file " ^ MlbUtil.quot d ^ " is not a directory")
      else ((OS.FileSys.mkDir d;()) handle _ => 
	    MlbUtil.error ("I cannot create directory " ^ MlbUtil.quot d ^ " --- the current directory is " ^ 
			   OS.FileSys.getDir()))

    fun maybe_create_mlbdir() =
      let val dirs = String.tokens (fn c => c = #"/") (P.mlbdir())
	fun append_path "" d = d
	  | append_path p d = p ^ "/" ^ d
	fun loop (p, nil) = ()
	  | loop (p, d::ds) = let val p = append_path p d
			      in maybe_create_dir p; loop(p, ds)
			      end
      in loop("", dirs)
      end

    fun change_dir p : {cd_old : unit -> unit, file : string} =
      let val {dir,file} = OS.Path.splitDirFile p
      in if dir = "" then {cd_old = fn()=>(),file=file}
	 else let val old_dir = OS.FileSys.getDir()
	          val _ = OS.FileSys.chDir dir
	      in {cd_old=fn()=>OS.FileSys.chDir old_dir, file=file}
	      end handle OS.SysErr _ => MlbUtil.error ("I cannot access directory " ^ MlbUtil.quot dir)
      end

    fun mkAbs file = OS.Path.mkAbsolute(file,OS.FileSys.getDir())

    fun subDir "" = (fn p => p)
      | subDir dir =
	let val dir_abs = mkAbs dir
	    in fn p =>
		let val p_abs = mkAbs p
		in OS.Path.mkRelative(p_abs,dir_abs)
		end
	end

    fun dirMod dir file = if OS.Path.isAbsolute file then file
			  else OS.Path.concat(dir,file)

    (* --------------------
     * Build an mlb-project
     * -------------------- *)

    fun readDependencies smlfile = 
	let val dir = OS.Path.dir smlfile
	    val depFile = depFileFromSmlFile smlfile
	    val is = TextIO.openIn depFile
	in let val all = TextIO.inputAll is handle _ => ""
	       val smlfiles = String.tokens Char.isSpace all
	       val smlfiles = map (dirMod dir) smlfiles   
	       val _ = vchat ("Dependencies for " ^ smlfile ^ ": " ^ MlbUtil.pp_list " " smlfiles)
	   in TextIO.closeIn is; smlfiles
	   end handle _ => (TextIO.closeIn is; nil)
	end handle _ => nil

    exception Recompile of string

    fun recompileUnnecessary mlbfile smlfile : bool =
    (* f.{sml,d}<f.{lnk,eb} and forall g \in F(f.d) . g.eb < f.lnk *)
	let fun rchat s = vchat ("Recompilation necessary: " ^ s)
	    fun mchat s = rchat ("cannot determine modification time of " ^ MlbUtil.quot s)
	    val _ = vchat ("Checking necessity of recompiling " ^ smlfile)
	    fun modTime f = OS.FileSys.modTime f
		handle X => (mchat f; raise X)
	    val op <= = Time.<=
	    val lnkFile = lnkFileFromSmlFile smlfile
	    val ebFile = ebFileFromSmlFile smlfile
	    val depFile = depFileFromSmlFile smlfile
	    val modTimeSmlFile = modTime smlfile 
	    val modTimeLnkFile = modTime lnkFile 
	    fun debug(s,b) = (vchat(s ^ ":" ^ Bool.toString b); b)
	    fun reportBool true s = true
	      | reportBool false s = (rchat s; false)
	in reportBool (modTimeSmlFile <= modTimeLnkFile) "sml-file newer than lnk-file"
	    andalso
	    let val modTimeEbFile = modTime ebFile 
	    in (*reportBool (modTimeSmlFile <= modTimeEbFile) "sml-file newer than eb-file"
		andalso *)
		let val modTimeDepFile = modTime depFile 
		in reportBool(modTimeDepFile <= modTimeLnkFile) "d-file newer than lnk-file"
		    andalso
(*		    reportBool(modTimeDepFile <= modTimeEbFile) "d-file newer than eb-file"
		    andalso *)
		    let val debs = readDependencies smlfile (* does not raise exception *)
		    in List.all (fn smlfile => 
				 let val ebFile = ebFileFromSmlFile smlfile
				 in reportBool(modTime ebFile <= modTimeLnkFile)
				     ("Dependent file " ^ MlbUtil.quot ebFile ^ " newer than lnk-file")
				 end) debs
		    end
		end
	    end
	end handle _ => false

    fun build_mlb_one (flags:string) (mlbfile:string) (smlfile:string) : unit =
	if recompileUnnecessary mlbfile smlfile then ()
	else 
	let val _ = vchat ("Reading dependencies for " ^ smlfile)
	    val deps = readDependencies smlfile
	    val basisFiles = map ebFileFromSmlFile deps
	in P.compile {verbose=verbose} 
	    {basisFiles=basisFiles,source=smlfile,
	     target=objFileFromSmlFile smlfile,
	     namebase=OS.Path.file mlbfile ^ "-" ^ OS.Path.file smlfile,
	     flags=flags}
	end

    fun map2 f ss = map (fn (x,y) => (f x,f y)) ss

    fun check_sources srcs_mlbs =
	let (* first canonicalize paths *)
	    val srcs_mlbs = map2 OS.Path.mkCanonical srcs_mlbs
	    fun report(s,m1,m2) =
		let val first = "The file " ^ MlbUtil.quot s ^ " is referenced "
		in if m1 = m2 then first ^ "more than once in " ^ MlbUtil.quot m1
		   else first ^ "in both " ^ MlbUtil.quot m1 
		       ^ " and " ^ MlbUtil.quot m2
		end
	    fun porder ((s1,m1),(s2,m2)) =
		if s1 < s2 then LESS
		else if s1 = s2 then
		        (if m1 < m2 then LESS
			 else if m1 = m2 then EQUAL
			      else GREATER)
		     else GREATER				
	    val srcs_mlbs = 
	      Listsort.sort porder srcs_mlbs
	    fun check ((s1,m1)::(s2,m2)::rest,acc) =
		check((s2,m2)::rest,
		      if s1 = s2 then (s1,m1,m2)::acc
		      else acc)
	      | check (_,nil) = ()
	      | check (_,acc) =	(MlbUtil.errors (map report acc))
	in check (srcs_mlbs,nil)
	end

    fun writeFile f c =
	let val os = TextIO.openOut f
	in (TextIO.output(os,c) before TextIO.closeOut os)
	    handle _ => TextIO.closeOut os
	end

    fun maybeWriteDefaultMlbFile mlbfile : string =
	case OS.Path.ext mlbfile of
	    SOME "sml" =>
		let val content =
		      "local open $(SML_LIB)/basis/basis.mlb in " 
		      ^ mlbfile ^ " end"
		    val file = mlbfile ^ ".mlb"
		in (writeFile file content; file)
		    handle _ => MlbUtil.error ("Failed to generate file " ^ file)
		end
	  | SOME "mlb" => mlbfile
	  | _ => MlbUtil.error "Expects file with extension '.mlb' or '.sml'"

    fun del f = OS.FileSys.remove f handle _ => ()

    exception Exit
    fun maybeWriteOneSourceFile ss =
	case !oneSrcFile of
	    NONE => ()
	  | SOME f => 
		let fun readAll s =
		      let val is = TextIO.openIn s
		      in TextIO.inputAll is
		      end
		    fun appendAll st =
			let val os = TextIO.openAppend f
			in TextIO.output(os,st)
			end
		    fun w s =
			let val st = readAll s
			in appendAll st
			end
		in del f; app w ss; raise Exit
		end

    val initialRev = 100
    fun readRevFromRevFile src =
	let infix ##
	    val op ## = OS.Path.concat
	    val revFile = 
		let val {dir,file} = OS.Path.splitDirFile src
		in dir ## P.mlbdir() ## (file ^ ".rev")
		end
	    fun err() = MlbUtil.error ("Failed to read threaded region/effect variable counter from file " 
				       ^ revFile ^ ", for profiling.")
	    val is = TextIO.openIn revFile
	in 
	    ((case String.tokens Char.isSpace (TextIO.inputAll is) of
		  [a,b] => let val _ = vchat ("First regvar/effvar used has id " ^ a ^ "; last has id " ^ b)
			       val (a,b) = (Option.valOf (Int.fromString a), 
					    Option.valOf (Int.fromString b))
			   in b
			   end
		| _ => err()) before TextIO.closeIn is)
   	    handle X => (TextIO.closeIn is; raise X)
	end

    fun build {flags, mlbfile, target} : unit =
	let val mlbfile = maybeWriteDefaultMlbFile mlbfile
	    val _ = vchat ("Finding sources...\n")		
	    val srcs_all = 
		let val srcs_mlbs_all : (string * string) list = 
		       MlbProject.sources MlbProject.SRCTYPE_ALL mlbfile
		    val _ = check_sources srcs_mlbs_all
		in
		    map #1 srcs_mlbs_all
		end

	    val srcs_scriptsonly = 
		map #1 (MlbProject.sources MlbProject.SRCTYPE_SCRIPTSONLY mlbfile)
	    val srcs_allbutscripts =
		map #1 (MlbProject.sources MlbProject.SRCTYPE_ALLBUTSCRIPTS mlbfile)

	    val _ = maybeWriteOneSourceFile srcs_all

	    val _ = vchat ("Updating dependencies...\n")
	    val _ = maybe_create_mlbdir()
	    val _ = MlbProject.depDir := P.mlbdir()
	    val _ = MlbProject.dep mlbfile
		
	    val _ = vchat ("Compiling...\n")		
	    val _ = foldl (fn (src,acc:int) =>
			   let 
			       (* val _ = print ("[Acc = " ^ Int.toString acc ^ "\n") *)
			       val b = P.maybeSetRegionEffectVarCounter acc
			       val () = build_mlb_one flags mlbfile src
			   in if b then readRevFromRevFile src else initialRev
			   end) initialRev srcs_all

	    val _ = vchat ("Linking...\n")		
	    val lnkFiles = map lnkFileFromSmlFile srcs_allbutscripts
	    val lnkFilesScripts = map lnkFileFromSmlFile srcs_scriptsonly
	in P.link {verbose=verbose} 
	    {target=target,lnkFiles=lnkFiles,lnkFilesScripts=lnkFilesScripts,flags=flags}
	end handle Exit => () (*exit on purpose*)
end (* functor MlbMake *)
