structure Options =
    struct
	fun opt s = case explode s of 
	    #"-":: #"-"::rest => SOME (implode rest)
	  | #"-"::rest => SOME (implode rest)
	  | _ => NONE

	fun lookup_key key (l:(string *('a->unit))list) : ('a -> unit) option =
	    let fun look nil = NONE
		  | look ((x,f)::rest) = if key=x then SOME f else look rest
	    in look l
	    end

	fun read_options  {nullary:(string*(unit->unit))list,
			   unary:(string*(string->unit))list,
			   options: string list} : string list =
	    let	
		fun loop nil = nil
		  | loop (all as s::ss) =
		    case opt s of 
			SOME key => 	      
			    let 
				fun try_nullary exn =
				    case lookup_key key nullary of 
					SOME f => (f(); loop ss)
				      | NONE => raise Fail exn
			    in case lookup_key key unary of 
				SOME f => 
				    (case ss
					 of s::ss => (f s; loop ss)
				       | nil => try_nullary ("missing argument to " ^ s))
			      | NONE => try_nullary ("unknown option: " ^ s)
			    end
		      | NONE => all
	    in loop options
	    end
    end

signature COMP =
  sig
     val name : string
     val compile : {basisFiles: string list, 
		    source: string, 
		    namebase: string,     (* for uniqueness of type names, etc *)
		    target: string, 
		    flags: string} -> string
     val link : {target: string, lnkFiles: string list} -> string

     val mlbdir : unit -> string
     val objFileExt : unit -> string (* e.g., .o *)
  end

fun pp_list sep nil = ""
  | pp_list sep [x] = x 
  | pp_list sep (x::xs) = x ^ sep ^ pp_list sep xs

structure MLKitComp : COMP =
    struct
	val name = "mlkit"
	val default_mlkitexe = "/usr/bin/mlkit"
	fun mlkitexe() = 
	    case OS.Process.getEnv "MLB_MLKIT" of
		SOME exe => exe
	      | NONE => default_mlkitexe
	fun compile {basisFiles: string list, 
		     source: string, 
		     target: string,  (* file.o -> file.o.lnk, file.o, file.o.eb *)
		     namebase: string,
		     flags: string} : string =
	    let val s = mlkitexe() ^ " -c -namebase " ^ namebase ^ " -o " ^ target
		val s = if flags = "" then s else s ^ " " ^ flags
		val s = case basisFiles of nil => s | _ => s ^ " -load " ^ pp_list " " basisFiles
	    in s ^ " " ^ source
	    end
	fun link {target: string, lnkFiles: string list} : string =
	    mlkitexe() ^ " -o " ^ target ^ " -link " ^ pp_list " " lnkFiles

	fun mlbdir() = "MLB/MLKit"
	fun objFileExt() = ".o"
    end
	
(* [mlkit-mlb flags sources.mlb] Flags not recognized by mlkit-mlb are
 * sent to the compiler for each invocation. A directory MLB is
 * constructed for storing compilation and link information. *)

(* Idea: different combinations of "behavioral" options (e.g., 
 * -gc,-prof) could result in information being stored in different
 * subdirectories to MLB. *)


functor Mlb(C: COMP) : sig val build : string -> string -> unit end =
struct
    structure MlbProject = MlbProject()

    (* -------------------------------------------
     * Some operations on directories and files
     * ------------------------------------------- *)

    fun quot s = "'" ^ s ^ "'"

    fun error (s : string) = (print ("\nError: " ^ s ^ ".\n\n"); 
			      raise Fail "error")

    fun verbose() = true
    fun vchat s = if verbose() then print (" +++ " ^ s ^ "\n") else ()

    fun objFileFromSmlFile mlbfile smlfile = C.mlbdir () ^ "/" ^ mlbfile ^ "-" ^ smlfile ^ C.objFileExt()

    fun lnkFileFromSmlFile mlbfile smlfile = objFileFromSmlFile mlbfile smlfile ^ ".lnk"

    fun ebFileFromSmlFile mlbfile smlfile = objFileFromSmlFile mlbfile smlfile ^ ".eb"

    fun maybe_create_dir d : unit = 
      if OS.FileSys.access (d, []) handle _ => error ("I cannot access directory " ^ quot d) then
	if OS.FileSys.isDir d then ()
	else error ("The file " ^ quot d ^ " is not a directory")
      else ((OS.FileSys.mkDir d;()) handle _ => 
	    error ("I cannot create directory " ^ quot d ^ " --- the current directory is " ^ 
		   OS.FileSys.getDir()))

    fun maybe_create_mlbdir() =
      let val dirs = String.tokens (fn c => c = #"/") (C.mlbdir())
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
	      end handle OS.SysErr _ => error ("I cannot access directory " ^ quot dir)
      end

    (* --------------------
     * Build an mlb-project
     * -------------------- *)

    fun readDependencies uid = 
	let val is = TextIO.openIn (C.mlbdir() ^ "/" ^ uid ^ ".d") 
	in let val all = TextIO.inputAll is handle _ => ""
	       val uids = String.tokens Char.isSpace all
	       val _ = vchat ("Dependencies for " ^ uid ^ ": " ^ all)
	   in TextIO.closeIn is; uids
	   end handle _ => (TextIO.closeIn is; nil)
	end handle _ => nil

    fun recompileUnnecessary mlbfile smlfile : bool =
    (* f.sml<f.{lnk,eb} and forall g \in F(f.d) . g.eb < f.lnk *)
	let val _ = vchat ("Checking necessity of recompiling " ^ smlfile)
	    fun modTime f = OS.FileSys.modTime f
	    val op < = Time.<=
	    val lnkFile = lnkFileFromSmlFile mlbfile smlfile
	    val ebFile = ebFileFromSmlFile mlbfile smlfile
	    val modTimeSmlFile = modTime smlfile handle X => (vchat "modTime SMLfile"; raise X)
	    val modTimeEbFile = modTime ebFile handle X => (vchat ("modTime ebFile" ^ ebFile); raise X)
	    val modTimeLnkFile = modTime lnkFile handle X => (vchat "modTime lnkFile"; raise X)
	    fun debug(s,b) = (vchat(s ^ ":" ^ Bool.toString b); b)
	in debug("req1", modTimeSmlFile < modTimeLnkFile) andalso
	   debug("req2", modTimeSmlFile < modTimeEbFile) andalso
	   List.all (fn smlfile => debug("reqN", modTime (ebFileFromSmlFile mlbfile smlfile) < modTimeLnkFile))
	   (readDependencies smlfile)
	end handle _ => false

    fun system cmd : unit = 
	(vchat ("Executing command: " ^ cmd) ;
	let 
	    val status = OS.Process.system cmd
		handle _ => error ("Command failed: " ^ quot cmd)
	in if status = OS.Process.failure then
	    error ("Command failed: " ^ quot cmd)
	   else ()
	end
	 )
	
    fun build_mlb_one (flags:string) (mlbfile:string) (smlfile:string) : unit =
	if recompileUnnecessary mlbfile smlfile then ()
	else 
	let val _ = vchat ("Reading dependencies for " ^ smlfile)
	    val deps = readDependencies smlfile
	    val basisFiles = map (ebFileFromSmlFile mlbfile) deps
	    val cmd = C.compile {basisFiles=basisFiles,source=smlfile,
				 target=objFileFromSmlFile mlbfile smlfile,
				 namebase=mlbfile,
				 flags=flags}
	in system cmd
	end

    fun build flags mlbfile =
	let val _ = maybe_create_mlbdir()
	    val _ = vchat ("Building mlb-project\n");
	    val _ = vchat ("Updating dependencies...\n")
	    val _ = MlbProject.depDir := C.mlbdir()
	    val _ = MlbProject.dep mlbfile
	    val _ = vchat ("Finding sources...\n")		
	    val ss = MlbProject.sources mlbfile
		
	    val _ = vchat ("Compiling...\n")		
	    val _ = app (build_mlb_one flags mlbfile) ss

	    val _ = vchat ("Linking...\n")		
	    val lnkFiles = map (lnkFileFromSmlFile mlbfile) ss
	    val cmd = C.link {target="a.out", lnkFiles=lnkFiles}
	in system cmd
	end

end

structure Main : sig end =
struct
    structure MlbMLKit = Mlb(MLKitComp) 
(*    structure MlbMosml = Mlb(MosmlComp) *)

    fun error (s : string) = (print ("\nError: " ^ s ^ ".\n\n"); 
			      raise Fail "error")

    val date = Date.fmt "%b %d, %Y" (Date.fromTimeLocal (Time.now()))
    val version = "0.1"

    fun cmdName() = "mlbmake"
	    
    fun print_greetings comp =
	print(cmdName() ^ " version " ^ version ^ comp ^ ", " ^ date ^ "\n")

    fun print_usage() = print ("\nUsage: " ^ cmdName() ^ " {-mlkit|-mosml} [OPTION]*... [file.mlb] [COMPILER OPTION]*\n\n" ^
			       "Options:\n\n")

    val options = [("-mlkit", ["Use ML Kit as compiler."]),
		   ("-mosml", ["Use Moscow ML as compiler."]),
		   ("-version", ["Print version information and exit."]),
		   ("-help", ["Print help information and exit."])
		   ]

    fun print_indent nil = ()
      | print_indent (s::ss) = (print ("     " ^ s ^ "\n"); print_indent ss)
	
    fun print_options() = app (fn (t, l) => (print(t ^ "\n"); print_indent l; print "\n")) options
 
    val unary_options = nil

    val nullary_options =
	[("version", fn () => OS.Process.exit OS.Process.success),
	 ("help", fn () => (print_usage();
			    print_options();
			    OS.Process.exit OS.Process.success))]
	
    fun mlbmake (cmd,comp::args) = 
	(let val _ = print_greetings comp
	     val rest = Options.read_options{options=args, 
					     nullary=nullary_options,
					     unary=unary_options}
	     val build = 
		 case comp of
		     "-mlkit" => MlbMLKit.build
		   | _ => error ("compiler " ^ comp ^ " not supported")
	     val flags = ""
	 in case rest of
	    [mlbfile] => (build flags mlbfile; OS.Process.success) 
	  | _ => error "I expect exactly one mlb-file as argument"
	 end handle _ => OS.Process.failure)
      | mlbmake _ = OS.Process.failure

    fun install() =
	let 
	    fun arch_os() = 
		case SMLofNJ.SysInfo.getHostArch() ^ "-" ^ SMLofNJ.SysInfo.getOSName() of
		    "X86-Linux" => "x86-linux"
		  | "HPPA-HPUX" => "hppa-hpux"
		  | "X86-BSD" => "x86-bsd"
		  | s => s
	    val _ = print ("\n ** Exporting " ^ cmdName() ^ " executable **\n\n")
	    val kitbin_path = OS.Path.mkCanonical (OS.Path.concat(OS.FileSys.getDir(), "../../../bin"))
	    val bin_path = OS.Path.joinDirFile{dir=kitbin_path, file=cmdName()}		
	    val binimage_path = OS.Path.joinDirFile{dir=kitbin_path, file=cmdName() ^ "." ^ arch_os()}
	    val os = TextIO.openOut bin_path
	    val _ = (TextIO.output(os, "sml @SMLload=" ^ binimage_path ^ " " ^ " $*"); 
		     TextIO.closeOut os)
	    val _ = OS.Process.system("chmod a+x " ^ bin_path)
		handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ 
				   bin_path ^ "' failed***\n");
			     OS.Process.failure)
	in SMLofNJ.exportFn(bin_path, mlbmake)
	end
    
    val _ = install() handle exn => error (General.exnMessage exn)

end