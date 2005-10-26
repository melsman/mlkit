structure MLKitPlugIn : MLB_PLUGIN =
    struct
	val default_mlkitexe = "/usr/bin/mlkit"
	fun mlkitexe() = 
	    case OS.Process.getEnv "MLB_MLKIT" of
		SOME exe => exe
	      | NONE => default_mlkitexe
	fun compile {verbose} {basisFiles: string list, 
			       source: string, 
			       target: string,  (* file.o -> file.o.lnk, file.o, file.o.eb *)
			       namebase: string,
			       flags: string} : unit =
	    MlbUtil.system verbose
	    let val s = mlkitexe() ^ " -c -no_cross_opt -namebase " ^ namebase ^ " -o " ^ target
		val s = if flags = "" then s else s ^ " " ^ flags
		val s = case basisFiles of nil => s | _ => s ^ " -load " ^ MlbUtil.pp_list " " basisFiles
	    in s ^ " " ^ source
	    end
	fun link {verbose} {target: string, lnkFiles: string list, lnkFilesScripts=nil, flags:string} : unit =
	    MlbUtil.system verbose (mlkitexe() ^ " " ^ flags ^ " -o " ^ target ^ " -link " ^ MlbUtil.pp_list " " lnkFiles)
	  | link _ _ = raise Fail "MLKitPlugIn: lnkFilesScripts not empty"

	fun mlbdir() = "MLB/MLKit"
	fun objFileExt() = ".o"
	fun maybeSetRegionEffectVarCounter _ = false
	fun lnkFileConsistent lnkFile = true	
    end

structure BarryPlugIn : MLB_PLUGIN =
    struct
	val default_barryexe = "/usr/bin/barry"
	fun compile {verbose} {basisFiles: string list, 
			       source: string, 
			       target: string,  (* file.o -> file.o.lnk, file.o, file.o.eb *)
			       namebase: string,
			       flags: string} : unit =
	    MlbUtil.system verbose
	    let fun barryexe() = 
		  case OS.Process.getEnv "MLB_BARRY" of
		      SOME exe => exe
		    | NONE => default_barryexe
		val s = barryexe() ^ " -c -no_cross_opt -namebase " ^ namebase ^ " -o " ^ target
		val s = if flags = "" then s else s ^ " " ^ flags
		val s = case basisFiles of nil => s | _ => s ^ " -load " ^ MlbUtil.pp_list " " basisFiles
	    in s ^ " " ^ source
	    end
	fun link {verbose} {target: string, lnkFiles: string list, lnkFilesScripts=nil, flags: string} : unit = ()
	  | link _ _ = raise Fail "BarryPlugIn: lnkFilesScripts not empty"

	fun mlbdir() = "MLB/Barry"
	fun objFileExt() = ".b"
	fun maybeSetRegionEffectVarCounter _ = false
	fun lnkFileConsistent lnkFile = true	
    end

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

functor MlbStandAlone() : 
    sig 
	val cmdName : unit -> string
	val mlbmake : string * string list -> OS.Process.status
    end =
struct

    val verbose = ref false
    fun vchat0 s = if !verbose then print s else ()

    val oneSrcFile : string option ref = ref NONE
    structure MlbMLKit = MlbMake(structure P = MLKitPlugIn
				 val verbose = fn () => !verbose
				 val oneSrcFile = oneSrcFile) 

    structure MlbBarry = MlbMake(structure P = BarryPlugIn
				 val verbose = fn () => !verbose
				 val oneSrcFile = oneSrcFile) 

(*    structure MlbMosml = Mlb(MosmlComp) *)

    val error = MlbUtil.error

    val date = Date.fmt "%b %d, %Y" (Date.fromTimeLocal (Time.now()))
    val version = "0.2"

    fun cmdName() = "mlbmake"
	    
    fun greetings comp =
	cmdName() ^ " version " ^ version ^ comp ^ ", " ^ date ^ "\n"

    fun print_usage() = print ("\nUsage: " ^ cmdName() ^ " {-mlkit|-mosml} [OPTION]*... [file.mlb] [COMPILER OPTION]*\n\n" ^
			       "Options:\n\n")

    val options = [("-compiler {mlkit,barry}", ["Specify compiler to use (default: mlkit)."]),
		   ("-oneSrcFile s", ["Copy all sources to the file s and exit."]),
		   ("-version", ["Print version information and exit."]),
		   ("-help", ["Print help information and exit."])
		   ]

    fun print_indent nil = ()
      | print_indent (s::ss) = (print ("     " ^ s ^ "\n"); print_indent ss)
	
    fun print_options() = app (fn (t, l) => (print(t ^ "\n"); print_indent l; print "\n")) options
 
    val compiler = ref "mlkit"

    val unary_options =
	[("compiler", fn s => compiler := s),
	 ("oneSrcFile", fn s => oneSrcFile := SOME s)]

    fun show_version() =
	(print (greetings(!compiler)); OS.Process.exit OS.Process.success)

    val nullary_options =
	[("version", fn () => show_version()),
	 ("V", fn () => show_version()),
	 ("verbose", fn () => verbose := true),
	 ("v", fn () => verbose := true),	 
   	 ("help", fn () => (print (greetings(!compiler));
			    print_usage();
			    print_options();
			    OS.Process.exit OS.Process.success))]
	
    fun mlbmake (cmd,args) = 
	(let val rest = Options.read_options{options=args, 
					     nullary=nullary_options,
					     unary=unary_options}
	     val _ = vchat0(greetings(!compiler))
	     val build = 
		 case !compiler of
		     "mlkit" => MlbMLKit.build
		   | "barry" => MlbBarry.build
		   | comp => error ("compiler " ^ comp ^ " not supported")
	     fun sappend nil = ""
	       | sappend [x] = x
	       | sappend (x::xs) = x ^ " " ^ sappend xs
	 in case rest of
	    mlbfile :: flags => (build {flags=sappend flags,mlbfile=mlbfile,target="a.out"}; OS.Process.success) 
	  | _ => error "I expect exactly one mlb-file as argument"
	 end handle _ => OS.Process.failure)
end