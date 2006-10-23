
signature MLB_PLUGIN =
  sig
     val compile : {verbose:unit->bool} 
	 -> {basisFiles: string list, 
	     source: string, 
	     namebase: string,     (* for uniqueness of type names, etc *)
	     target: string,
       unique : int,
       lockfile : string option,
	     flags: string} 
	 -> Posix.Process.pid option
     val wait : Posix.Process.pid option -> Posix.Process.pid
     val link : {verbose:unit->bool} 
	 -> {mlbfile: string,
	     target: string, 
	     lnkFiles: string list, 
	     lnkFilesScripts: string list, 
	     flags: string} 
	 -> unit

(*     val flag_pp_bg : unit -> bool (* Should we print the dependency graph *) *)
     val getParallelN : unit -> int
     val mlbdir : unit -> string
     val objFileExt : unit -> string (* e.g., .o *)
     val maybeSetRegionEffectVarCounter : int -> bool
     val lnkFileConsistent : {lnkFile:string} -> bool	
  end

