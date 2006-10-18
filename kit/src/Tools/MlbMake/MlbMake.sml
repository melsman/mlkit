(*structure Listsort =
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
  end*)
(*
signature MLB_PLUGIN =
  sig
     val compile : {verbose:unit->bool} 
	 -> {basisFiles: string list, 
	     source: string, 
	     namebase: string,     (* for uniqueness of type names, etc *)
	     target: string,
       unique : int,
       lockfile : string,
	     flags: string} 
	 -> bool
     val link : {verbose:unit->bool} 
	 -> {mlbfile: string,
	     target: string, 
	     lnkFiles: string list, 
	     lnkFilesScripts: string list, 
	     flags: string} 
	 -> unit

     val mlbdir : unit -> string
     val objFileExt : unit -> string (* e.g., .o *)
     val maybeSetRegionEffectVarCounter : int -> bool
     val lnkFileConsistent : {lnkFile:string} -> bool	
  end
*)
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

signature MLB_MAKE =
    sig
	val build : {flags:string,mlbfile:string,target:string} -> unit 
(*
	val mlb_to_ulfile : (string->string list) 
	    -> {mlbfile:string} -> string
*)
    end

functor MlbMake(structure MlbProject : MLB_PROJECT
                structure MlbPlugIn: MLB_PLUGIN
		val verbose : unit->bool
		val oneSrcFile : string option ref)
    : MLB_MAKE =
struct
    structure P = MlbPlugIn

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

	fun lockFileFromSmlFile smlfile =
	    fileFromSmlFile smlfile ".lock"

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

    (* Recompilation is unnecessary if both  
     *
     *  (1) f.{sml,d}<f.lnk 
     *  (2) forall g \in F(f.d) . g.eb < f.lnk
     *
     * Assumption:
     *
     *  (A1) if f.sml is recompiled f.lnk is written to disk; f.eb need 
     *       not be newer than f.sml after compilation (i.e., if there 
     *       was an identical eb-file on disk already.)
     *
     *  (A2) The lnk-file is the last file being written during 
     *       compilation. 
     *)

    fun recompileUnnecessary mlbfile smlfile : bool =
    (* f.{sml,d}<f.lnk and forall g \in F(f.d) . g.eb < f.lnk *)
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
			andalso 
			reportBool(P.lnkFileConsistent {lnkFile=lnkFile}) "lnk-file not consistent"
		    end
		end
	    end
	end handle _ => false

    fun build_mlb_one (force : bool) (flags:string) (mlbfile:string) (smlfile:string) (unique:int) : Posix.Process.pid option option =
	if recompileUnnecessary mlbfile smlfile then NONE
(*	if recompileUnnecessary mlbfile smlfile then true *)
	else 
	let val _ = vchat ("Reading dependencies for " ^ smlfile)
	    val deps = readDependencies smlfile
	    val basisFiles = map ebFileFromSmlFile deps
	in SOME(P.compile {verbose=verbose} 
(*	in P.compile {verbose=verbose}  *)
	    {basisFiles=basisFiles,source=smlfile,
	     target=objFileFromSmlFile smlfile,
       unique = unique,
       lockfile = if force then NONE else SOME (lockFileFromSmlFile smlfile),
	     namebase=OS.Path.file mlbfile ^ "-" ^ OS.Path.file smlfile,
	     flags=flags})
	end

    fun map2 f ss = map (fn (x,y,a) => (f x,f y)) ss

    fun check_sources srcs_mlbs_ann =
	let (* first canonicalize paths *)
	    val srcs_mlbs = map2 OS.Path.mkCanonical srcs_mlbs_ann
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
		in del f; app w (ss ()); raise Exit
		end

    (* Rev: Region/Effect-variable ids *)
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

        structure Running :> 
           sig
             type R
             val empty : R
             val number : R -> int
             val add : R * (Posix.Process.pid * MlbProject.File) list -> R
             val wait : R * int option -> R * MlbProject.File list
           end =
           struct
             type R = (Posix.Process.pid,MlbProject.File) Binarymap.dict
             fun pidcmp (p1,p2) = SysWord.compare (Posix.Process.pidToWord p1,Posix.Process.pidToWord p2)
             val empty = Binarymap.mkDict pidcmp : R
             val number = Binarymap.numItems
             fun add (r,[]) = r
               | add (r,((p,s)::pr)) = case Binarymap.peek (r,p)
                                       of SOME _ => raise Fail "" 
                                        | NONE => add (Binarymap.insert(r,p,s),pr)
             fun done (r,p) = case Binarymap.peek (r,p)
                              of SOME _ => Binarymap.remove(r,p)
                               | NONE => raise Fail "MlbMake.Running: pid reported done, but I don't know pid"
             fun wait (running:R,max) =
                  let fun wait' (running:R,d) =
                      if number running = 0
                      then (running,d)
                      else
                        case max
                        of NONE =>
                               let
                             (*    val _ = print "Waiting NONE\n" *)
                                 val c = P.wait NONE
                                 val (running,b) = done (running,c)
                               in (running,b::d)
                               end
                         | SOME max =>
                             if number running >= max
                             then 
                               let 
                              (*   val _ = print "Waiting SOME\n" *)
                                 val c = P.wait NONE
                                 val (running,s) = done (running,c)
                               in
                                 wait'(running,s::d)
                               end
                             else (running,d)
                  in wait' (running,[])
                  end
           end

      structure Queue :>
        sig
          type 'a Q
          exception BadFile
          datatype 'a File = Go of 'a
                        | Problem of 'a
                        | Done
          val empty : (('a * 'a) -> order) -> ('a Q)
          val enqueue : 'a * ('a Q) -> ('a Q)
          val dequeue : ('a Q) -> ('a Q) * 'a File
          val enqueue_problem : 'a * ('a Q) -> 'a Q
        end =
        struct
          type 'a Q = 'a list * 'a list * 'a Binaryset.set
          exception BadFile
          datatype 'a File = Go of 'a
                        | Problem of 'a
                        | Done
          val empty = fn (cmp : ('a * 'a) -> order) => ([] : 'a list,[] : 'a list,Binaryset.empty cmp)
          fun enqueue_problem (e,(q1,q2,p)) = (
                                       if Binaryset.member (p, e)
                                       then raise BadFile 
                                       else (q1,e::q2,Binaryset.add(p, e)))
          fun enqueue (e,(q1,q2,p)) = (e::q1,q2,p)
          fun dequeue ([],[],p) = (([],[],p),Done)
            | dequeue ([],a::ar,p) = (([],ar,p),Problem a)
            | dequeue (a::ar,b,p) = ((ar,b,p),Go a)
        end

(*    fun build {flags, mlbfile, target} : unit =
	let val mlbfile = maybeWriteDefaultMlbFile mlbfile
	    val _ = vchat ("Finding sources...\n")		
	    val srcs_mlbs_anns_all' : (int * string * string * string list) list = 
		MlbProject.sources MlbProject.SRCTYPE_ALL mlbfile
      val srcs_mlbs_anns_all = List.map (fn (a,b,c,d) => (b,c,d)) srcs_mlbs_anns_all'
	    val _ = check_sources srcs_mlbs_anns_all
	    val srcs_all = map #1 srcs_mlbs_anns_all

	    val srcs_scriptsonly = 
		map #2 (MlbProject.sources MlbProject.SRCTYPE_SCRIPTSONLY mlbfile)
	    val srcs_allbutscripts =
		map #2 (MlbProject.sources MlbProject.SRCTYPE_ALLBUTSCRIPTS mlbfile)

	    val _ = maybeWriteOneSourceFile (fn () => srcs_all)

	    val _ = vchat ("Updating dependencies...\n")
	    val _ = maybe_create_mlbdir()
	    val _ = MlbProject.depDir := P.mlbdir()
	    val _ = MlbProject.dep mlbfile
		
	    val _ = vchat ("Compiling...\n")		
	    val _ = foldl (fn ((priority,src,mlb,anns),acc:{regionVar:int,unique:int}) =>
			   let 
			       (* val _ = print ("[Acc = " ^ Int.toString acc ^ "\n") *)
			       val b = P.maybeSetRegionEffectVarCounter (#regionVar acc)
			       val anns = MlbUtil.pp_list " " anns
			       val flags = anns ^ " " ^ flags
			       val _ = build_mlb_one flags mlb src (#unique acc)
			   in if b then {regionVar=readRevFromRevFile src,unique = (#unique acc) + 1}
                 else {regionVar=initialRev,unique =(#unique acc)+1}
			   end) {regionVar=initialRev,unique=1} srcs_mlbs_anns_all'

	    val _ = vchat ("Linking...\n")		
	    val lnkFiles = map lnkFileFromSmlFile srcs_allbutscripts
	    val lnkFilesScripts = map lnkFileFromSmlFile srcs_scriptsonly *)

    fun build {flags, mlbfile, target} : unit =
	let val mlbfile = maybeWriteDefaultMlbFile mlbfile
	    val _ = vchat ("Finding sources...\n")		
	    val srcs_mlbs_anns_all = MlbProject.sources mlbfile
	 (*   val _ = check_sources srcs_mlbs_anns_all *) (* This is checked in the creation of the BuildGraph *)

      val srcs_scriptsonly = MlbProject.fold (fn (MlbProject.Script sml,_,_,acc) => (MlbProject.Atom.toString sml) :: acc
                                               | (MlbProject.NonScript sml,_,_,acc) => acc) [] srcs_mlbs_anns_all

      val srcs_allbutscripts = MlbProject.fold (fn (MlbProject.NonScript sml,_,_,acc) => (MlbProject.Atom.toString sml) :: acc
                                               | (MlbProject.Script sml,_,_,acc) => acc) [] srcs_mlbs_anns_all

      fun strip (MlbProject.Script a) = a
        | strip (MlbProject.NonScript a) = a
	    val _ = maybeWriteOneSourceFile (fn () => MlbProject.fold (fn (s,_,_,acc) => (MlbProject.Atom.toString (strip s))::acc) [] srcs_mlbs_anns_all)

	    val _ = vchat ("Updating dependencies...\n")
	    val _ = maybe_create_mlbdir()
	    val _ = MlbProject.depDir := P.mlbdir()
	    val _ = MlbProject.dep mlbfile
			val setRegionEffectVarCounter = P.maybeSetRegionEffectVarCounter initialRev
      val maxN = let
                   val m = P.getParallelN ()
                 in
                   if m > 1 andalso setRegionEffectVarCounter 
                   then (vchat ("Parallel building and region profiling don't go together") ; 1)
                   else m
                 end

(*      val _ = if P.flag_pp_bg () 
              then print (MlbProject.pp_bg srcs_mlbs_anns_all)
              else () *)
		
	    val _ = vchat ("Compiling...\n")		

      val enqueue = Queue.enqueue (* (fn ((a,b,c),q) => Queue.enqueue ((a,b,c),q)) *)
      val fileToString = MlbProject.Atom.toString o strip o MlbProject.project

      type Acc = {regionVar:unit -> int,unique:int,children:Running.R, bg:MlbProject.BG, queue : ((MlbProject.File * string * string list) Queue.Q)}
      fun run' single sml (acc : Acc) : Acc = 
          let
            val (children,done) = case sml
                                  of SOME _ =>
                                      if single
                                      then Running.wait(#children acc, SOME 1)
                                      else Running.wait(#children acc, SOME maxN)
                                   | NONE => Running.wait (#children acc, NONE)
        (*    val _ = print ("Child DONE: " ^ (String.concat (List.map (fn (a) => MlbProject.Atom.toString a ^ " ") done)) ^ "\n") *)
            val (new,bg) = List.foldl (fn (s,(acc,bg)) =>
                                          let 
                                            val (new,bg) = MlbProject.done (s,bg)
      (* val _ = print ("DONE: " ^ (MlbProject.Atom.toString s) ^ " got: " ^ 
                     (String.concat (List.map (fn (a,_,_) => (MlbProject.Atom.toString (strip a) ^ " ")) new) ^ "\n")) *)
                                          in (new @ acc,bg)
                                          end) ([],#bg acc) done
            val queue = List.foldl enqueue (#queue acc) new
          in
            case sml
            of NONE => {regionVar = #regionVar acc, unique = (#unique acc), children = children, bg = bg,queue = queue}
             | SOME (file,mlb,anns) =>
                 let 
          (*  val _ =  print ("About to compile: " ^ (MlbProject.Atom.toString src) ^ "\n") *)
			      val flags = (MlbUtil.pp_list "" anns) ^ " " ^ flags
            val b = if setRegionEffectVarCounter 
                    then P.maybeSetRegionEffectVarCounter(#regionVar acc ())
                    else false
            val res = build_mlb_one single flags mlb (fileToString file) (#unique acc)
            val (children,queue,bg) = (case res
                               of NONE => let
                                            val (new,bg) = MlbProject.done (file,bg)  (* already compiled *)
     (* val _ = print ("DONE: " ^ (MlbProject.Atom.toString src) ^ " got: " ^ 
                     (String.concat (List.map (fn (a,_,_) => (MlbProject.Atom.toString (strip a) ^ " ")) new) ^ "\n")) *)
                                          in (children,List.foldl enqueue queue new,bg)
                                          end
                                | SOME NONE => (children,Queue.enqueue_problem ((file,mlb,anns),queue),bg)  (* couldn't get lock, lets save it for later *)
                                | SOME (SOME pid) => (Running.add (children,[(pid,file)]),queue,bg))   (* It is rolling *)
                                     handle BadFile => raise Fail ("Multiple locking errors for file: " ^ (fileToString file))
            in 
            {regionVar = if b then (fn f => fn () => readRevFromRevFile f) (fileToString file)
                         else fn () => initialRev, unique = (#unique acc) + 1, children = children, bg = bg,queue = queue}
            end
          end

      fun updateQueue nq (acc: Acc) : Acc =
                    {regionVar= #regionVar acc, unique = #unique acc, children = #children acc,
                     bg = #bg acc, queue = nq}

      fun run (acc:Acc) : Acc =
                    case Queue.dequeue (#queue acc)
                    of (_,Queue.Done) => if Running.number (#children acc) = 0 then acc else run(run' false NONE acc)
                     | (nq,Queue.Problem a) => run (run' true (SOME a) (updateQueue nq acc))
                     | (nq,Queue.Go a) => run (run' false (SOME a) (updateQueue nq acc))

      fun strip (MlbProject.Script a) = a
        | strip (MlbProject.NonScript a) = a
      val first = MlbProject.initial srcs_mlbs_anns_all
     (* val _ = print ("Initial: " ^ (String.concat (List.map (fn (a,_,_) => (MlbProject.Atom.toString (strip a) ^ " ")) first) ^ "\n")) *)
      val left = run {regionVar = fn () => initialRev, unique = 1, children = Running.empty, bg = srcs_mlbs_anns_all,
                      queue = List.foldl enqueue
                         (Queue.empty (fn ((a,_,_),(b,_,_)) => (MlbProject.Atom.compare (strip (MlbProject.project a), strip (MlbProject.project b))))) first}
      val _ = Running.wait (#children left,SOME 1)
	    val _ = vchat ("Linking...\n")
	    val lnkFiles = List.rev (map lnkFileFromSmlFile srcs_allbutscripts)
	    val lnkFilesScripts = List.rev (map lnkFileFromSmlFile srcs_scriptsonly)
	in P.link {verbose=verbose} 
	    {mlbfile=mlbfile,target=target,lnkFiles=lnkFiles,lnkFilesScripts=lnkFilesScripts,flags=flags}
	end handle Exit => () (*exit on purpose*)

end (* functor MlbMake *)
