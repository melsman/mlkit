
(* [mlkit-mlb flags sources.mlb] Flags not recognized by mlkit-mlb are
 * sent to the compiler for each invocation. A directory MLB is
 * constructed for storing compilation and link information. *)

(* Idea: different combinations of "behavioral" options (e.g.,
 * -gc,-prof) could result in information being stored in different
 * subdirectories to MLB. *)

signature MLB_MAKE =
    sig
        val build : {flags:string,mlbfile:string,target:string} -> unit
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

    fun die s = (print ("Die: MlbMake: " ^ s ^ "\n"); raise Fail s)

    fun vchat s = MlbUtil.vchat0 verbose s

    local
        fun fileFromSmlFile smlfile ext =
            let val {dir,file} = OS.Path.splitDirFile smlfile
                infix ##
                val op ## = OS.Path.concat
            in dir ## P.mlbdir () ## (file ^ ext)
            end handle OS.Path.Path => die "fileFromSmlFile. Path"
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

    fun maybe_create_mlbdir () =
      let val dirs = String.tokens (fn c => c = #"/") (P.mlbdir())
        fun append_path "" d = d
          | append_path p d = p ^ "/" ^ d
        fun loop (p, nil) = ()
          | loop (p, d::ds) = let val p = append_path p d
                              in maybe_create_dir p; loop(p, ds)
                              end
      in loop("", dirs)
      end

    fun path_concat dir file =
        (if OS.Path.isAbsolute file then file
         else OS.Path.concat(dir,file))
        handle OS.Path.Path => die "Path: path_concat"

    (* --------------------
     * Build an mlb-project
     * -------------------- *)

    fun readDependencies smlfile =
        let val dir = OS.Path.dir smlfile
            val depFile = depFileFromSmlFile smlfile
            val is = TextIO.openIn depFile
        in let val all = TextIO.inputAll is handle _ => ""
               val smlfiles = String.tokens Char.isSpace all
               val smlfiles = map (path_concat dir) smlfiles
               (*val _ = vchat ("Dependencies for " ^ smlfile ^ ": " ^ String.concatWith " " smlfiles)*)
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
(*                  reportBool(modTimeDepFile <= modTimeEbFile) "d-file newer than eb-file"
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

    fun build_mlb_one (force : bool) (flags:string) (mlbfile:string,mlbhash:string)
                      (smlfile:string) (unique:int) : Posix.Process.pid option option =
        if recompileUnnecessary mlbfile smlfile then NONE
        else
        let val _ = vchat ("Reading dependencies for " ^ smlfile)
            val deps = readDependencies smlfile
            val basisFiles = map ebFileFromSmlFile deps
            val chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            val hash = MD5.fromStringP chars (mlbhash ^ "-" ^
                                              OS.Path.file mlbfile ^ "-" ^
                                              (if OS.Path.isAbsolute smlfile
                                               then OS.Path.file smlfile
                                               else smlfile))
            val hash = if size hash > 0 andalso Char.isDigit (String.sub(hash,0))
                       then "h" ^ hash (* make sure namebase does not start with a digit *)
                       else hash
        in SOME(P.compile {verbose = verbose}
                          {basisFiles = basisFiles,
                           source     = smlfile,
                           target     = objFileFromSmlFile smlfile,
                           unique     = unique,
                           lockfile   = if force then NONE else SOME (lockFileFromSmlFile smlfile),
                           namebase   = hash ^ "-" ^ OS.Path.file mlbfile ^ "->" ^ OS.Path.file smlfile,
                           flags      = flags}
               )
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
                end handle OS.Path.Path => die "readRevFromRevFile.Path"
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
                                 of SOME _ => raise Fail "" (* exit with success *)
                                  | NONE => add (Binarymap.insert(r,p,s),pr)
      fun done (r,p) = case Binarymap.peek (r,p)
                        of SOME _ => Binarymap.remove(r,p)
                         | NONE => MlbUtil.error "MlbMake.Running: pid reported done, but I don't know pid"
      fun wait (running:R,max) =
          let fun wait' (running:R,d) =
                  if number running = 0 then (running,d)
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
                        if number running >= max then
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
      type 'a Q = 'a list * 'a list * 'a list * 'a Binaryset.set
      exception BadFile
      datatype 'a File = Go of 'a
                       | Problem of 'a
                       | Done
      val empty = fn (cmp : ('a * 'a) -> order) => ([] : 'a list, [] : 'a list, [] : 'a list,Binaryset.empty cmp)
      fun enqueue_problem (e,(q0,q1,q2,p)) = (
          if Binaryset.member (p, e) then raise BadFile
          else (q0,q1,e::q2,Binaryset.add(p, e)))
      fun enqueue (e,(q0,q1,q2,p)) = (e::q0,q1,q2,p)
      fun dequeue ([],[],[],p) = (([],[],[],p),Done)
        | dequeue ([],[],a::ar,p) = (([],[],ar,p),Problem a)
        | dequeue (q0,[],q2,p) = dequeue([],List.rev q0,q2,p)
        | dequeue (q0,a::ar,b,p) = ((q0,ar,b,p),Go a)
    end

    fun build {flags, mlbfile, target} : unit =
        let
          val mlbfile = maybeWriteDefaultMlbFile mlbfile
          val _ = vchat ("Finding sources and checking MLB file:" ^ mlbfile ^ " \n")
          val srcs_mlbs_anns_all = MlbProject.sources mlbfile
          (*  val _ = check_sources srcs_mlbs_anns_all *) (* This is checked in the creation of the BuildGraph *)

          val srcs_allbutscripts =
              MlbProject.fold
                  (fn (sml,_,_,acc) => MlbProject.Atom.toString sml :: acc)
                  [] srcs_mlbs_anns_all

          val _ = maybeWriteOneSourceFile (fn () => MlbProject.fold
                                                        (fn (s,_,_,acc) => MlbProject.Atom.toString s::acc) [] srcs_mlbs_anns_all
                                          )

          val _ = vchat ("Updating dependencies...\n")
          val _ = maybe_create_mlbdir()
          val _ = MlbProject.depDir := P.mlbdir()
          val _ = MlbProject.dep mlbfile
          val setRegionEffectVarCounter = P.maybeSetRegionEffectVarCounter initialRev
          val maxN =
              let
                val m = P.getParallelN ()
              in
                if m > 1 andalso setRegionEffectVarCounter then
                  (vchat ("Parallel building and region profiling don't go together") ; 1)
                else m
              end

(*
          val _ = if P.flag_pp_bg ()
              then print (MlbProject.pp_bg srcs_mlbs_anns_all)
              else ()
*)
          val _ = vchat ("Compiling...\n")

          val enqueue = Queue.enqueue
          val fileToString = MlbProject.Atom.toString o MlbProject.project

          type Acc = {regionVar:unit -> int,
                      unique:int,
                      children:Running.R,
                      bg:MlbProject.BG,
                      queue : ((MlbProject.File * (string * string) * string list) Queue.Q)}

          fun run' single sml (acc : Acc) : Acc =
              let
                val (children,done) =
                    case sml
                     of SOME _ =>
                        if single then Running.wait(#children acc, SOME 1)
                        else Running.wait(#children acc, SOME maxN)
                      | NONE => Running.wait (#children acc, NONE)
             (* val _ = print ("Child DONE: " ^ (String.concat (List.map (fn a => fileToString a ^ " ") done)) ^ "\n")  *)
                val (new,bg) =
                    List.foldl (fn (s,(acc,bg)) =>
                                   let
                                     val (new,bg) = MlbProject.done (s,bg)
                                   (*   val _ = print ("DONE: " ^ (fileToString s) ^ " got: " ^
                                                       (String.concat (List.map (fn (a,_,_) => (fileToString a ^ " ")) new) ^ "\n")) *)
                                   in (new @ acc,bg)
                                   end) ([],#bg acc) done
                val queue = List.foldl enqueue (#queue acc) new
              in
                case sml
                 of NONE => {regionVar = #regionVar acc,
                             unique = (#unique acc),
                             children = children,
                             bg = bg,
                             queue = queue}
                  | SOME (file,(mlb,mlbhash:string),anns) =>
                    let
                      (*   val _ =  print ("About to compile: " ^ (fileToString file) ^ "\n")  *)
                      val flags = (String.concatWith "" anns) ^ " " ^ flags
                      val b = if setRegionEffectVarCounter
                              then P.maybeSetRegionEffectVarCounter(#regionVar acc ())
                              else false
                      val res = build_mlb_one single flags (mlb,mlbhash) (fileToString file) (#unique acc)
                      val (children,queue,bg) =
                          (case res
                            of NONE => let
                                 val (new,bg) = MlbProject.done (file,bg)  (* already compiled *)
                               (*   val _ = print ("DONE: " ^ (fileToString file) ^ " got: " ^
                                                   (String.concat (List.map (fn (a,_,_) => (fileToString  a ^ " ")) new) ^ "\n")) *)
                               in (children,List.foldl enqueue queue new,bg)
                               end
                             | SOME NONE => (children,Queue.enqueue_problem ((file,(mlb,mlbhash),anns),queue),bg)  (* couldn't get lock, lets save it for later *)
                             | SOME (SOME pid) => (Running.add (children,[(pid,file)]),queue,bg))   (* It is rolling *)
                          handle BadFile => MlbUtil.error ("Multiple locking errors for file: " ^ (fileToString file))
                    in
                      {regionVar = if b then (fn f => fn () => readRevFromRevFile f) (fileToString file)
                                   else fn () => initialRev, unique = (#unique acc) + 1, children = children, bg = bg,queue = queue}
                    end
              end

          fun updateQueue nq (acc: Acc) : Acc =
              {regionVar= #regionVar acc,
               unique = #unique acc,
               children = #children acc,
               bg = #bg acc,
               queue = nq}

          fun run (acc:Acc) : Acc =
              case Queue.dequeue (#queue acc)
               of (_,Queue.Done) => if Running.number (#children acc) = 0 then acc else run(run' false NONE acc)
                | (nq,Queue.Problem a) => run (run' true (SOME a) (updateQueue nq acc))
                | (nq,Queue.Go a) => run (run' false (SOME a) (updateQueue nq acc))

          val first = MlbProject.initial srcs_mlbs_anns_all
          (* val _ = print ("Initial: " ^ (String.concat (List.map (fn (a,_,_) => (fileToString a ^ " ")) first) ^ "\n")) *)
          val left = run {regionVar = fn () => initialRev, unique = 1, children = Running.empty, bg = srcs_mlbs_anns_all,
                          queue = List.foldl enqueue
                                             (Queue.empty (fn ((a,_,_),(b,_,_)) =>
                                                              (MlbProject.Atom.compare (MlbProject.project a,
                                                                                        MlbProject.project b)))) first}
          val _ = Running.wait (#children left,SOME 1)
          val _ = vchat ("Linking...\n")
          val lnkFiles = List.rev (map lnkFileFromSmlFile srcs_allbutscripts)
        in P.link {verbose=verbose}
                  {mlbfile=mlbfile,target=target,lnkFiles=lnkFiles,flags=flags}
        end handle Exit => () (*exit on purpose*)

end (* functor MlbMake *)
