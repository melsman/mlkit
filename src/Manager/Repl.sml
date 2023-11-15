signature REPL = sig
  val run : unit -> OS.Process.status
end

functor Repl(structure ManagerObjects : MANAGER_OBJECTS
             structure IntModules : INT_MODULES
             sharing type ManagerObjects.IntBasis = IntModules.IntBasis
             sharing type ManagerObjects.modcode = IntModules.modcode
             structure Manager : MANAGER
                 where type PickleBases.modcode = ManagerObjects.modcode
                 where type PickleBases.Basis0 = ManagerObjects.Basis.Basis0
                 where type PickleBases.Basis1 = ManagerObjects.Basis.Basis1
                 where type PickleBases.name = Name.name
                 where type PickleBases.InfixBasis = ManagerObjects.InfixBasis
                 where type PickleBases.ElabBasis = ManagerObjects.ElabBasis
                 where type PickleBases.opaq_env = ManagerObjects.opaq_env
                 where type PickleBases.IntBasis = ManagerObjects.IntBasis
                 where type PickleBases.hce = Pickle.hce
                 where type PickleBases.funid = FreeIds.funid
                 where type PickleBases.sigid = FreeIds.sigid
                 where type PickleBases.longstrid = FreeIds.longstrid
                 where type PickleBases.longtycon = FreeIds.longtycon
                 where type PickleBases.longid = FreeIds.longid
            )
        : REPL =
struct

structure PE = ParseElab
structure ME = ModuleEnvironments
structure IB = InfixBasis
structure EB = ME.B
structure MO = ManagerObjects
structure IM = IntModules
structure B = MO.Basis
structure PP = PrettyPrint
structure ModCode = MO.ModCode
structure CEnv = CompilerEnv
structure Ty = StatObject.Type
structure L = LambdaExp
structure PB = Manager.PickleBases

type Basis = MO.Basis
type modcode = MO.modcode
type absprjid = ME.absprjid
type InfixBasis = MO.InfixBasis
type ElabBasis = MO.ElabBasis
type IntBasis = MO.IntBasis
type opaq_env = MO.opaq_env

(* -------------------------------------------
 * Logging and various utitlity functions
 * ------------------------------------------- *)

fun die (s:string) : 'a =
    (print("Error: " ^ s ^ "\n"); raise Fail ("Internal Error - Repl: " ^ s))
fun log (s:string) : unit = TextIO.output (!Flags.log, s)
fun chat s = if !Flags.chat then log (s ^ "\n") else ()
fun chatf f = if !Flags.chat then log (f() ^ "\n") else ()
fun pr_st (st) : unit = PP.outputTree (print, st, 120)

infix ##
val op ## = OS.Path.concat

fun mapi (f:int*'a->'b) (xs:'a list) : 'b list =
    List.rev (#2 (List.foldl (fn (x,(i,acc)) => (i+1,f(i,x)::acc)) (0,nil) xs))

(* -------------------------------------------
 * Debugging and reporting
 * ------------------------------------------- *)

val basislib_p = Flags.is_on0 "basislib"

val debug_p = Flags.is_on0 "debug_compiler"

fun debug s =
    if debug_p() then print("[REPL DEBUG: " ^ s ^ "]\n")
    else ()

val print_post_elab_ast = Flags.is_on0 "print_post_elab_ast"
val print_export_bases = Flags.is_on0 "print_export_bases"

fun maybe_print_topdec s topdec =
    if print_post_elab_ast() orelse debug_p() then
      let val _ = print (s ^ ":\n")
	  val st = PostElabTopdecGrammar.layoutTopdec topdec
      in pr_st st
      end
    else ()

val pretty_depth =
    Flags.add_int_entry {long="pretty_depth", short=NONE,
                         menu=["REPL Pretty Printing", "pretty depth"],
                         item=ref 5,
                         desc="This flag controls the pretty-printing depth of\n\
                              \values printed in the REPL. The value must be an\n\
                              \integer larger than zero."}

val pretty_string_size =
    Flags.add_int_entry {long="pretty_string_size", short=NONE,
                         menu=["REPL Pretty Printing", "pretty string size"],
                         item=ref 80,
                         desc="This flag controls the pretty-printing size of\n\
                              \string printed in the REPL. The value must be an\n\
                              \integer larger than zero."}

(* -------------------------------------------
 * Child Protocol - commands and replies
 * (see also src/Runtime/Repl.c)
 * ------------------------------------------- *)

local
  fun send_cmd (fd:Posix.FileSys.file_desc, cmd:string) : unit =
      let val cmd_bytes = Word8VectorSlice.full(Byte.stringToBytes cmd)
          val () = debug ("sending " ^ cmd)
      in if Posix.IO.writeVec(fd,cmd_bytes) <> Word8VectorSlice.length cmd_bytes then
           die ("send_cmd: failed to send whole cmd '" ^ cmd ^ "'")
         else ()
      end
in
  fun send_LOADRUN (fd, lib) : unit =
      send_cmd (fd, "LOADRUN " ^ lib ^ ";")

  fun send_PRINT (fd, ty, lab) : unit =
      send_cmd (fd, "PRINT \"" ^ ty ^ "\" " ^ lab ^ ";")

  fun send_SET (fd, key, value) : unit =
      send_cmd (fd, "SET " ^ key ^ " " ^ Int.toString value ^ ";")
end

datatype loadrun_msg = EXN | DONE

fun receive_EXN_or_DONE (fd:Posix.FileSys.file_desc) : loadrun_msg =
    let val msg_exn = "EXN;"
        val msg_done = "DONE;"
        fun try_msg msg s =
            let val m = Posix.IO.readVec (fd, size msg - size s)
                val s = s ^ Byte.bytesToString m
            in if size s < size msg_exn then try_msg msg s
               else if s = msg then NONE
               else SOME s
            end
    in case try_msg msg_exn "" of
           NONE => EXN
         | SOME s =>
           case try_msg msg_done s of
               NONE => DONE
             | SOME s => raise Fail ("receive_EXN_or_DONE: received '" ^ s
                                     ^ "' - expected '" ^ msg_exn
                                     ^ "' or '" ^ msg_done ^ "'")
    end

fun receive_DONE (fd:Posix.FileSys.file_desc) : unit =
    let val msg = "DONE;"
        fun loop s0 =
            let val m = Posix.IO.readVec (fd, size msg)
                val s = s0 ^ Byte.bytesToString m
            in if size s < size msg then loop s
               else if s = msg then ()
               else raise Fail ("receive_DONE: received '" ^ s
                                ^ "' - expected '" ^ msg ^ "'")
            end
    in loop ""
    end

fun receive_STR (fd:Posix.FileSys.file_desc) : string option =
    let fun eat msg s0 =
            let val m = Posix.IO.readVec (fd, size msg)
                val s = s0 ^ Byte.bytesToString m
            in if size s < size msg then eat msg s
               else if s = msg then ()
               else raise Fail ("receive_STR: received '" ^ s
                                ^ "' - expected '" ^ msg ^ "'")
            end
        fun read_int a =
            let val m = Posix.IO.readVec (fd, 1)
                val s = Byte.bytesToString m
            in if size s < 1 then read_int a
               else case Int.fromString s of
                        SOME i => read_int (a*10+i)
                      | NONE =>
                        if s = " " then a else
                        raise Fail ("receive_STR: received an '"
                                    ^ s ^ "'")
            end
        fun read_string i s0 =
            let val m = Posix.IO.readVec (fd, i)
                val s = s0 ^ Byte.bytesToString m
            in if size s < i then read_string (i-size s) s
               else if size s = i then s
               else raise Fail ("receive_STR: received more than asked for")
            end
        val () = eat "STR " ""
        val i = read_int 0
        val s = read_string i ""
        val () = eat ";" ""
    in SOME s
    end handle Fail msg =>
               ( print ("FAIL: " ^ msg ^ "\n")
               ; NONE)

(* -------------------------------------------
 * Value Rendering
 * ------------------------------------------- *)

type rp = {command_pipe : Posix.IO.file_desc,
           reply_pipe : Posix.IO.file_desc,
           pid : Posix.Process.pid,
           sessionid: string}

fun retrieve (rp:rp) B t longid =
    case MO.retrieve_longid B longid of
        MO.VAR lab =>
        ( send_PRINT (#command_pipe rp, t, "D." ^ lab);
          case receive_STR(#reply_pipe rp) of
              SOME s => s
            | NONE => "none" )
      | MO.STR s => s
      | MO.UNKN => "unknown"

local

  type tyvar = L.tyvar
  type Type = L.Type
  type TyName = TyName.TyName
  type TypeScheme = tyvar list * Type

  fun pp_Type t = PP.flatten1 (L.layoutType_repl t)

  fun pp_TypeScheme (nil,t) = pp_Type t
    | pp_TypeScheme (tvs,t) =
      "(" ^ String.concatWith "," (map L.pr_tyvar tvs) ^ ")." ^
      pp_Type t

  fun isArrow (_,t) =
      case t of
          L.ARROWtype _ => true
        | _ => false

  fun pp_crep (name:string, sch) =
      String.concat [name, ":", pp_TypeScheme sch]

  fun pp_boxity b =
      case b of
          TyName.ENUM => " (u)"
        | TyName.UNB_LOW => " (u)"
        | TyName.UNB_ALL => " (ua)"
        | TyName.SINGLE TyName.BOXED => " (u)"
        | TyName.BOXED => " (b)"
        | _ => die ("pp_boxity: " ^ TyName.pr_boxity b)

  fun pp_def (tn, creps) =
      String.concat [
        TyName.pr_TyName_repl tn
      , pp_boxity (TyName.boxity tn)
      , " = ["
      , String.concatWith "," (map pp_crep creps)
      , "]; "
      ]

  fun unions nil = TyName.Set.empty
    | unions (s::ss) = TyName.Set.union s (unions ss)

  fun tynames seen t =
      case t of
          L.TYVARtype _ => TyName.Set.empty
        | L.ARROWtype(ts,_,ts',_) =>
          unions (map (tynames seen) ts @
                  map (tynames seen) ts')
        | L.CONStype (ts,tn,_) =>
          let val s = unions (map (tynames seen) ts)
          in if TyName.Set.member tn seen then s
             else TyName.Set.insert tn s
          end
        | L.RECORDtype (ts,_) => unions (map (tynames seen) ts)

  fun tynames_crep seen (_,(_,t)) = tynames seen t

  fun tynames_defs seen ds =
      unions(map (fn (_,creps) =>
                     unions (map (tynames_crep seen) creps)) ds)

  fun defsClosure B tns_seen tns =
      let val tns = TyName.Set.difference tns tns_seen
      in case TyName.Set.fold (fn tn => fn acc =>
                                  case MO.tyname_reps B tn of
                                      SOME info => (tn, info) :: acc
                                    | NONE => acc
                              ) nil tns of
             nil => nil
           | defs =>
             let val tns_seen' = TyName.Set.union tns_seen tns
             in defs @ defsClosure B tns_seen'
                                   (tynames_defs tns_seen' defs)
             end
      end

  fun str_t B t =
      let val predefined = TyName.Set.fromList (TyName.tynamesPredefined)
          val defs = defsClosure B predefined (Ty.tynames t)
          val alldefs = String.concat (map pp_def defs)
          (* val () = print (alldefs ^ "\n") *)
      in SOME(alldefs ^ Ty.string_repl t)
      end
in
fun render rp B (strids,id,sch) =
    let val longid = Ident.implode_LongId(strids,id)
        val (tvs,rvs,t) = StatObject.TypeScheme.to_TyVars_and_Type sch
    in if List.null tvs then
         if Ty.is_Arrow t then "fn"
         else if Ty.eq(Ty.Unit, t) then "()"
         else case str_t B t of
                  SOME t => retrieve rp B t longid
                | NONE => ".."
       else "fun"
    end
end

(* --------------------------------------------------
 * Operations on dependencies
 * ------------------------------------------------- *)

datatype dep = SMLdep of string | MLBdep of string * string list

fun depToEb p =
    let val {dir,file} = OS.Path.splitDirFile p
        val dir = dir ## MO.mlbdir()
    in dir ## file ^ ".o.eb"
    end

(* When an mlb-file path/file.mlb is compiled, the contributing files
   are written to the file path/MLB/RI/file.mlb.df, where MLB/RI is
   the relevant mlbdir... The df-file indicates which basis files
   (.eb, .eb1) to load...
 *)
fun mlbfileToDfFile p =
    let val {dir,file} = OS.Path.splitDirFile p
        val dir' = dir ## MO.mlbdir()
    in (dir, dir' ## file ^ ".df")
    end

(* When an mlb-file path/file.mlb is compiled, a shared library
   is written to the file path/MLB/RI/file.mlb.so, where MLB/RI is
   the relevant mlbdir...
 *)
fun mlbfileToSoFile p =
    let val {dir,file} = OS.Path.splitDirFile p
        val dir' = dir ## MO.mlbdir()
    in (dir, dir' ## file ^ ".so")
    end

(* The trim below implements the optimisation that
              a + b + a = b + a
   for environments and bases (products of environments)
 *)

fun files_deps deps =
    let val deps = map (fn SMLdep f => [f]
                         | MLBdep (_,deps) => deps) (rev deps)
        val rdevs = rev (List.concat deps)
        val cwd = OS.FileSys.getDir()
        fun isin N y = List.exists (fn x => y = x) N
        fun trim (N:string list) nil acc = acc
          | trim N (x::xs) acc =
            let val y = OS.Path.mkAbsolute {path=x,relativeTo=cwd}
                val y = OS.Path.mkCanonical y
            in if isin N y then trim N xs acc
               else trim (y::N) xs (x::acc)
            end
    in trim nil rdevs nil
    end

fun read_df (mlbfile:string) : string list =
    let val (dir,dffile) = mlbfileToDfFile mlbfile
        val is = TextIO.openIn dffile
    in let val all = TextIO.inputAll is
           val smlfiles = String.tokens Char.isSpace all
           val smlfiles = map (fn f => if OS.Path.isAbsolute f then f
                                       else dir ## f) smlfiles
       in TextIO.closeIn is; smlfiles
       end
    end

(* --------------------------------------------------
 * Command management. Commands are prefixed with
 * a ':' and terminated with a ';'
 * ------------------------------------------------- *)

local

  fun pr "" = ()
    | pr s =
      let val s = "|" ^ String.translate (fn c => if c = #"\n" then "\n|" else str c) s
          val s = if String.isSuffix "|" s then
                    String.extract (s,0,SOME (size s-1))
                  else s
      in print s
      end

  fun string_to_nat s =
      if CharVector.all Char.isDigit s then
        Int.fromString s
      else NONE

  fun err s = print ("!" ^ s ^ "\n")

  fun mem s ss = List.exists (fn s' => s=s') ss
  fun ins a acc = if mem a acc then acc else a::acc

  fun is_flag_kind (f:string option -> bool) s =
      List.exists (fn opt => f(#kind opt)
                             andalso (mem s (#long opt) orelse mem s (#short opt)))
                  (Flags.getOptions_noneg())

  fun is_flag0 s = is_flag_kind (fn k => k = NONE) s
  fun is_flagN s = is_flag_kind (fn k => k = SOME "N") s
  fun is_flagS s = is_flag_kind (fn k => k = SOME "S") s
  fun is_flag s = is_flag_kind (fn k => true) s

  fun print_help () =
      pr ("The REPL accepts toplevel declarations and commands of the\n\
          \following forms:\n\n\
          \  cmd ::= :flags;           -- describe all flags\n\
          \        | :help;            -- general help\n\
          \        | :help flag;       -- get help about a flag\n\
          \        | :load mlb-file;   -- load an mlb-file\n\
          \        | :menu [N];        -- print flag menu [N]\n\
          \        | :quit;            -- quit\n\
          \        | :reset;           -- reset\n\
          \        | :search s;        -- search for help about s\n\
          \        | :set flag [arg];  -- set flag [maybe with arg]\n\
          \        | :unset flag;      -- unset the flag\n\n\
          \Notice that both toplevel declarations and commands must\n\
          \be terminated with a semi-colon. More flags are available\n\
          \from the command-line at REPL initialisation time.\n\
          \")

  fun menu_headings () =
      let val menus = List.map #menu (Flags.getOptions_noneg())
          val h1s = List.foldl (fn (nil,acc) => acc
                               | (a::_,acc) => ins a acc) nil menus
      in h1s
      end

  fun center w s =
      let val i = (w-size s) div 2
          val p = CharVector.tabulate (i,fn _ => #" ")
      in p ^ s
      end

  fun print_menuN s =
      case string_to_nat s of
          NONE => err ("No menu item " ^ s)
        | SOME n =>
          let val h1s = menu_headings()
              val h = List.nth (h1s,n-1)
              val H = "--- " ^ " (" ^ s ^ ") " ^ h ^ " ---"
              val () = pr (center Flags.menu_width H ^ "\n")
              val opts = List.filter (fn {menu=(h'::_),...} => h=h' | _ => false)
                                     (Flags.getOptions_noneg())
          in case List.concat (List.map #long opts) of
                 nil => err ("No help available for menu '" ^ s ^ "'")
               | ks => List.app (pr o Flags.help_nodash) ks
          end handle _ => err ("No menu item " ^ s)

  fun print_menu () =
      let val h1s = menu_headings()
          val (_,s) = List.foldl (fn (h,(i,acc)) =>
                                     (i+1,
                                      "  (" ^ Int.toString i ^ ") " ^ h ^ "\n" :: acc)) (1,nil)
                                 h1s
      in pr (String.concat (rev s))
      end

  fun print_flag_help k =
      if is_flag k then pr (Flags.help_nodash k)
      else err ("No flag '" ^ k ^ "'")

  fun print_search k =
      let val opts =
              List.filter (fn {long,short,desc,...} =>
                              List.exists (fn l => String.isSubstring k l) long
                              orelse List.exists (fn s => String.isSubstring k s) short
                              orelse mem k (String.tokens Char.isSpace desc)
                          )
                          (Flags.getOptions_noneg())
      in case List.concat (List.map #long opts) of
             nil => err ("No help available for '" ^ k ^ "'")
           | ks => List.app (pr o Flags.help_nodash) ks
      end

  fun print_flags () = print_search ""

  fun load stepno (rp:rp) libs_acc deps mlbfilepath =
      let val () = Manager.comp mlbfilepath
          (* Now, we create a new sofile that links the runtime and all the object files found in
           * the modcode for the compiled mlbfile - later we can keep track of already
           * linked objects, but for now, we just assume that the mlb-file does not import other
           * mlb-files... *)
          val modc =
              let val {dir,file} = OS.Path.splitDirFile mlbfilepath
                  val lnkfile = dir ## MO.mlbdir() ## (file ^ ".lnk")
              in PB.unpickleLnkFile lnkfile
              end handle _ => raise Fail ("Failed to load mlb-file")

          val punit = "stdin-" ^ #sessionid rp ^ "-" ^ Int.toString stepno
          val sofile = MO.mlbdir() ## ("lib" ^ punit ^ ".so")
          (* create a so-file with initialisation code as we do for sml-files *)
          val () = ModCode.mk_sharedlib (modc, ["runtime"], punit, sofile)
          (* memo: filter out those o-files that have already been loaded *)
          (* load the so-file *)
          val () = send_LOADRUN(#command_pipe rp,sofile)
      in case receive_EXN_or_DONE(#reply_pipe rp) of
             EXN =>
             ( err ("Loading '" ^ mlbfilepath ^ "' raised an exception")
             ; (stepno+1, libs_acc, deps) )
           | DONE =>
             let val defs = read_df mlbfilepath
                 (* val () = print ("Read defs: " ^ String.concatWith ", " defs ^ "\n") *)
                 val () = Flags.report_warnings()
             in (stepno+1, punit::libs_acc, MLBdep(mlbfilepath,defs)::deps)
             end
      end
      handle Manager.PARSE_ELAB_ERROR es =>
             ( pr(String.concat (map (fn ec => Manager.ErrorCode.pr ec ^ "\n") es))
             ; (stepno, libs_acc, deps)
             )
           | Report.DeepError r =>
             ( Report.print' r (!Flags.log)
             ; (stepno, libs_acc, deps)
             )
           | Fail msg =>
             ( err msg
             ; (stepno+1, libs_acc, deps)
             )
in

fun maybe_set_rtflag (rp:rp) k n =
    if k = "pretty_depth" orelse k = "pretty_string_size" then
      if n > 0 then
        ( Flags.lookup_int_entry k := n
        ; send_SET (#command_pipe rp, k, n)
        ; receive_DONE (#reply_pipe rp)
        ; true)
      else
        ( err ("Flag '" ^ k ^ "' expects an interger larger than 0")
        ; true
        )
    else false

exception Quit
fun process_cmd rt_exe stepno state (rp:rp) (cmd:string) libs_acc deps =
    let val ts = String.tokens Char.isSpace cmd
    in case ts of
           [":set", s] =>
           ( if is_flag0 s then Flags.turn_on s
             else err ("Invalid flag '" ^ s ^ "'")
           ; (stepno, state, rp, libs_acc, deps)
           )
         | [":set", s, a] =>
           ( if is_flagN s then
               case string_to_nat a of
                   SOME n => if maybe_set_rtflag rp s n then ()
                             else Flags.lookup_int_entry s := n
                 | NONE => err ("Flag '" ^ s ^ "' expects a positive integer - got '"
                                ^ a ^ "'")
             else if is_flagS s then
               Flags.lookup_string_entry s := a
             else err ("Flag '" ^ s ^ "' is not a flag accepting an argument!")
           ; (stepno, state, rp, libs_acc, deps)
           )
         | [":unset", s] =>
           ( if is_flag0 s then Flags.turn_off s
             else err ("Invalid flag '" ^ s ^ "'")
           ; (stepno, state, rp, libs_acc, deps)
           )
         | [":help"] => ( print_help(); (stepno, state, rp, libs_acc, deps) )
         | [":help", fl] => ( print_flag_help fl; (stepno, state, rp, libs_acc, deps) )
         | [":menu"] => ( print_menu(); (stepno, state, rp, libs_acc, deps) )
         | [":menu", n] => ( print_menuN n; (stepno, state, rp, libs_acc, deps) )
         | [":search", s] => ( print_search s; (stepno, state, rp, libs_acc, deps) )
         | [":flags"] => ( print_flags(); (stepno, state, rp, libs_acc, deps) )
         | [":load", f] =>
           let val (stepno, libs_acc, deps) = load stepno rp libs_acc deps f
           in (stepno, state, rp, libs_acc, deps)
           end
         | [":quit"] => raise Quit
         | [":reset"] =>
           let val pid = #pid rp
               val () = ( Posix.Process.kill(Posix.Process.K_PROC pid, Posix.Signal.kill)
                        ; Posix.Process.wait()
                        ; ())
               val (stepno, state, rp, libs_acc, deps) = initialise_repl state rt_exe
           in (stepno, state, rp, libs_acc, deps)
           end
         | _ => ( err ("Invalid command '" ^ cmd ^ "'")
                ; (stepno, state, rp, libs_acc, deps) )
    end

  and initialise_repl parserstate (rt_exe:string) =
    let
      (* Now, start the runtime executable in a child process,
         but first create two named pipes *)
      val sessionid =
          let fun loop i =
                  let val f = MO.mlbdir() ## "command_pipe-" ^ Int.toString i
                  in if OS.FileSys.access(f, [OS.FileSys.A_READ]) then loop (i+1)
                     else Int.toString i
                  end
          in loop 0
          end
      val command_pipe_name = MO.mlbdir() ## "command_pipe-" ^ sessionid
      val reply_pipe_name = MO.mlbdir() ## "reply_pipe-" ^ sessionid
      val repl_logfile = MO.mlbdir() ## "repl-" ^ sessionid ^ ".log"
      val () = Posix.FileSys.mkfifo (command_pipe_name, Posix.FileSys.S.irwxu)
      val () = Posix.FileSys.mkfifo (reply_pipe_name, Posix.FileSys.S.irwxu)
      val childpid =
          case Posix.Process.fork() of
              SOME pid => pid
            | NONE =>
              ( Posix.Process.execp (rt_exe, [OS.Path.file rt_exe,
                                              "-command_pipe", command_pipe_name,
                                              "-reply_pipe", reply_pipe_name,
                                              "-repl_logfile", repl_logfile])
              ; OS.Process.exit OS.Process.failure (* never gets here *)
              )
      val () = debug "created fifos"
      val command_pipe = Posix.FileSys.openf(command_pipe_name,
                                             Posix.FileSys.O_WRONLY,
                                             Posix.FileSys.O.flags[])
                         handle _ => die "run: failed to open command_pipe"
      val () = debug "opened command_pipe"
      val reply_pipe = Posix.FileSys.openf(reply_pipe_name,
                                           Posix.FileSys.O_RDONLY,
                                           Posix.FileSys.O.flags[])
                       handle _ => die "run: failed to open reply_pipe"
      val () = debug "opened reply_pipe"
      val rp = {command_pipe=command_pipe,reply_pipe=reply_pipe,pid=childpid,sessionid=sessionid}
      val init = (0, parserstate, rp, ["runtime"], nil)
      val init = if basislib_p() then
                   let val (stepno,state,rp,libs_acc,deps) = init
                       val cmd = ":load " ^ (!Flags.install_dir ## "basis/repl.mlb")
                       val (stepno,state,rp,libs_acc,deps) = process_cmd rt_exe stepno state rp cmd libs_acc deps
                   in (stepno,state,rp,libs_acc,deps)
                   end
                 else ( print ("!Basis Library and Pretty Printing not loaded!\n")
                      ; init )

      val () = if pretty_depth() <= 0 orelse pretty_string_size() <= 0 then
                 die "Only positive values for 'pretty_depth' and 'pretty_string_size' are supported"
               else
                 ( maybe_set_rtflag rp "pretty_depth" (pretty_depth())
                 ; maybe_set_rtflag rp "pretty_string_size" (pretty_string_size())
                 ; ()
                 )
    in init
    end

end

(* -------------------------------------------
 * The REPL
 * ------------------------------------------- *)

fun do_exit (rp:rp) status =
    ( print "Exiting\n"
    ; Posix.Process.kill (Posix.Process.K_PROC (#pid rp), Posix.Signal.kill)
    ; Posix.Process.wait()
    ; OS.Process.exit status
    )

fun repl (rt_exe, stepno, state, rp:rp, libs_acc, deps:dep list) : OS.Process.status =
    let val (stepno,state,libs_acc,deps,rp) =
            let fun loop stepno state libs_acc deps rp =
                    case PE.colonLine (PE.stripSemiColons state) of
                        SOME(cmd,state) =>
                        let val (stepno, state, rp, libs_acc, deps) = process_cmd rt_exe stepno state rp cmd libs_acc deps
                        in loop stepno state libs_acc deps rp
                        end
                      | NONE => (stepno, state, libs_acc, deps, rp)
            in loop stepno state libs_acc deps rp
            end
        val absprjid = ME.mk_absprjid "repl"
        val smlfile = "stdin-" ^ #sessionid rp ^ "-" ^ Int.toString stepno

        (* load the bases that smlfile depends on *)
        val ebfiles = map depToEb (files_deps deps)
        val (unpickleStream, elabBasesInfo) = PB.unpickleBases0 ebfiles
        val initialBasis0 = B.initialBasis0()
        val (infB,elabB) =
            List.foldl (fn ({infixElabBasis,...}, acc) =>
                           B.plusBasis0(acc,infixElabBasis))
                       initialBasis0
                       elabBasesInfo

        val _ = Flags.reset_warnings ()
        val _ = Name.bucket := []
        val base = absprjid
        val _ = Name.baseSet base
    in case PE.parse_elab_stdin {infB=infB, elabB=elabB,
                                 absprjid=absprjid, state=state} of
           (SOME state', PE.SUCCESS {doreport,infB=infB',elabB=elabB',topdec}) =>
           let val () = debug "elaboration succeeded"
               val _ = chat "[finding free identifiers begin...]"
               val freelongids =
                   let val intinfrep = StrId.mk_LongStrId ["IntInfRep"]
                   in PB.add_longstrid intinfrep (FreeIds.fid_topdec topdec)
                   end
               val _ = chat "[finding free identifiers end...]"

                val _ = chat "[computing actual dependencies begin...]"
                val ebfiles_actual = PB.compute_actual_deps
                    (#2 initialBasis0) elabBasesInfo freelongids
                val ebfiles_actual = map (fn x => x ^ "1") ebfiles_actual
                val _ = chat "[computing actual dependencies end...]"

                val (B_im,_) =
                    let val (opaq_env,intB) =
                        PB.unpickleBases1 unpickleStream ebfiles_actual
                        val B = B.mk(infB,elabB,opaq_env,intB)
                    in B.restrict(B,freelongids)
                    end
                val (_,_,oE_im,intB_im) = B.un B_im

                (* Setting up for generation of second export basis (eb1) *)
               val names_elab = !Name.bucket
               val _ = List.app Name.mk_rigid names_elab
               val _ = Name.bucket := []
               val _ = Name.baseSet (base ^ "1")

               val _ = chat "[opacity elimination begin...]"
               val (topdec', oE') = OpacityElim.opacity_elimination(oE_im, topdec)
               val _ = chat "[opacity elimination end...]"

	       val _ = maybe_print_topdec "AST after opacity elimination" topdec

               val _ = chat "[interpretation begin...]"
               val functor_inline = true
               val (intB', modc) = IM.interp(functor_inline, absprjid, intB_im, topdec', smlfile)
               val names_int = !Name.bucket
               val _ = List.app Name.mk_rigid names_int
               val _ = Name.bucket := []
               val _ = chat "[interpretation end...]"

               (* compute result basis *)
               val B' = B.mk(infB',elabB',oE',intB')

               val _ =
                   if print_export_bases() then
                     (  print ("[Export basis for " ^ smlfile ^ " before closure:]\n")
                      ; pr_st (MO.Basis.layout B')
                      ; print "\n")
                   else ()

               (* create a shared library *)
               val modc = ModCode.emit (absprjid,modc)
               val sofile = MO.mlbdir() ## ("lib" ^ smlfile ^ ".so")
               val () = ModCode.mk_sharedlib (modc, libs_acc, smlfile, sofile)

               (* Write closed export bases to disk if identical export bases are
                * not already saved *)
               val B'Closed = B.closure (B_im,B')
               val () =
                   let val (b1,b2,b3,b4) = B.un B'Closed
                       val NB0' = (names_elab, (b1,b2))
                       val NB1' = (names_int, (b3,b4))
                       val smlfile = MO.mlbdir() ## smlfile
                       val ofile = smlfile ^ ".o"
                   in PB.pickleNB smlfile ofile (NB0',NB1')
                   end

               val () = Flags.report_warnings()

               (* send command to load and run the shared library *)
               val () = send_LOADRUN(#command_pipe rp,sofile)

           in case receive_EXN_or_DONE(#reply_pipe rp) of
                  EXN =>
                  ( repl (rt_exe,
                          stepno+1,          (* No reporting! *)
                          PE.begin_stdin(),  (* Clear the state *)
                          rp,
                          smlfile::libs_acc, (* Code may be saved in mutable objects before exn is raised *)
                          deps)              (* No contribution to static basis *)
                  )
                | DONE =>
                  ( Report.print (doreport (SOME (render rp (B.plus(B_im, B'Closed)))))
                  ; repl (rt_exe,
                          stepno+1,
                          state',
                          rp,
                          smlfile::libs_acc,
                          SMLdep smlfile :: deps)
                  )
           end
         | (SOME state', PE.FAILURE (report,errs)) =>
           ( Report.print report
           ; repl (rt_exe,
                   stepno+1,
                   PE.begin_stdin(),   (* Clear the state *)
                   rp,
                   libs_acc,
                   deps)
           )
         | (_, PE.FAILURE (report,errs)) =>   (* some syntax errors end here, so don't exit unless eof... *)
           ( Report.print report
           ; if List.exists (fn e => PE.ErrorCode.eq(e,PE.ErrorCode.error_code_eof)) errs then
               do_exit rp OS.Process.failure
             else repl (rt_exe,
                        stepno+1,
                        PE.begin_stdin(),   (* Clear the state *)
                        rp,
                        libs_acc,
                        deps)
           )
         | (NONE, PE.SUCCESS _) => die "repl - impossible"
    end handle Quit => do_exit rp OS.Process.success
             | Report.DeepError r =>
               ( Report.print' r (!Flags.log)
               ; repl (rt_exe,
                       stepno+1,
                       PE.begin_stdin(),   (* Clear the state *)
                       rp,
                       libs_acc,
                       deps)
               )

val flags_to_block = ["regionvar", "values_64bit", "uncurrying",
    "safeLinkTimeElimination", "repository", "strip", "tag_pairs",
    "tag_values", "unbox_reals", "warn_spurious", "region_profiling",
    "recompile_basislib", "print_K_normal_forms",
    "parallelism_alloc_unprotected", "print_bit_vectors",
    "print_all_program_points", "parallelism", "output", "namebase",
    "mlb-subdir", "link_time_dead_code_elimination", "libs",
    "link_code", "libdirs", "import_basislib",
    "generational_garbage_collection", "gdb_support",
    "garbage_collection", "extra_gc_checks", "compile_only",
    "link_exe", "link_shared", "assembler", "argobots",
    "alloc_protect_always", "SML_LIB", "quotation",
    "warn_on_parallel_puts", "mlb_path_maps", "load_basis_files",
    "disable_spurious_type_variables", "dangling_pointers_statistics",
    "dangling_pointers", "statistics_spurious", "type_check_lambda",
    "report_file_sig", "log_to_file"]

fun run () : OS.Process.status =
    let val () = Flags.turn_on "report_file_sig"
        val () = List.app Flags.block_entry flags_to_block
        val () = if Flags.is_on "garbage_collection" then
                   ( print("|Garbage collection disabled - it is not supported in the REPL!\n")
                   ; Flags.turn_off "garbage_collection"
                   )
                 else ()
        val () = print "|Type :help; for help...\n";
        val () = Flags.turn_off "messages"
    in case MO.mk_repl_runtime of
           SOME f =>
           let val rt_exe = f()
               val (stepno, state, rp, libs_acc, deps) =
                   initialise_repl (PE.begin_stdin()) rt_exe
           in repl (rt_exe, stepno, state, rp, libs_acc, deps)
           end
         | NONE => die "run - not possible to build runtime"
    end

end
