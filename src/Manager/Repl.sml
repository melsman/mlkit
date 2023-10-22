signature REPL = sig
  val run : unit -> OS.Process.status
end

functor Repl(structure ManagerObjects : MANAGER_OBJECTS
             structure IntModules : INT_MODULES
             sharing type ManagerObjects.IntBasis = IntModules.IntBasis
             sharing type ManagerObjects.modcode = IntModules.modcode
             structure Manager : MANAGER)
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

type Basis = MO.Basis
type modcode = MO.modcode
type absprjid = ME.absprjid
type InfixBasis = MO.InfixBasis
type ElabBasis = MO.ElabBasis
type IntBasis = MO.IntBasis
type opaq_env = MO.opaq_env

(* -------------------------------------------
 * Logging
 * ------------------------------------------- *)

fun die (s:string) : 'a =
    (print("Error: " ^ s ^ "\n"); raise Fail ("Internal Error - Repl: " ^ s))
fun log (s:string) : unit = TextIO.output (!Flags.log, s)
fun chat s = if !Flags.chat then log (s ^ "\n") else ()
fun chatf f = if !Flags.chat then log (f() ^ "\n") else ()
fun pr_st (st) : unit = PP.outputTree (print, st, 120)

infix ##
val op ## = OS.Path.concat

(* -------------------------------------------
 * Debugging and reporting
 * ------------------------------------------- *)

val debug_p = Flags.is_on0 "debug_compiler"

fun debug s =
    if debug_p() then print("[REPL DEBUG: " ^ s ^ "]\n")
    else ()

val print_post_elab_ast = Flags.is_on0 "print_post_elab_ast"

fun maybe_print_topdec s topdec =
    if print_post_elab_ast() orelse debug_p() then
      let val _ = print (s ^ ":\n")
	  val st = PostElabTopdecGrammar.layoutTopdec topdec
      in pr_st st
      end
    else ()

(* -------------------------------
 * Compute actual dependencies
 * ------------------------------- *)

local

  fun lookup (look: ElabBasis -> 'a -> bool) elabBasesInfo (eb0:ElabBasis) (id:'a) =
      let fun loop nil =
              if look eb0 id then ()
              else die "computing actual dependencies.lookup failed"
            | loop ({ebfile,infixElabBasis=(_,eb),used}::xs) =
              if look eb id then used:=true
              else loop xs
      in loop elabBasesInfo
      end

  fun collapse (longstrids,longtycons,longvids) =
      let fun exists e l = List.exists (fn x => x = e) l
          fun ins e l = if exists e l then l else e::l
          val strids =
              foldl (fn (longstrid,acc) =>
                        case StrId.explode_longstrid longstrid of
                            (s::_,_) => ins s acc
                          | (nil,s) => ins s acc)
                    nil longstrids
          val (strids,tycons) =
              foldl (fn (longtycon,(strids,tycons)) =>
                        case TyCon.explode_LongTyCon longtycon of
                            (s::_,_) => (ins s strids,tycons)
                          | (nil,tycon) => (strids,ins tycon tycons))
                    (strids,nil) longtycons
          val (strids,vids) =
              foldl (fn (longvid,(strids,vids)) =>
                        case Ident.decompose longvid of
                            (s::_,_) => (ins s strids,vids)
                          | (nil,vid) => (strids,ins vid vids))
                    (strids,nil) longvids
      in (vids,tycons,strids)
      end
in

  fun compute_actual_deps
          (eb0:ElabBasis)
          (elabBasesInfo:{ebfile:string,infixElabBasis:InfixBasis*ElabBasis,used:bool ref}list)
          {funids,sigids,longstrids,longtycons,longvids} =
      let val (vids,tycons,strids) = collapse (longstrids,longtycons,longvids)
          fun look_vid B vid =
              Option.isSome (Environments.VE.lookup(Environments.E.to_VE(EB.to_E B)) vid)
          fun look_tycon B tycon =
              Option.isSome (Environments.TE.lookup(Environments.E.to_TE(EB.to_E B)) tycon)
          fun look_sigid B sigid =
              Option.isSome (ME.G.lookup(EB.to_G B) sigid)
          fun look_funid B funid =
              Option.isSome (ME.F.lookup(EB.to_F B) funid)
          fun look_strid B strid =
              Option.isSome (Environments.SE.lookup(Environments.E.to_SE(EB.to_E B)) strid)
          (* look into newest basis first *)
          val rev_elabBasesInfo = rev elabBasesInfo
      in app (lookup look_vid rev_elabBasesInfo eb0) vids
       ; app (lookup look_tycon rev_elabBasesInfo eb0) tycons
       ; app (lookup look_strid rev_elabBasesInfo eb0) strids
       ; app (lookup look_sigid rev_elabBasesInfo eb0) sigids
       ; app (lookup look_funid rev_elabBasesInfo eb0) funids
       ; map #ebfile (List.filter (! o #used) elabBasesInfo)
      end

end

fun add_longstrid longstrid {funids, sigids, longstrids, longtycons, longvids} =
    let val longstrids = longstrid::longstrids
    in {funids=funids, sigids=sigids, longstrids=longstrids,
        longtycons=longtycons, longvids=longvids}
    end

val intinfrep = StrId.mk_LongStrId ["IntInfRep"]

fun fid_topdec a = FreeIds.fid_topdec a
fun opacity_elimination a = OpacityElim.opacity_elimination a

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
      send_cmd (fd, "PRINT " ^ ty ^ " " ^ lab ^ ";")
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
 * Pickling
 * ------------------------------------------- *)

fun readFile f : string =
    let val is = BinIO.openIn f
    in let val v = BinIO.inputAll is
           val s = Byte.bytesToString v
       in BinIO.closeIn is; s
       end handle ? => (BinIO.closeIn is; raise ?)
    end

local (* Pickling *)

  fun sizeToStr sz =
      if sz < 10000 then Int.toString sz ^ " bytes"
      else if sz < 10000000 then Int.toString (sz div 1024) ^ "Kb (" ^ Int.toString sz ^ " bytes)"
      else Int.toString ((sz div 1024) div 1024) ^ "Mb (" ^ Int.toString sz ^ " bytes)"

in

  fun targetFromSmlFile smlfile ext =
      smlfile ^ "." ^ ext

  fun writeTextFile file s =
      let val os = TextIO.openOut file
          val _ = chatf (fn() => "[writing file " ^ file ^ "...]")
      in (  TextIO.output(os,s)
          ; TextIO.closeOut os
         ) handle X => (TextIO.closeOut os; raise X)
      end

  fun isFileContentStringBIN f s =
      let val is = BinIO.openIn f
      in ((Byte.bytesToString (BinIO.inputAll is) = s)
          handle _ => (BinIO.closeIn is; false))
         before BinIO.closeIn is
      end handle _ => false

  fun writePickle file pickleString =
      let val os = BinIO.openOut file
          val _ = chatf (fn() => "[writing pickle to file " ^ file ^ "...]")
      in (  BinIO.output(os,Byte.stringToBytes pickleString)
          ; BinIO.closeOut os
         ) handle X => (BinIO.closeOut os; raise X)
      end

  fun 'a doPickleGen0 (punit:string) (pu_obj: 'a Pickle.pu) (ext: string) (obj:'a) : string =
      let val _ = chatf (fn() => "[begin pickling " ^ ext ^ "-result for " ^ punit ^ "...]")
          val res = Pickle.pickle pu_obj obj
          val _ = chatf (fn() => "[end pickling " ^ ext ^ " (sz = " ^ sizeToStr (size res) ^ ")]")
      in res
      end

  fun 'a doPickleGen (punit:string) (pu_obj: 'a Pickle.pu) (ext: string) (obj:'a) =
      let val res = doPickleGen0 punit pu_obj ext obj
          val file = targetFromSmlFile punit ext
      in writePickle file res
      end

  fun unpickleGen smlfile pu ext : 'a option =    (* MEMO: perhaps use hashconsing *)
      let val s = readFile (targetFromSmlFile smlfile ext)
          val res = Pickle.unpickle pu s
      in SOME res
      end handle _ => NONE

  val pu_names = Pickle.listGen Name.pu
  val pu_NB0 = Pickle.pairGen(pu_names,B.pu_Basis0)
  val pu_NB1 = Pickle.pairGen(pu_names,B.pu_Basis1)

  fun doPickleNB smlfile (NB0,NB1) : unit =
      let val (ext0,ext1) = ("o.eb","o.eb1")
          val f0 = targetFromSmlFile smlfile ext0
          val f1 = targetFromSmlFile smlfile ext1
          val p0 = doPickleGen0 smlfile pu_NB0 ext0 NB0
          val p1 = doPickleGen0 smlfile pu_NB1 ext1 NB1
      in if (isFileContentStringBIN f0 p0
             andalso isFileContentStringBIN f1 p1) then
           (chat "[no writing: valid pickle strings already in eb-files.]")
         else (writePickle f0 p0 ; writePickle f1 p1)
      end

  fun doUnpickleBases0 ebfiles
      : Pickle.hce * {ebfile:string,infixElabBasis:InfixBasis*ElabBasis,used:bool ref}list =
      let val _ = chat "[begin unpickling elaboration bases...]"
          fun process (nil,hce,acc) = (hce, rev acc)
            | process (ebfile::ebfiles,hce,acc) =
              let val s = readFile ebfile handle _ => die("doUnpickleBases0.error reading file " ^ ebfile)
                  val ((_,infixElabBasis),hce) = Pickle.unpickle' pu_NB0 hce s
                                                 handle _ => die("doUnpickleBases0.error unpickling infixElabBasis from file " ^ ebfile)
                  val entry = {ebfile=ebfile,infixElabBasis=infixElabBasis,
                               used=ref false}
              in process(ebfiles,hce,entry::acc)
              end
      in
        case ebfiles of
            nil => (Pickle.empty_hce(),nil)
          | ebfile::ebfiles =>
            let val s = readFile ebfile handle _ =>
                                               die("doUnpickleBases0.error reading file " ^ ebfile)
                val hce = Pickle.empty_hce()
                val ((_,infixElabBasis),hce) =
                    Pickle.unpickle' pu_NB0 hce s
                    handle Fail st =>
                           die("doUnpickleBases0.error unpickling infixElabBasis from file "
                               ^ ebfile ^ ": Fail(" ^ st ^ "); sz(s) = " ^ Int.toString (size s))
                         | e =>
                           die("doUnpickleBases0.error unpickling infixElabBasis from file "
                               ^ ebfile ^ ": " ^ General.exnMessage e)
                val (hce, entries) =
                    process(ebfiles,hce,[{ebfile=ebfile,
                                          infixElabBasis=infixElabBasis,
                                          used=ref false}])
            in (hce, entries)
            end handle _ => die ("doUnpickleBases. error \n")
      end

  fun doUnpickleBases1 (hce: Pickle.hce) ebfiles : opaq_env * IntBasis =
      let val _ = chat "[begin unpickling compiler bases...]"
          fun process (nil,hce,basisPair) = basisPair
            | process (ebfile::ebfiles,hce,basisPair) =
              let val s = readFile ebfile
                  val ((_,basisPair'),hce) = Pickle.unpickle' pu_NB1 hce s
              in process(ebfiles,hce,B.plusBasis1(basisPair,basisPair'))
              end
          val basisPair0 = B.initialBasis1()
      in
        case ebfiles of
            nil => basisPair0
          | ebfile::ebfiles =>
            let val s = readFile ebfile
                val ((_,basisPair),hce) = Pickle.unpickle' pu_NB1 hce s
            in process(ebfiles,hce,B.plusBasis1(basisPair0,basisPair))
            end handle _ => die ("doUnpickleBases1. error \n")
      end
end

(* -------------------------------------------
 * Value Rendering
 * ------------------------------------------- *)

type rp = {command_pipe:Posix.IO.file_desc,reply_pipe:Posix.IO.file_desc,pid:Posix.Process.pid}

fun retrieve (rp:rp) B t longid =
    case MO.retrieve_longid B longid of
        MO.VAR lab =>
        ( send_PRINT (#command_pipe rp, t, "D." ^ lab);
          case receive_STR(#reply_pipe rp) of
              SOME s => s
            | NONE => "none" )
      | MO.STR s => s
      | MO.UNKN => "unknown"

fun str_t t =
    if Ty.eq(t,Ty.IntDefault()) then SOME "int"
    else if Ty.eq(t,Ty.Bool) then SOME "bool"
    else if Ty.eq(t,Ty.Real) then SOME "real"
    else if Ty.eq(t,Ty.Char) then SOME "char"
    else if Ty.eq(t,Ty.Int31) then SOME "int31"
    else if Ty.eq(t,Ty.Int32) then SOME "int32"
    else if Ty.eq(t,Ty.Int63) then SOME "int63"
    else if Ty.eq(t,Ty.Int64) then SOME "int64"
    else if Ty.eq(t,Ty.Word8) then SOME "word8"
    else if Ty.eq(t,Ty.WordDefault()) then SOME "word"
    else if Ty.eq(t,Ty.Word31) then SOME "word31"
    else if Ty.eq(t,Ty.Word32) then SOME "word32"
    else if Ty.eq(t,Ty.Word63) then SOME "word63"
    else if Ty.eq(t,Ty.Word64) then SOME "word64"
    else if Ty.eq(t,Ty.String) then SOME "string"
    else NONE

fun render rp B (strids,id,sch) =
    let val longid = Ident.implode_LongId(strids,id)
        val (tvs,rvs,t) = StatObject.TypeScheme.to_TyVars_and_Type sch
    in if List.null tvs then
         if Ty.is_Arrow t then "fn"
         else if Ty.eq(Ty.Unit, t) then "()"
         else case str_t t of
                  SOME t => retrieve rp B t longid
                | NONE => ".."
       else "fun"
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

fun files_deps deps =
    let val deps = map (fn SMLdep f => [f]
                         | MLBdep (_,deps) => deps) (rev deps)
    in List.concat deps
    end

fun read_df (mlbfile:string) : string list =
    let val (dir,dffile) = mlbfileToDfFile mlbfile
        val is = TextIO.openIn dffile
    in let val all = TextIO.inputAll is handle _ => ""
           val smlfiles = String.tokens Char.isSpace all
           val smlfiles = map (fn f => dir ## f) smlfiles
       in TextIO.closeIn is; smlfiles
       end handle _ => (TextIO.closeIn is; nil)
    end handle _ => nil

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

  fun string_to_nat s =
      if CharVector.all Char.isDigit s then
        Int.fromString s
      else NONE

  fun err s = print ("!" ^ s ^ "\n")

  fun print_help () =
      pr ("The REPL accepts toplevel declarations and commands on the\n\
          \form ':cmd;' where cmd is one of the following:\n\n\
          \  cmd ::= flags           -- describe all flags\n\
          \        | help            -- general help\n\
          \        | help flag       -- get help about a flag\n\
          \        | menu [N]        -- print flag menu [N]\n\
          \        | quit            -- quit\n\
          \        | search s        -- search for help about s\n\
          \        | set flag [arg]  -- set flag [maybe with arg]\n\
          \        | unset flag      -- unset the flag\n\n\
          \Notice that more flags are available from the command-line\n\
          \at REPL initialisation time.\n\
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
                  val modc = (* load modcode from lnk-file *)
                      let val s = readFile lnkfile
                                  handle _ => raise Fail ("Failed to load mlb-file")
                      in Pickle.unpickle ModCode.pu s
                      end
              in modc
              end
          val punit = "stdin_" ^ Int.toString stepno
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
             in (stepno+1, punit::libs_acc, MLBdep(mlbfilepath,defs)::deps)
             end
      end
      handle Manager.PARSE_ELAB_ERROR es =>
             ( pr(String.concat (map (fn ec => Manager.ErrorCode.pr ec ^ "\n") es))
             ; (stepno, libs_acc, deps)
             )
           | Fail msg =>
             ( pr msg
             ; (stepno+1, libs_acc, deps)
             )

in

exception Quit
fun process_cmd stepno (rp:rp) (cmd:string) libs_acc deps =
    let val ts = String.tokens Char.isSpace cmd
    in case ts of
           [":set", s] =>
           ( if is_flag0 s then Flags.turn_on s
             else err ("Invalid flag '" ^ s ^ "'")
           ; (stepno, libs_acc, deps)
           )
         | [":set", s, a] =>
           ( if is_flagN s then
               case string_to_nat a of
                   SOME n => Flags.lookup_int_entry s := n
                 | NONE => err ("Flag '" ^ s ^ "' expects a positive integer - got '"
                                ^ a ^ "'")
             else if is_flagS s then
               Flags.lookup_string_entry s := a
             else err ("Flag '" ^ s ^ "' is not a flag accepting an argument!")
           ; (stepno, libs_acc, deps)
           )
         | [":unset", s] =>
           ( if is_flag0 s then Flags.turn_off s
             else err ("Invalid flag '" ^ s ^ "'")
           ; (stepno, libs_acc, deps)
           )
         | [":help"] => ( print_help(); (stepno, libs_acc, deps) )
         | [":help", fl] => ( print_flag_help fl; (stepno, libs_acc, deps) )
         | [":menu"] => ( print_menu(); (stepno, libs_acc, deps) )
         | [":menu", n] => ( print_menuN n; (stepno, libs_acc, deps) )
         | [":search", s] => ( print_search s; (stepno, libs_acc, deps) )
         | [":flags"] => ( print_flags(); (stepno, libs_acc, deps) )
         | [":load", f] => load stepno rp libs_acc deps f
         | [":quit"] => raise Quit
         | _ => ( err ("Invalid command '" ^ cmd ^ "'") ; (stepno, libs_acc, deps) )
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

fun repl (stepno, state, rp:rp, libs_acc, deps:dep list) : OS.Process.status =
    let val (stepno,state,libs_acc,deps) =
            let fun loop stepno state libs_acc deps =
                    case PE.colonLine (PE.stripSemiColons state) of
                        SOME(cmd,state) =>
                        let val (stepno, libs_acc, deps) = process_cmd stepno rp cmd libs_acc deps
                        in loop stepno state libs_acc deps
                        end
                      | NONE => (stepno, state, libs_acc, deps)
            in loop stepno state libs_acc deps
            end
        val absprjid = ME.mk_absprjid "repl"
        val smlfile = "stdin_" ^ Int.toString stepno

        (* load the bases that smlfile depends on *)
        val ebfiles = map depToEb (files_deps deps)
        val (unpickleStream, elabBasesInfo) = doUnpickleBases0 ebfiles
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
               val freelongids = add_longstrid intinfrep (fid_topdec topdec)
               val _ = chat "[finding free identifiers end...]"

                val _ = chat "[computing actual dependencies begin...]"
                val ebfiles_actual = compute_actual_deps
                    (#2 initialBasis0) elabBasesInfo freelongids
                val ebfiles_actual = map (fn x => x ^ "1") ebfiles_actual
                val _ = chat "[computing actual dependencies end...]"

                val (B_im,_) =
                    let val (opaq_env,intB) =
                        doUnpickleBases1 unpickleStream ebfiles_actual
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
               val (topdec', oE') = opacity_elimination(oE_im, topdec)
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
                   in doPickleNB smlfile (NB0',NB1')
                   end

               (* send command to load and run the shared library *)
               val () = send_LOADRUN(#command_pipe rp,sofile)

           in case receive_EXN_or_DONE(#reply_pipe rp) of
                  EXN =>
                  ( repl (stepno+1,         (* No reporting! *)
                          PE.begin_stdin(), (* Clear the state *)
                          rp,
                          libs_acc,         (* Nothing from this unit to link to later, I think *)
                          deps)             (* No contribution to static basis *)
                  )
                | DONE =>
                  ( Report.print (doreport (SOME (render rp B'Closed)))
                  ; repl (stepno+1,
                          state',
                          rp,
                          smlfile::libs_acc,
                          SMLdep smlfile :: deps)
                  )
           end
         | (SOME state', PE.FAILURE (report,errs)) =>
           ( Report.print report
           ; repl (stepno+1,
                   PE.begin_stdin(),   (* Clear the state *)
                   rp,
                   libs_acc,
                   deps)
           )
         | (_, PE.FAILURE (report,errs)) =>   (* some syntax errors end here, so we shouldn't exit... *)
           ( Report.print report
           ; repl (stepno+1,
                   PE.begin_stdin(),   (* Clear the state *)
                   rp,
                   libs_acc,
                   deps)
           ; do_exit rp OS.Process.failure
           )
         | (NONE, PE.SUCCESS _) => die "repl - impossible"
    end handle Quit => do_exit rp OS.Process.success

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
                   ( print("Disabling garbage collection - it is not supported with the REPL\n")
                   ; Flags.turn_off "garbage_collection"
                   )
                 else ()
        val () = print "Type :help; for help...\n";
        val () = Flags.turn_off "messages"
    in case MO.mk_repl_runtime of
           SOME f =>
           let val rt_exe = f()
               val () = debug ("wrote runtime executable " ^ rt_exe)
               (* Now, start the runtime executable in a child process,
                  but first create two named pipes *)
               val command_pipe_name = MO.mlbdir() ## "command_pipe"
               val reply_pipe_name = MO.mlbdir() ## "reply_pipe"
               val repl_logfile = MO.mlbdir() ## "repl.log"
               val () = OS.FileSys.remove command_pipe_name handle _ => ()
               val () = OS.FileSys.remove reply_pipe_name handle _ => ()
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
               val command_pipe = Posix.FileSys.openf(command_pipe_name,Posix.FileSys.O_WRONLY,
                                                      Posix.FileSys.O.flags[])
                                  handle _ => die "run: failed to open command_pipe"
               val () = debug "opened command_pipe"
               val reply_pipe = Posix.FileSys.openf(reply_pipe_name,Posix.FileSys.O_RDONLY,
                                                    Posix.FileSys.O.flags[])
                                handle _ => die "run: failed to open reply_pipe"
               val () = debug "opened reply_pipe"
               val rp = {command_pipe=command_pipe,reply_pipe=reply_pipe,pid=childpid}
           in repl (0, PE.begin_stdin(), rp, ["runtime"], nil)
           end
         | NONE => die "run - not possible to build runtime"
    end

end
