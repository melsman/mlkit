(* $Id:$ *)
(* Author: Carsten Varming 2006 *)

structure ULLrVals = ULLrValsFun(structure Token = LrParser.Token)
structure ULLex = UlLexFun(structure Tokens = ULLrVals.Tokens)

structure UlParser = Join(structure ParserData = ULLrVals.ParserData
                          structure Lex = ULLex
                          structure LrParser = LrParser);

val filepermission = Posix.FileSys.S.flags [Posix.FileSys.S.irusr,Posix.FileSys.S.irgrp,Posix.FileSys.S.iroth]
val dirpermission = Posix.FileSys.S.flags [Posix.FileSys.S.irwxu,Posix.FileSys.S.irgrp,Posix.FileSys.S.iroth,
                                           Posix.FileSys.S.ixgrp,Posix.FileSys.S.ixoth]

val pp_err = fn s => TextIO.output (TextIO.stdErr, s)

fun createLexerStream (is : TextIO.instream) =
  UlParser.makeLexer ((fn _ => TextIO.input is) handle 
              IO.Io {cause = OS.SysErr(c,_), name = name, ...} => 
                 raise Fail (String.concat
                   ["\n! IO Error, tried reading file: ",
                    name, ",\n! but got the error: ", c, "\n"]))

fun parse file =
    let 
      val _ = List.app pp_err ["[opening file \"",file,"\"]\n"]
      val is     = TextIO.openIn file handle
                           IO.Io {cause = OS.SysErr(c,_), name = name, ...} =>
                                    raise Fail (String.concat
                                                  ["\n! IO Error, tried opening file: ",
                                                   name, ",\n! but got the error: ", c, "\n"])
      val lexbuf = createLexerStream is
      val (expr,lb) =
                 (UlParser.parse (0,lexbuf, fn (s,p1,p2) =>
                      List.app pp_err [s, " ", Int.toString p1,
                                      ",", Int.toString p2], ()))
                 handle exn => (TextIO.closeIn is; raise exn)
    in
      (List.app pp_err ["[closing file \"",file,"\"]\n"] ; TextIO.closeIn is; expr)
    end

fun pp_uofile f = String.concat [" ", f, "\n"]

fun pp_scripts (f,loc) = String.concat [" ", f, " As ", loc, "\n"]

fun pp_syntax (UlFile.UoFile l) = String.concat ["Codefiles\n", String.concat (List.map pp_uofile l), "End\n"]
  | pp_syntax (UlFile.Script l) = String.concat ["Scripts\n", String.concat (List.map pp_scripts l), "End\n"]

fun copyFile (i,out) = 
     let
       fun errmsg s = ("copyFile : " ^ i ^ " --> " ^ out ^ " : " ^ s ^ "\n")
       val fdout = (Posix.FileSys.creat(out,filepermission))
                    handle OS.SysErr (s,e) => raise Fail ("Couldn'y open file: " ^ out ^ " for writing\n" ^ (errmsg s))
       val fdin = (Posix.FileSys.openf(i,Posix.FileSys.O_RDONLY,Posix.FileSys.O.flags []))
                    handle OS.SysErr (s,e) => raise Fail ("Couldn't open file: " ^ i ^ " for reading\n" ^ (errmsg s))
       fun writeOut vc = 
            let
              val n = Posix.IO.writeVec (fdout,vc)
            in
              if n < Word8VectorSlice.length vc
              then writeOut (Word8VectorSlice.subslice (vc,n,NONE))
              else ()
            end
       fun loop () = 
            let
              val id = Posix.IO.readVec(fdin,4000)
            in
              if Word8Vector.length id = 0
              then ()
              else (writeOut (Word8VectorSlice.full id);loop())
            end
       fun close () = (Posix.IO.close fdin; Posix.IO.close fdout)
     in
       (loop () ; close ()) handle ? => (close () ; raise ?)
     end

val mt = Binaryset.empty String.compare
val mt' = Binarymap.mkDict String.compare

val ac = ref mt
val ac' = ref mt'

fun alreadyCopied i = (Binaryset.member (!ac,i)) before ac := Binaryset.add (!ac,i)
fun alreadyCopied' (i,out) =
                      case (Binarymap.peek (!ac',i)) 
                      of SOME a => SOME a
                       | NONE => (ac' := Binarymap.insert (!ac',i,out) ; NONE)


fun cc_uofile pres newname infile f =
     let
       val f' = if OS.Path.isRelative f
                then OS.Path.concat(infile,f)
                else f
     in
       if alreadyCopied f'
       then NONE
       else
       let
         val out = if Binaryset.member (pres,f') then newname (SOME (#file (OS.Path.splitDirFile f'))) else newname NONE
         val _ = copyFile (f',out)
       in
         SOME out
       end
     end

fun cc_scripts pres newname infile (f,loc) = 
     let
       val f' = if OS.Path.isRelative f
                then OS.Path.concat(infile,f)
                else f
     in
       if not (alreadyCopied f')
       then 
         let
           val out = if Binaryset.member (pres,f') then newname (SOME (#file(OS.Path.splitDirFile f'))) else newname NONE
         (*  val _ = print (f' ^ " --> " ^ out ^ "\n") *)
         in
           case alreadyCopied' (f',out)
           of SOME a => raise Fail  ("the file: " ^ f' ^ " is suppose to be new, but I already know of it\n")
            | NONE => (copyFile (f',out); SOME (out,loc))
          end
        else
           case Binarymap.peek (!ac',f')
           of SOME out => SOME (f',out)
            | NONE => raise Fail ("the file: " ^ f' ^ " is suppose to copied before, but I don't recall that\n")
      end

local
  val a = ref 0
  val base = 60
  fun gename' i q =
        let
          val l = i mod base
          val k = i div base
        in
          if k > 0 
          then gename' k (l::q)
          else (l::q)
        end
  fun gename a = 
    let
      val a' = gename' a []
      val b = List.map (fn x => if x > 25 
                                then
                                  if x > 50
                                  then  chr(x + (ord #"0") - 50)
                                  else chr(x + (ord #"a") - 25)
                                else chr(x + (ord #"A"))) a'
    in
      implode b
    end
  fun newname pres = 
       let
         fun g () = 
           let
             val name = (gename (!a)) before a:= (!a) + 1
           in
             if Binaryset.member(pres,name)
             then g ()
             else name
           end
       in
         g
       end
in
  val newname = fn pres => 
        let
          val newname = newname (List.foldl (fn (p,acc) => (fn x =>
                                                 if Binaryset.member (acc,x) then raise Fail ("Multiple files named " ^ x ^ " cannot be preserved")
                                                 else Binaryset.add(acc,x)) (#file (OS.Path.splitDirFile p)))
                                            (Binaryset.empty String.compare) pres)
        in fn base => fn NONE => OS.Path.concat(base,(newname ()) ^ ".uo")
                       | SOME n => OS.Path.concat(base,n)
        end
end

fun mkdir file = 
  let
    val {dir,file} = OS.Path.splitDirFile file
    val {isAbs, vol, arcs} = OS.Path.fromString dir
    fun loop (s,acc) = let
                         val dir = OS.Path.concat (acc,s)
                       in
                         if ((Posix.FileSys.ST.isDir (Posix.FileSys.stat dir)) 
                            handle OS.SysErr _ => ((Posix.FileSys.mkdir(dir,dirpermission);true)
                               handle OS.SysErr (s,e) => raise Fail ("cannot create " ^ dir ^ " : " ^ s ^ "\n")))
                         then dir
                         else raise Fail ("cannot create " ^ dir ^ " as is already exists and it is not a directory\n")
                       end
  in
    ignore (List.foldl loop (if isAbs then "/" else "") arcs)
  end

fun isEmpty dir = 
       let val d = (Posix.FileSys.opendir dir) handle OS.SysErr(s,e) => raise Fail ("cannot check " ^ dir ^ " : " ^ s ^ "\n")
       in (case Posix.FileSys.readdir d
          of NONE => (Posix.FileSys.closedir d ; true)
           | SOME _ => (Posix.FileSys.closedir d ; false))
             handle OS.SysErr (s,e) => (Posix.FileSys.closedir d ; raise Fail ("cannot check " ^ dir ^ " : " ^ s ^ "\n"))
       end

local
fun run (pres,infile,os,newname) = 
      let
        val {dir=indir,...} = OS.Path.splitDirFile infile
        val infile' = OS.Path.mkCanonical(OS.Path.concat(indir,"../../"))
        val parseTree = parse infile
        val npt = List.mapPartial (fn x => UlFile.mapPartial (fn y => SOME y) (cc_uofile pres newname infile') (cc_scripts pres newname infile') x) parseTree
      in
        ignore(TextIO.StreamIO.output (os, String.concat (List.map pp_syntax npt)))
      end
in

fun run' (_,[],_) = raise Fail "No input files ?"
  | run' (pres,l,outfile) = 
      let
        val {dir=outdir,...} = OS.Path.splitDirFile outfile
        val newname = newname pres outdir
        val pres = Binaryset.addList (Binaryset.empty String.compare, pres)
        val _ = mkdir outfile
        val _ = if isEmpty outdir then () else raise Fail ("directory " ^ outdir ^ " is not empty\n")
        val fdout = (Posix.FileSys.createf(outfile,Posix.FileSys.O_WRONLY, Posix.FileSys.O.flags [], filepermission))
              handle OS.SysErr (s,e) => raise Fail ("Couldn'y open file: " ^ outfile ^ " for writing\n" ^ s)
        val writer = Posix.IO.mkTextWriter {fd=fdout,name=outfile,initBlkMode=true,appendMode=false,chunkSize=4000}
        val os = TextIO.StreamIO.mkOutstream (writer,IO.LINE_BUF)
        fun close () = TextIO.StreamIO.closeOut os
      in
        (List.app (fn i => run (pres,i,os,newname)) l ; close ()) handle ? => (close (); raise ?)
      end
end

val run = run'

val _ = let 
          val (pres,infiles,outfile) = Arg.parse (CommandLine.arguments())
        in
          run (pres,infiles,outfile)
        end
          handle Fail a => TextIO.output (TextIO.stdErr,a ^ "\n")
               | OS.SysErr(s,e) => TextIO.output (TextIO.stdErr, "Something bad happened: " ^ s ^ "\n")


