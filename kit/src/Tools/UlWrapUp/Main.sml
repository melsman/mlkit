
structure ULLrVals = ULLrValsFun(structure Token = LrParser.Token)
structure ULLex = UlLexFun(structure Tokens = ULLrVals.Tokens)

structure UlParser = Join(structure ParserData = ULLrVals.ParserData
                          structure Lex = ULLex
                          structure LrParser = LrParser);

val pp_err = fn s => TextIO.output (TextIO.stdErr, s)

fun createLexerStream (is : TextIO.instream) =
  UlParser.makeLexer ((fn _ => TextIO.input is) handle 
              IO.Io {cause = OS.SysErr(c,_), name = name, ...} => 
                 raise Fail (String.concat
                   ["\n! IO Error, tried reading file: ",
                    name, ",\n! but got the error: ", c, "\n"]))

fun parse file =
    let val is     = TextIO.openIn file
        val lexbuf = createLexerStream is
        val (expr,lb) =
                   (UlParser.parse (0,lexbuf, fn (s,p1,p2) =>
                        List.app pp_err [s, " ", Int.toString p1,
                                        ",", Int.toString p2], ()))
                   handle exn => (TextIO.closeIn is; raise exn)
    in
      TextIO.closeIn is; expr
    end

fun pp_uofile f = String.concat [" ", f, "\n"]

fun pp_scripts (f,loc) = String.concat [" ", f, " As ", loc, "\n"]

fun pp_syntax (UlFile.UoFile l) = String.concat ["Codefiles\n", String.concat (List.map pp_uofile l), "End\n"]
  | pp_syntax (UlFile.Script l) = String.concat ["Scripts\n", String.concat (List.map pp_scripts l), "End\n"]


fun openfile file =
    let
      val _ = List.app pp_err ["[opening file \"",file,"\"]\n"]
      val is = (TextIO.openIn file) handle
                           IO.Io {cause = OS.SysErr(c,_), name = name, ...} =>
                                    raise Fail (String.concat
                                                  ["\n! IO Error, tried opening file: ",
                                                   name, ",\n! but got the error: ", c, "\n"])
    in ref (SOME (createLexerStream is,is), file)
    end

fun copyFile (i,out) = 
     let
       fun errmsg s = ("copyFile : " ^ i ^ " --> " ^ out ^ " : " ^ s ^ "\n")
       val fdout = (Posix.FileSys.creat(out,Posix.FileSys.S.flags [Posix.FileSys.S.irusr]))
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

local
  val a = ref 48
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
  fun newname () = (gename (!a)) before a:= (!a) + 1
in
  val newname = fn () => (newname ()) ^ ".uo"
end

fun cc_uofile infile f =
     let
       val f' = if OS.Path.isRelative f
                then OS.Path.concat(infile,f)
                else f
     in
       if alreadyCopied f'
       then NONE
       else
       let
         val out = newname()
         val _ = copyFile (f',out)
       in
         SOME out
       end
     end

fun cc_scripts infile (f,loc) = 
     let
       val f' = if OS.Path.isRelative f
                then OS.Path.concat(infile,f)
                else f
     in
       if not (alreadyCopied f')
       then 
         let
           val out = newname()
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

fun run (infile,outfile) = 
  let
    val parseTree = parse infile
    val {dir,file} = OS.Path.splitDirFile infile
    val infile' = OS.Path.mkCanonical(OS.Path.concat(dir,"../../"))
    val npt = List.mapPartial (fn x => UlFile.mapPartial (fn y => SOME y) (cc_uofile infile') (cc_scripts infile') x) parseTree
    val fdout = (Posix.FileSys.createf(outfile,Posix.FileSys.O_WRONLY, Posix.FileSys.O.flags [], Posix.FileSys.S.flags [Posix.FileSys.S.irusr]))
          handle OS.SysErr (s,e) => raise Fail ("Couldn'y open file: " ^ outfile ^ " for writing\n" ^ s)
    val writer = Posix.IO.mkTextWriter {fd=fdout,name=outfile,initBlkMode=true,appendMode=false,chunkSize=4000}
    val os = TextIO.StreamIO.mkOutstream (writer,IO.LINE_BUF)
    val _ = TextIO.StreamIO.output (os, String.concat (List.map pp_syntax npt))
  in
    TextIO.StreamIO.closeOut os
  end

val _ = let 
          val args = CommandLine.arguments()
          val _ = if List.length args < 2 then raise Fail ("Less than two arguments where given to me.\n" ^
                                                           "Please provide an ul-file as first argument and\n" ^
                                                           "an output file for output. Then I fill up the current\n" ^
                                                           "directory with a project described in the output file\n")
                  else ()
          val (infile::outfile::arg) = args
        in
          run (infile,outfile)
        end
          handle Fail a => TextIO.output (TextIO.stdErr,a)
               | OS.SysErr(s,e) => TextIO.output (TextIO.stdErr, "Something bad happened: " ^ s)


