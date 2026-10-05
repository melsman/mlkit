(* Location tables for printed IR. All offsets and columns count bytes. *)
structure IRLocations =
struct
  fun render tree width =
    let val chunks = ref []
        val spans = PrettyPrint.outputTreeWithSpans
          {device = fn s => chunks := s :: !chunks, tree = tree, width = width}
    in {text = String.concat (rev (!chunks)), spans = spans}
    end

  fun tableAt {offset,firstLine} text (spans : PrettyPrint.span list) =
    let
      fun advance (offset,line,column) target =
        if offset = target then (offset,line,column)
        else if String.sub (text,offset) = #"\n" then
          advance (offset+1,line+1,1) target
        else advance (offset+1,line,column+1) target
      fun rows ([],_,acc) = String.concat (rev acc)
        | rows ({mark,start,length}::rest,pos,acc) =
            let val pos' as (_,line,column) = advance pos start
                val row = String.concatWith "\t"
                  (map Int.toString [mark,start+offset,length,line,column]) ^ "\n"
            in rows (rest,pos',row::acc)
            end
    in
      "\nMLKIT-IR-LOCATIONS 1\nmark\tstart\tlength\tline\tcolumn\n" ^
      rows (spans,(0,firstLine,1),[]) ^ "MLKIT-IR-LOCATIONS-END\n"
    end

  fun table text spans = tableAt {offset = 0, firstLine = 1} text spans

  fun output {device,tree,width} =
    let val {text,spans} = render tree width
    in
      device "\nMLKIT-IR-BEGIN\n";
      device text;
      device (table text spans)
    end
  val currentIdentity = ref ""
  fun newIdentity unit = MD5.fromString (unit ^ IntInf.toString (Time.toNanoseconds (Time.now ())))
  val currentCalls = ref ([] : (string * string * string) list)
  type document = {calls : (string * string * string) list, identity : string, unit : string, source : string, tree : PrettyPrint.StringTree}
  val zeroDigest = String.implode (List.tabulate (32,fn _ => #"0"))

  fun header {identity,unit,source,objectDigest,codeBytes} digest =
    String.concat ["MLKIT-IR 3\n", "identity\t",identity,"\n", "unit\t",unit,"\n", "source\t",source,"\n",
                   "object-md5\t",objectDigest,"\n", "content-md5\t",digest,"\n",
                   "code-bytes\t",Int.toString codeBytes,"\n"]

  fun write {object,document = {identity,unit,source,tree,calls} : document} =
    let
      (* Fixed layout settings make the artifact independent of diagnostic flags. *)
      val oldRagged = !PrettyPrint.raggedRight
      val oldWidth = !PrettyPrint.colwidth
      fun restore () = (PrettyPrint.raggedRight := oldRagged;
                        PrettyPrint.colwidth := oldWidth)
      val rendered =
        (PrettyPrint.raggedRight := true; PrettyPrint.colwidth := 100;
         (render tree 100 before restore ()) handle exn => (restore (); raise exn))
      val {text,spans} = rendered
      val info = {identity = identity, unit = String.toString unit, source = String.toString source,
                  objectDigest = MD5.fromFile object, codeBytes = size text}
      val prefix = header info zeroDigest
      val callTable = "MLKIT-IR-CALLS 1\n" ^
        String.concat (map (fn (kind,caller,callee) =>
          String.concatWith "\t" [kind,String.toString caller,String.toString callee] ^ "\n") calls) ^
        "MLKIT-IR-CALLS-END\n"
      val payload = text ^ tableAt {offset = size prefix, firstLine = 8} text spans ^ callTable
      val digest = MD5.fromString (prefix ^ payload)
      val file = object ^ ".ir"
      val temporary = file ^ ".tmp"
      val stream = TextIO.openOut temporary
      fun cleanup () = (TextIO.closeOut stream handle _ => ();
                        OS.FileSys.remove temporary handle _ => ())
    in
      (TextIO.output (stream,header info digest ^ payload);
       TextIO.closeOut stream;
       OS.FileSys.rename {old = temporary, new = file})
      handle exn => (cleanup (); raise exn)
    end

  (* Cache validation checks both the object and the complete IR artifact,
   * including metadata and the trailing table. MD5 is an identity check,
   * not an authentication mechanism. *)
  fun consistent object =
    let
      val stream = TextIO.openIn (object ^ ".ir")
      fun read () =
        let
          fun line () = case TextIO.inputLine stream of
              SOME s => if String.isSuffix "\n" s then String.substring (s,0,size s-1)
                        else raise Fail "unterminated IR header"
            | NONE => raise Fail "truncated IR header"
          fun field key =
            let val s = line ()
                val prefix = key ^ "\t"
            in if String.isPrefix prefix s then String.extract (s,size prefix,NONE)
               else raise Fail "invalid IR header"
            end
          val version = line ()
          val identity = field "identity"
          val unit = field "unit"
          val source = field "source"
          val objectDigest = field "object-md5"
          val digest = field "content-md5"
          val codeBytes = case Int.fromString (field "code-bytes") of
              SOME n => n | NONE => raise Fail "invalid IR size"
          val payload = TextIO.inputAll stream
          val prefix = header {identity = identity,unit = unit,source = source,objectDigest = objectDigest,
                               codeBytes = codeBytes} zeroDigest
        in
          version = "MLKIT-IR 3" andalso codeBytes >= 0 andalso codeBytes <= size payload
          andalso String.isPrefix "\nMLKIT-IR-LOCATIONS 1\n"
                    (String.extract (payload,codeBytes,NONE))
          andalso String.isSuffix "MLKIT-IR-CALLS-END\n" payload
          andalso MD5.fromString (prefix ^ payload) = digest
          andalso MD5.fromFile object = objectDigest
        end
    in (read () before TextIO.closeIn stream)
       handle exn => (TextIO.closeIn stream; raise exn)
    end handle _ => false
  (* The linker sees the actual object paths, including installed/cached units.
   * Record them here instead of trying to reconstruct them from source names. *)
  fun linkMap files =
    let
      fun entry object =
        let val input = TextIO.openIn (object ^ ".ir")
            val lines = ((TextIO.inputLine input,TextIO.inputLine input)
                         before TextIO.closeIn input)
                        handle e => (TextIO.closeIn input; raise e)
        in case lines of
            (SOME "MLKIT-IR 3\n",SOME identity) =>
              if String.isPrefix "identity\t" identity andalso String.isSuffix "\n" identity then
                SOME (String.substring(identity,9,size identity-10),OS.FileSys.fullPath object)
              else NONE
          | _ => NONE
        end handle IO.Io _ => NONE | OS.SysErr _ => NONE
      fun quoted s = "\"" ^ String.toCString s ^ "\""
      fun row (identity,path) = "{" ^ quoted identity ^ "," ^ quoted path ^ "},\n"
    in "const char *const volatile mlkit_rp_ir_objects[][2] = {\n" ^
       String.concat (map row (List.mapPartial entry files)) ^ "{0,0}};\n"
    end
end
