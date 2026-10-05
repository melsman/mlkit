open ProfileJson
fun assert b msg = if b then () else raise Fail msg
fun write path text =
  let val out = TextIO.openOut path
  in TextIO.output(out,text); TextIO.closeOut out
  end
val object = OS.FileSys.tmpName ()
val _ = write object "object"
val tree = PrettyPrint.HNODE {start = "\195\166 ",finish = "",childsep = PrettyPrint.RIGHT " ",
  children = [PrettyPrint.MARKED_LEAF(7,"$foreign"),PrettyPrint.MARKED_LEAF(7,"attop r9"),
              PrettyPrint.MARKED_LEAF(7,"attop r10")]}
val _ = IRLocations.write {object = object,
  document = {identity = "matching-build",unit = "unit",source = "/unavailable/source.sml",tree = tree}}
(* Relocation does not require the original object or source tree. *)
val companion = object ^ ".o.ir"
val _ = OS.FileSys.rename {old = object ^ ".ir",new = companion}
val _ = OS.FileSys.remove object
fun site id point kind identity = Obj
  ([("definition",Num id),("site",Num id),("unit",Str "unit"),
    ("source",Str "/unavailable/source.sml")] @
   (if identity = "legacy" then [] else
     [("point",Num point),("location_kind",Num kind),("ir_identity",Str identity),("ir_object",Str (object ^ ".o"))]))
val sites = [site "1" "7" "0" "matching-build", site "2" "7" "1" "matching-build",
             site "3" "0" "0" "matching-build", site "4" "8" "0" "matching-build",
             site "5" "7" "0" "other-build", site "6" "7" "0" "legacy"]
fun report () = ProfileIR.enrich []
  {samples = [],metadata = Obj [("allocations",Arr (sites @ [hd sites]))]}
val {metadata,...} = report ()
val locations = ProfileIR.rows metadata "ir_sites"
val documents = ProfileIR.rows metadata "ir_documents"
val _ = assert (length locations = 6 andalso length documents = 1) "deduplication"
fun location id = valOf (List.find (fn r => get r "definition" = Num id) locations)
val _ = assert (length (ProfileIR.rows (location "1") "spans") = 2) "duplicated allocation locations"
val _ = assert (length (ProfileIR.rows (location "2") "spans") = 1) "foreign-call token location"
val _ = app (fn (id,status) => assert (get (location id) "status" = Str status) status)
  [("1","available"),("2","available"),("3","generated"),("4","missing-mark"),
   ("5","missing-or-mismatched-ir"),("6","legacy-profile")]
val moved = object ^ "-moved.o.ir"
val _ = OS.FileSys.rename {old = companion,new = moved}
val _ = assert (null (ProfileIR.rows (#metadata(report ())) "ir_documents")) "implicit path search"
val relocated = ProfileIR.enrich [moved,moved]
  {samples = [],metadata = Obj [("allocations",Arr sites)]}
val _ = assert (length (ProfileIR.rows (#metadata relocated) "ir_documents") = 1) "explicit relocation"
val _ = OS.FileSys.rename {old = moved,new = companion}
val text = ProfileIR.stringField (hd documents) "text"
val _ = write companion (text ^ "corruption")
val _ = assert (null (ProfileIR.rows (#metadata(report ())) "ir_documents")) "corrupt artifact accepted"
val _ = OS.FileSys.remove companion
val _ = assert (null (ProfileIR.rows (#metadata(report ())) "ir_documents")) "missing artifact accepted"
(* A valid checksum is insufficient if the table lies about a span. *)
fun replace text needle replacement =
  let val (a,b) = Substring.position needle (Substring.full text)
  in assert (not(Substring.isEmpty b)) "test replacement missing";
     Substring.string a ^ replacement ^ Substring.string(Substring.triml (size needle) b)
  end
val fields = String.tokens (fn c => c = #"\n") text
val digestLine = valOf(List.find (String.isPrefix "content-md5\t") fields)
val zero = String.implode(List.tabulate(32,fn _ => #"0"))
val normalized = replace text digestLine ("content-md5\t" ^ zero)
val span = hd (ProfileIR.rows (location "2") "spans")
val row = String.concatWith "\t" (map (fn k => encodeJson(get span k))
  ["mark","start","length","line","column"]) ^ "\n"
val badRow = String.concatWith "\t" (map (fn k => if k = "line" then "999" else encodeJson(get span k))
  ["mark","start","length","line","column"]) ^ "\n"
val malformed = replace normalized row badRow
val malformed = replace malformed ("content-md5\t" ^ zero) ("content-md5\t" ^ MD5.fromString malformed)
val _ = write companion malformed
val _ = assert (null (ProfileIR.rows (#metadata(report ())) "ir_documents")) "invalid span accepted"
val _ = OS.FileSys.remove companion
val _ = print "IR report mapping, identity, relocation and fallback: PASS\n"
