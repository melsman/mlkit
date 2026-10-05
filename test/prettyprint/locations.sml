open PrettyPrint
fun check b = if b then () else raise Fail "IR locations"
val tree = NODE {start = "", finish = "", indent = 2, childsep = NOSEP,
                 children = [MARKED_LEAF (7,"attop r3"),MARKED_LEAF (8,"sat r4")]}
val _ = raggedRight := false
val {text,spans} = IRLocations.render tree 12
val _ = check (text = "\n  attop r3\n  sat r4")
val table = IRLocations.table text spans
val _ = check (String.isSubstring "7\t3\t8\t2\t3\n" table)
val _ = check (String.isSubstring "8\t14\t6\t3\t3\n" table)
val {text,spans} = IRLocations.render (MARKED_LEAF (1,"\206\187")) 80
val _ = check (String.isSubstring "1\t1\t2\t2\t1\n" (IRLocations.table text spans))
val _ = print "IR location tables: PASS\n"
val object = OS.FileSys.tmpName ()
fun writeFile file contents =
  let val stream = TextIO.openOut file
  in TextIO.output (stream,contents); TextIO.closeOut stream
  end
fun readFile file =
  let val stream = TextIO.openIn file
  in TextIO.inputAll stream before TextIO.closeIn stream
  end
val _ = writeFile object "object\000bytes"
val document = {identity = "test-identity",unit = "unit",source = "/source/a file\n.sml",tree = tree,calls = [("function","caller",""),("direct","caller","callee"),("indirect","callee","")]}
val _ = IRLocations.write {object = object, document = document}
val ir = readFile (object ^ ".ir")
val _ = check (IRLocations.consistent object)
val fields = String.tokens Char.isSpace
val lines = String.fields (fn c => c = #"\n") ir
fun verifyRow line =
  case map Int.fromString (fields line) of
      [SOME mark,SOME start,SOME length,SOME row,SOME column] =>
        (check (String.substring (ir,start,length) =
                  (if mark = 7 then "attop r3" else "sat r4"));
         check (String.substring (List.nth (lines,row-1),column-1,length) =
                String.substring (ir,start,length)))
    | _ => ()
val _ = List.app verifyRow lines
val _ = writeFile (object ^ ".ir") (ir ^ "corruption")
val _ = check (not (IRLocations.consistent object))
val _ = writeFile (object ^ ".ir")
  (String.translate (fn #"7" => "9" | c => String.str c) ir)
val _ = check (not (IRLocations.consistent object))
val _ = writeFile (object ^ ".ir") ir
val moved = object ^ "-relocated"
val _ = OS.FileSys.rename {old = object, new = moved}
val _ = OS.FileSys.rename {old = object ^ ".ir", new = moved ^ ".ir"}
val _ = check (IRLocations.consistent moved)
val _ = OS.FileSys.rename {old = moved, new = object}
val _ = OS.FileSys.rename {old = moved ^ ".ir", new = object ^ ".ir"}
val _ = writeFile object "different object"
val _ = check (not (IRLocations.consistent object))
val _ = OS.FileSys.remove (object ^ ".ir")
val _ = check (not (IRLocations.consistent object))
val _ = OS.FileSys.remove object
val _ = print "IR artifact identity: PASS\n"
