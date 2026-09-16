(* setPosOut restores an output position taken by getPosOut; the seek
 * was relative to the current position instead of absolute. *)
fun p s = print (s ^ "\n")
val path = "setposout.tmp"
val outs = TextIO.openOut path
val () = TextIO.output (outs, "abcdef")
val () = TextIO.flushOut outs
val pos = TextIO.getPosOut outs
val () = TextIO.output (outs, "ghi")
val () = TextIO.flushOut outs
val () = TextIO.setPosOut (outs, pos)
val () = TextIO.output (outs, "XYZ")
val () = TextIO.closeOut outs
val ins = TextIO.openIn path
val () = p ("\"" ^ String.toString (TextIO.inputAll ins) ^ "\"")
val () = TextIO.closeIn ins
val () = OS.FileSys.remove path
val outs = BinIO.openOut path
val () = BinIO.output (outs, Byte.stringToBytes "0123456789")
val pos = BinIO.getPosOut outs
val () = BinIO.output (outs, Byte.stringToBytes "abc")
val () = BinIO.setPosOut (outs, pos)
val () = BinIO.output (outs, Byte.stringToBytes "Z")
val () = BinIO.closeOut outs
val ins = BinIO.openIn path
val () = p ("\"" ^ Byte.bytesToString (BinIO.inputAll ins) ^ "\"")
val () = BinIO.closeIn ins
val () = OS.FileSys.remove path
