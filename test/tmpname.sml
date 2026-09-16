(* tmpName creates the file it names. *)
fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
val n = OS.FileSys.tmpName ()
val () = p ("exists: " ^ b (OS.FileSys.access (n, [])))
val () = p ("empty: " ^ b (OS.FileSys.fileSize n = 0))
val () = p ("absolute: " ^ b (OS.Path.isAbsolute n))
val () = p ("another name: " ^ b (OS.FileSys.tmpName () <> n))
val () = OS.FileSys.remove n
val () = p ("removed: " ^ b (not (OS.FileSys.access (n, []))))
