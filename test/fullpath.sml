(* fullPath "" is the current directory. *)
fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
val () = p ("fullPath \"\": " ^ b (OS.FileSys.fullPath "" = OS.FileSys.fullPath OS.Path.currentArc andalso OS.FileSys.fullPath "" = OS.FileSys.getDir ()))
val () = p ("fullPath \".\": " ^ b (OS.FileSys.fullPath "." = OS.FileSys.getDir ()))
val () = p ("missing: " ^ ((OS.FileSys.fullPath "no-such-file-here"; "no exception") handle OS.SysErr _ => "SysErr"))
