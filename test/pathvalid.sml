(* joinDirFile and toString raise InvalidArc on an arc containing the
 * separator. *)
fun p s = print (s ^ "\n")
fun t f = (f ()) handle OS.Path.InvalidArc => "InvalidArc" | OS.Path.Path => "Path"
val () = p (t (fn () => OS.Path.joinDirFile {dir = "x", file = "a/b"}))
val () = p (t (fn () => OS.Path.joinDirFile {dir = "x", file = "ab"}))
val () = p (t (fn () => OS.Path.joinDirFile {dir = "x", file = ""}))
val () = p (t (fn () => OS.Path.joinDirFile {dir = "", file = "ab"}))
val () = p (t (fn () => OS.Path.joinDirFile {dir = "/x/y", file = "/ab"}))
val () = p (t (fn () => OS.Path.toString {isAbs = false, vol = "", arcs = ["a/b"]}))
val () = p (t (fn () => OS.Path.toString {isAbs = true, vol = "", arcs = ["a", "b/c", "d"]}))
val () = p (t (fn () => OS.Path.toString {isAbs = true, vol = "", arcs = ["a", "", "d"]}))
val () = p (t (fn () => OS.Path.toString {isAbs = false, vol = "", arcs = []}))
val () = p (t (fn () => OS.Path.joinBaseExt {base = "a/b", ext = SOME "c"}))
