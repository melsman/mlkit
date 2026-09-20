(* Times may be negative; conversions round towards zero; fmt takes
 * only a non-negative number of digits; scan accepts a sign. *)
fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
val secs = Time.fromSeconds o LargeInt.fromInt
val ms = Time.fromMilliseconds o LargeInt.fromInt
fun L i = LargeInt.toString i
val () = p ("negative: " ^ L (Time.toSeconds (secs ~1)) ^ " " ^ b (Time.< (secs ~1, Time.zeroTime)) ^ " " ^ Time.toString (secs ~1) ^ " " ^ Time.toString (ms ~1500) ^ " " ^ Time.toString (ms ~1) ^ " " ^ Time.toString (Time.fromMicroseconds (LargeInt.fromInt ~1)))
val () = p ("arithmetic: " ^ Time.toString (Time.+ (secs 1, secs 2)) ^ " " ^ Time.toString (Time.- (secs 3, secs 2)) ^ " " ^ Time.toString (Time.- (secs 2, secs 3)) ^ " " ^ Time.toString (Time.+ (ms ~1500, ms 700)) ^ " " ^ Time.toString (Time.- (ms 1, ms 2)) ^ " " ^ Time.toString (Time.+ (ms ~1, ms 1)))
val () = p ("towards zero: " ^ L (Time.toSeconds (ms ~1500)) ^ " " ^ L (Time.toSeconds (ms ~1999)) ^ " " ^ L (Time.toSeconds (ms ~999)) ^ " " ^ L (Time.toSeconds (ms 999)) ^ " " ^ L (Time.toMilliseconds (Time.fromMicroseconds (LargeInt.fromInt ~1999))) ^ " " ^ L (Time.toMicroseconds (Time.fromNanoseconds (LargeInt.fromInt ~1999))))
val () = p ("units: " ^ L (Time.toSeconds (ms 2010)) ^ " " ^ L (Time.toMilliseconds (ms 2010)) ^ " " ^ L (Time.toMicroseconds (ms 2010)) ^ " " ^ L (Time.toNanoseconds (ms 2010)) ^ " " ^ L (Time.toNanoseconds (ms ~2010)))
fun sz f = (f ()) handle Size => "Size"
val () = p ("fmt: " ^ sz (fn () => Time.fmt ~1 (secs 1)) ^ " " ^ Time.fmt 0 (ms 1500) ^ " " ^ Time.fmt 0 (ms 1499) ^ " " ^ Time.fmt 1 (ms 1450) ^ " " ^ Time.fmt 2 (ms 1005) ^ " " ^ Time.fmt 6 (ms 1) ^ " " ^ Time.fmt 8 (ms 1) ^ " " ^ Time.fmt 3 (ms ~14824) ^ " " ^ Time.fmt 0 (ms ~500) ^ " " ^ Time.fmt 2 (ms ~3873))
fun sc s = (case Time.fromString s of NONE => "NONE" | SOME t => Time.toString t)
val () = p ("scan: " ^ String.concatWith " " (map sc ["+1.5", "-1.5", "~1.5", "1.", ".5", "  2", "1.0000005", "1.0000004", "abc", "", "-0.25", "-", "."]))
val () = p ("scan rest: " ^ (case Time.scan Substring.getc (Substring.full "1.5x") of SOME (t, rest) => Time.toString t ^ " \"" ^ Substring.string rest ^ "\"" | NONE => "NONE"))
val () = p ("real: " ^ Real.toString (Time.toReal (Time.fromReal ~2.5)) ^ " " ^ Real.toString (Time.toReal (Time.fromReal 1.5)) ^ " " ^ Time.toString (Time.fromReal ~0.0000004) ^ " " ^ Time.toString (Time.fromReal 1234.5678))
val () = p ("compare: " ^ b (Time.< (ms ~1, ms 1)) ^ " " ^ b (Time.<= (ms ~1000, secs ~1)) ^ " " ^ b (Time.> (ms ~999, secs ~1)) ^ " " ^ (case Time.compare (ms ~2, ms ~1) of LESS => "LESS" | EQUAL => "EQUAL" | GREATER => "GREATER") ^ " " ^ b (Time.+ (secs ~5, secs 5) = Time.zeroTime))
val () = p ("now is positive: " ^ b (Time.> (Time.now (), secs 1000000000)))
val () = p ("Time: " ^ ((Time.toString (Time.fromSeconds (IntInf.pow (IntInf.fromInt 2, 100))); "no exception") handle Time.Time => "Time"))
