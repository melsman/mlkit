local
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

in
end
local
(* Timer.checkCPUTimes splits the CPU time into collector and
 * non-collector parts that add up to checkCPUTimer; checkGCTime is the
 * collector's user time. *)
fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
fun burn 0 acc = acc
  | burn n acc = burn (n - 1) (acc + n mod 7)
val c = Timer.startCPUTimer ()
val _ = burn 3000000 0
val {nongc, gc} = Timer.checkCPUTimes c
val {usr, sys} = Timer.checkCPUTimer c
val direct = Timer.checkGCTime c
val nonneg = Time.>= (#usr nongc, Time.zeroTime) andalso Time.>= (#sys nongc, Time.zeroTime)
             andalso Time.>= (#usr gc, Time.zeroTime) andalso Time.>= (#sys gc, Time.zeroTime)
val () = p ("non-negative: " ^ b nonneg)
val () = p ("total covers the parts: " ^ b (Time.>= (usr, Time.+ (#usr nongc, #usr gc)) andalso Time.>= (sys, Time.+ (#sys nongc, #sys gc))))
val () = p ("gc time within total: " ^ b (Time.<= (#usr gc, Time.+ (usr, sys))))
val () = p ("checkGCTime later is not smaller: " ^ b (Time.>= (direct, #usr gc)))
val () = p ("real timer runs: " ^ b (Time.>= (Timer.checkRealTimer (Timer.startRealTimer ()), Time.zeroTime)))

in
end
local
(* Date.offset reports the offset a date was built with, west of UTC and
 * within a day; an offset beyond a day moves the date on. *)
fun p s = print (s ^ "\n")
fun so f NONE = "NONE"
  | so f (SOME x) = "SOME " ^ f x
fun mk (y, m, d, h, mi, s) = Date.date {year = y, month = m, day = d, hour = h, minute = mi, second = s, offset = SOME Time.zeroTime}
val hours = Time.fromSeconds o LargeInt.fromInt
fun off d = so (LargeInt.toString o Time.toSeconds) (Date.offset d)
fun withOffset secs = Date.date {year = 2000, month = Date.Jan, day = 10, hour = 12, minute = 0, second = 0, offset = SOME (hours secs)}
fun show d = off d ^ "/" ^ Int.toString (Date.day d) ^ "/" ^ Int.toString (Date.hour d)
val () = p ("offsets: " ^ String.concatWith " " (map (show o withOffset) [0, 3600, 43200, 50000, 86399, 90000, 86400 * 2 + 3600]))
val () = p ("local: " ^ so (LargeInt.toString o Time.toSeconds) (Date.offset (Date.date {year = 2000, month = Date.Jan, day = 1, hour = 0, minute = 0, second = 0, offset = NONE})))
val () = p ("utc: " ^ so (LargeInt.toString o Time.toSeconds) (Date.offset (mk (2000, Date.Jan, 1, 0, 0, 0))))

in
end
local
(* Date.toString has the shape of fmt "%a %b %d %H:%M:%S %Y", and in
 * fmt an unknown directive stands for its character. *)
fun p s = print (s ^ "\n")
fun so f NONE = "NONE"
  | so f (SOME x) = "SOME " ^ f x
fun mk (y, m, d, h, mi, s) = Date.date {year = y, month = m, day = d, hour = h, minute = mi, second = s, offset = SOME Time.zeroTime}
val d = mk (2000, Date.Jan, 1, 12, 30, 45)
val () = p ("toString: " ^ Date.toString d ^ " (" ^ Int.toString (size (Date.toString d)) ^ ") " ^ Date.toString (mk (1999, Date.Dec, 31, 23, 59, 59)) ^ " " ^ Date.toString (mk (2024, Date.Feb, 29, 0, 5, 7)))
val () = p ("fmt: " ^ Date.fmt "%a %b %d %H:%M:%S %Y" d ^ " | " ^ Date.fmt "%Y-%m-%d %j %I%p %y %%" (mk (2000, Date.Jan, 2, 9, 5, 7)) ^ " | " ^ Date.fmt "%q %~ %5 a%qb x%Yy %" d ^ "|" ^ Date.fmt "" d ^ "|")
val () = p ("unknown directives: " ^ (if List.all (fn c => Date.fmt ("%" ^ str c) d = str c) (List.filter (fn c => not (Char.contains "aAbBcdHIjmMpSUwWxXyYZ%" c)) (List.tabulate (94, fn i => Char.chr (i + 33)))) then "ok" else "bad"))

in
end
local
(* Date.scan skips leading whitespace and insists on the format that
 * toString writes. *)
fun p s = print (s ^ "\n")
fun so f NONE = "NONE"
  | so f (SOME x) = "SOME " ^ f x
fun mk (y, m, d, h, mi, s) = Date.date {year = y, month = m, day = d, hour = h, minute = mi, second = s, offset = SOME Time.zeroTime}
val d = mk (2000, Date.Jan, 1, 12, 30, 45)
fun sc s = so Date.toString (Date.fromString s)
val () = p ("scan: " ^ String.concatWith " | " (map sc ["Sat Jan 01 12:30:45 2000", "  Sat Jan 01 12:30:45 2000", "Jan 01 12:30:45 2000", "Sat Jan 1 12:30:45 2000", "Sat Jan 01 12:30:45", "Sat Jan  1 12:30:45 2000", "not a date", "", "Mon Feb 29 00:00:00 2024"]))
val () = p ("scan rest: " ^ (case Date.scan Substring.getc (Substring.full "Sat Jan 01 12:30:45 2000 tail") of SOME (d, rest) => Int.toString (Date.year d) ^ " \"" ^ Substring.string rest ^ "\"" | NONE => "NONE"))
val () = p ("round trip: " ^ (if Option.map Date.toString (Date.fromString (Date.toString d)) = SOME (Date.toString d) then "ok" else "bad"))

in
end
local
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

in
end
