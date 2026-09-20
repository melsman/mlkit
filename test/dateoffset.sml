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
