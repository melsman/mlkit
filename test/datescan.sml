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
