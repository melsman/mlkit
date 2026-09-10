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
