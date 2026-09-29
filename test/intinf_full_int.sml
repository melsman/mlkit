fun check (name, b) = print (name ^ ": " ^ (if b then "OK\n" else "FAIL\n"))
fun roundtrip (i:int) =
    IntInf.toString (IntInf.fromInt i) = Int.toString i andalso
    IntInf.toInt (valOf (IntInf.fromString (Int.toString i))) = i
val lo = valOf Int.minInt
val hi = valOf Int.maxInt
val () = check ("int boundaries", List.all roundtrip [lo,lo+1,~1,0,1,hi-1,hi])
val p31 = IntInf.pow (2,31)
val () = check ("32-bit boundary", List.all (fn n => roundtrip (Int.fromLarge n))
    [p31-1,p31,p31+1,~p31-1,~p31,~p31+1])
fun over n = (IntInf.toInt n; false) handle Overflow => true
val () = check ("out of range", over (Int.toLarge lo-1) andalso over (Int.toLarge hi+1)
    andalso over (IntInf.pow (2,100)) andalso over (~(IntInf.pow (2,100))))
val () = check ("LargeInt", LargeInt.toInt (LargeInt.fromInt hi) = hi)
val t = Time.fromSeconds p31
val () = check ("Time conversions", Time.toSeconds t = p31 andalso
    Time.toMicroseconds t = p31 * 1000000 andalso Time.toString t = "2147483648.000" andalso
    Time.toSeconds (Time.fromReal 2147483648.0) = p31)
val () = check ("Time arithmetic", Time.toSeconds (Time.+ (Time.fromSeconds (p31-1),
    Time.fromSeconds 1)) = p31 andalso Time.toSeconds (Time.fromSeconds (~p31-1)) = ~p31-1)
val d = Date.date {year = 2100, month = Date.Jan, day = 1, hour = 0,
                   minute = 0, second = 0, offset = SOME Time.zeroTime}
val () = check ("Date 2100", Date.year (Date.fromTimeUniv (Date.toTime d)) = 2100)
