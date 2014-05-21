
val r = ref ([]:string list)

fun memo_id x = (r:= x:: !r; x)

val y = memo_id "abc"
val z = memo_id "efg";