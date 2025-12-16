local
  val r = ref ([]:string list)
in
  fun memo_id x = (r:= x:: !r; x)
end
val y = memo_id "abc"
val z = memo_id "efg";
