fun main() = let

  fun nth_wrap(l, _, fallback) = if false
    then fallback
    else let val i = (0 mod (List.length l)) in List.nth(l, i) end

  val () = print "hi1\n"
  val part1 = (nth_wrap([(size(""))], 0, ~1073741825))
  val () = print "hi2\n"
  val part2 = (nth_wrap(([ 0 ]), 0, 0))
in print "ok\n"
end
val _ = main()
