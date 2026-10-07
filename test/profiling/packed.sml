fun check b = if b then () else raise Fail "packed descriptor"
val large = Array.tabulate (100000, fn i => i)
val text = CharVector.tabulate (600000, fn i => Char.chr (65 + i mod 26))
fun loop 0 = 0
  | loop n =
    let val xs = List.tabulate (1000, fn i => (i, i + 1, ref i))
        val ys = List.rev xs
        val (x,y,r) = hd ys
    in check (x = 999 andalso y = 1000 andalso !r = 999);
       x + loop (n - 1)
    end
val _ = check (loop 100 = 99900)
val _ = check (Array.sub (large, 99999) = 99999)
val _ = check (String.size text = 600000 andalso String.sub (text, 599999) = Char.chr (65 + 599999 mod 26))
val _ = print "packed descriptors: OK\n"
