fun repeat(x, 0) = []
  | repeat(x, n) = x :: repeat(x, n - 1)

val r = let val a = repeat(42, 3)
		in length (repeat(7, 5)) + length a
		end
