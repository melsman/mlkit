(* Auxiliary functions for test cases *)

fun check b = if b then "OK" else "WRONG"
fun tst0 s s' = print (s ^ " : " ^ s' ^ "</br>")
fun tst s b = tst0 s (check b)


(* test/real.sml -- PS 1995-03-24, 1996-05-16, 1996-07-02, 1996-09-25 *)

val _ = print "Testing structure PackRealBig...</br>"

local
    open Real PackRealBig
    infix ==
    fun inv r =
	let val _ = print ("[r = " ^ Real.toString r)
	    val bytes = toBytes r
	    val _ = print (", sz(vector) = " ^ Int.toString (Word8Vector.length bytes))
	    val bytes_s = Byte.bytesToString bytes
	    val _ = print (", sz(bs) " ^ Int.toString(size bytes_s))
	    val r' = fromBytes bytes
	    val _ = print (", r' = " ^ Real.toString r' ^ "]</br>")
	in r == r'
	end
in
val test1 = tst "test1" (inv 0.0 andalso inv 2.0 andalso inv 43.23 andalso inv ~43.2);
end
