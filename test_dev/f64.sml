infix  7  * / div mod
infix  6  + -
infix  4  = <> > >= < <=

val a = (1.0,2.0,3.0)
val b = (4.0,5.0,6.0)

fun f () =
    let val v =
            let val v156 = #1(a)
                val v157 = #2(a)
                val v158 = #3(a)
                val v153 = #1(b)
                val v154 = #2(b)
                val v155 = #3(b)
            in  (v157 * v155 - v158 * v154,v158 * v153 - v156 * v155,v156 * v154 - v157 * v153)
            end
    in  let val v318 = #1(v) * #1(v)
            val v319 = #2(v) * #2(v)
            val v320 = #3(v) * #3(v)
        in  v318 + v319 + v320
        end
    end

fun printReal (n:real):unit = prim("printReal",n)

val () = printReal (f())  (* 54.00 *)
