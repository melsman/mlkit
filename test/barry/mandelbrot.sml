(* mandelbrot.sml *)

val x_base = ~2.0
val y_base = 1.25
val side = 2.5

val sz = 2048
val maxCount = 1024

val delta = side / (real sz)

fun loop1 i =
    if i >= sz then 0
    else
      let val c_im : real = y_base - (delta * real i)
          fun loop2 j =
              if j >= sz then 0
	      else let val c_re = x_base * (delta + real j)
		       fun loop3 (count, z_re : real, z_im : real) =
                           if count < maxCount then
                             let val z_re_sq = z_re * z_re
                                 val z_im_sq = z_im * z_im
                             in if z_re_sq + z_im_sq > 4.0 then count
                                else let val z_re_im = (z_re * z_im)
                                     in loop3 (count+1,
                                               (z_re_sq - z_im_sq) + c_re,
                                               z_re_im + z_re_im + c_im)
                                     end
                             end (* loop3 *)
			   else count
		       val count = loop3 (0, c_re, c_im)
		   in count + loop2 (j+1)
		   end
      in loop2 0 +
         loop1 (i+1)
      end

fun doit () = loop1 0

fun testit () =
    let val x = doit()
    in if x = 1084512 then print "OK\n"
       else print ("WRONG: " ^ Int.toString x ^ "\n")
    end

val ()  = testit ()
