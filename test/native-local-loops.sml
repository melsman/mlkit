(* The result is used after the helper returns, so entry is not a tail call. *)
fun count__noinline (xs:int list,initial) =
    let
      fun loop ([]:int list,n) = n
        | loop (_ :: rest,n) = loop(rest,n+1)
      val n = loop(xs,initial)
    in n+1
    end

fun countWord__noinline (xs:int list) =
    let
      fun loop ([]:int list,n:word) = n
        | loop (_ :: rest,n) = loop(rest,n+0w1)
      val n = loop(xs,0w0)
    in n+0w1
    end

(* Allocation and extra live arguments must keep ordinary call/frame handling. *)
fun reverse__noinline xs =
    let
      fun loop ([],acc) = acc
        | loop (x::rest,acc) = loop(rest,x::acc)
      val ys = loop(xs,[])
    in (ys,length ys)
    end
fun sum__noinline xs =
    let
      fun loop ([],a,b,c,d) = a+b+c+d
        | loop (x::rest,a,b,c,d) = loop(rest,a+x,b+x,c+x,d+x)
      val n = loop(xs,0,1,2,3)
    in n+1
    end
val xs = List.tabulate(257,fn i => i)
val (ys,n) = reverse__noinline xs
val overflow = (count__noinline ([0],valOf Int.maxInt); false)
               handle Overflow => true
val () = if count__noinline (xs,0) = 258 andalso count__noinline ([],0) = 1
            andalso countWord__noinline xs = 0w258
            andalso overflow andalso n = 257
            andalso hd ys = 256 andalso sum__noinline xs = 131591
         then print "native local loops passed\n"
         else raise Fail "native local loops"
