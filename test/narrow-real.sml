(* Run without the Basis: these are compiler types and primitives. *)
infix 7 * / mod
infix 6 + -
infix 4 = < > <= >=
fun op = (x:''a, y:''a) : bool = prim ("=", (x,y))
fun not true = false
  | not false = true
fun print (s:string) : unit = prim ("printStringML", s)
fun assert true = ()
  | assert false = raise Div
fun bits32 (x:real32) : word = prim ("id", x)
fun bits16 (x:real16) : word = prim ("id", x)
fun eq32 (x:real32, y:real32) : bool = prim ("__equal_real32", (x,y))
fun ne32 (x:real32, y:real32) : bool = prim ("__unequal_real32", (x,y))
fun sqrt32 (x:real32) : real32 = prim ("__sqrt_real32", x)
fun widen (x:real32) : real = prim ("__real32_to_real", x)
fun narrow (x:real) : real32 = prim ("__real_to_real32", x)
val _ = assert (bits32 (1.000000059604644775390625:real32) = 0w1065353216)
val _ = assert (bits32 (1.00000005960464477539062500001:real32) = 0w1065353217)
val _ = assert (bits16 (1.00048828125000000000001:real16) = 0w15361)
val _ = assert (bits16 (~0.0:real16) = 0w32768)
val _ = assert (bits32 (~0.0:real32) = 0w2147483648)
val _ = assert (bits16 (65504.0:real16) = 0w31743)
val _ = assert (bits16 (65520.0:real16) = 0w31744)
val _ = assert (bits16 (0.000000059604644775390625:real16) = 0w1)
val _ = assert (bits16 (0.0000000298023223876953125:real16) = 0w0)
val _ = assert (bits16 (0.00000002980232238769531250000000001:real16) = 0w1)
val _ = assert (bits32 (1.40129846432481707092372958328991613e~45:real32) = 0w1)
val _ = assert (bits32 (3.40282346638528859811704183484516925e38:real32) = 0w2139095039)
fun calc (x:real32, y:real32) = (x+y, x-y, x*y, x/y)
val (a,b,c,d) = calc (7.0,2.0)
val _ = assert (eq32(a,9.0) andalso eq32(b,5.0) andalso eq32(c,14.0) andalso eq32(d,3.5))
val _ = assert (bits32 (~(0.0:real32)) = 0w2147483648)
val _ = assert (bits32 (abs (~0.0:real32)) = 0w0)
val _ = assert (eq32(sqrt32 4.0,2.0))
val zero : real32 = 0.0
val nan = zero/zero
val _ = assert (not (nan < zero) andalso not (nan <= zero) andalso
                not (nan > zero) andalso not (nan >= zero))
val _ = assert (not (eq32(nan,nan)) andalso ne32(nan,nan) andalso eq32(zero,~zero))
val _ = assert (widen (narrow 1.5) > 1.4)
datatype payload = Empty | Single of real32 | Half of real16
fun keep x = x
val _ = assert (case keep (Single d) of Single x => eq32(x,3.5) | _ => false)
val _ = assert (case keep (Half 1.0) of Half x => bits16 x = 0w15360 | _ => false)
(* Exercise low-unboxed values in a recursive call with spilled arguments. *)
fun fromInt32 (i:int) : real32 = prim ("__int_to_real32", i)
fun many (n,a,b,c,d,e,f,g,h,flag,x:real32,y:real16) =
    if n = 0 then
      (assert (b = a+1 andalso c = a+2 andalso d = a+3 andalso e = a+4 andalso
               f = a+5 andalso g = a+6 andalso h = a+7);
       assert (flag = (a mod 2 = 1));
       assert (eq32(x,fromInt32 a));
       assert (bits16 y = (if flag then 0w16384 else 0w15360)))
    else
      (many(n-1,a+1,b+1,c+1,d+1,e+1,f+1,g+1,h+1,not flag,x+1.0,
            if flag then 1.0 else 2.0);
       assert (b = a+1 andalso flag = (a mod 2 = 1)))
val seed : int = prim ("get_time_base", 0:int)
val () = many(seed mod 3 + 1,1,2,3,4,5,6,7,8,true,1.0,2.0)
fun eq16 (x:real16,y:real16) : bool = prim ("__equal_real16", (x,y))
fun calc16 (x:real16,y:real16) = (x+y,x-y,x*y,x/y)
val (ha,hb,hc,hd) = calc16(7.0,2.0)
val _ = assert (eq16(ha,9.0) andalso eq16(hb,5.0) andalso
                eq16(hc,14.0) andalso eq16(hd,3.5))
val _ = assert (bits16 (~(0.0:real16)) = 0w32768 andalso bits16 (abs (~0.0:real16)) = 0w0)
val halfZero : real16 = 0.0
val halfNan = halfZero/halfZero
val _ = assert (not (halfNan < halfZero) andalso not (halfNan <= halfZero) andalso
                not (halfNan > halfZero) andalso not (halfNan >= halfZero) andalso
                not (eq16(halfNan,halfNan)))
(* Unconstrained reals still default to ordinary real. *)
val default = 1.0/2.0
val _ : real = default
val _ = print "OK\n"
