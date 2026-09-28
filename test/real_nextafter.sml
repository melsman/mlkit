fun check (name, b) = print (name ^ ": " ^ (if b then "OK\n" else "FAIL\n"))
val p = Real.posInf
val n = Real.negInf
val m = Real.maxFinite
val () = check ("inward", List.all (fn d => Real.== (Real.nextAfter (p,d),m)) [0.0,1.0,n]
    andalso List.all (fn d => Real.== (Real.nextAfter (n,d),~m)) [0.0,~1.0,p])
val () = check ("outward", Real.== (Real.nextAfter (m,p),p)
    andalso Real.== (Real.nextAfter (~m,n),n))
val () = check ("equal infinities", Real.== (Real.nextAfter (p,p),p)
    andalso Real.== (Real.nextAfter (n,n),n))
val nan = 0.0 / 0.0
val () = check ("NaN", List.all (fn x => Real.isNan (Real.nextAfter (x,nan))
    andalso Real.isNan (Real.nextAfter (nan,x))) [p,n,0.0,1.0])
val () = check ("zero steps", Real.== (Real.nextAfter (0.0,p),Real.minPos)
    andalso Real.== (Real.nextAfter (0.0,n),~Real.minPos))
val () = check ("equal zeros", Real.signBit (Real.nextAfter (0.0,~0.0))
    andalso not (Real.signBit (Real.nextAfter (~0.0,0.0))))
