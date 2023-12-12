local

structure B = struct
  type t = string
  fun sub (t:t,i:int) : real = prim("__blockf64_sub_real", (t,i))
  fun upd (t:t,i:int,v:real) : unit = prim ("__blockf64_update_real", (t,i,v))
end

structure B2 = struct type t = B.t
                      fun sub0 t = B.sub(t,0)
                      fun sub1 t = B.sub(t,1)
                      fun upd0 (t,v) = B.upd(t,0,v)
                      fun upd1 (t,v) = B.upd(t,1,v)
               end
structure B3 = struct open B2 fun sub2 t = B.sub(t,2) and upd2 (t,v) = B.upd(t,2,v) end
structure B4 = struct open B3 fun sub3 t = B.sub(t,3) and upd3 (t,v) = B.upd(t,3,v) end
structure B5 = struct open B4 fun sub4 t = B.sub(t,4) and upd4 (t,v) = B.upd(t,4,v) end
structure B6 = struct open B5 fun sub5 t = B.sub(t,5) and upd5 (t,v) = B.upd(t,5,v) end
structure B7 = struct open B6 fun sub6 t = B.sub(t,6) and upd6 (t,v) = B.upd(t,6,v) end
structure B8 = struct open B7 fun sub7 t = B.sub(t,7) and upd7 (t,v) = B.upd(t,7,v) end
structure B9 = struct open B8 fun sub8 t = B.sub(t,8) and upd8 (t,v) = B.upd(t,8,v) end
structure B10 = struct open B9 fun sub9 t = B.sub(t,9) and upd9 (t,v) = B.upd(t,9,v) end
structure B11 = struct open B10 fun sub10 t = B.sub(t,10) and upd10 (t,v) = B.upd(t,10,v) end

in

structure Real64Block2 :> REAL64_BLOCK2 = struct
open B2
fun pack (r0:real,r1:real) : t = prim ("__blockf64", (r0,r1))
fun unpack t = (sub0 t, sub1 t)
end

structure Real64Block3 :> REAL64_BLOCK3 = struct
open B3
fun pack (r0:real,r1:real,r2:real) : t = prim ("__blockf64", (r0,r1,r2))
fun unpack t = (sub0 t, sub1 t, sub2 t)
end

structure Real64Block4 :> REAL64_BLOCK4 = struct
open B4
fun pack (r0:real,r1:real,r2:real,r3:real) : t = prim ("__blockf64", (r0,r1,r2,r3))
fun unpack t = (sub0 t, sub1 t, sub2 t, sub3 t)
end

structure Real64Block5 :> REAL64_BLOCK5 = struct
open B5
fun pack (r0:real,r1:real,r2:real,r3:real,r4:real) : t = prim ("__blockf64", (r0,r1,r2,r3,r4))
fun unpack t = (sub0 t, sub1 t, sub2 t, sub3 t, sub4 t)
end

structure Real64Block6 :> REAL64_BLOCK6 = struct
open B6
fun pack (r0:real,r1:real,r2:real,r3:real,r4:real,r5:real) : t = prim ("__blockf64", (r0,r1,r2,r3,r4,r5))
fun unpack t = (sub0 t, sub1 t, sub2 t, sub3 t, sub4 t, sub5 t)
end

structure Real64Block7 :> REAL64_BLOCK7 = struct
open B7
fun pack (r0:real,r1:real,r2:real,r3:real,r4:real,r5:real,r6:real) : t =
    prim ("__blockf64", (r0,r1,r2,r3,r4,r5,r6))
fun unpack t = (sub0 t, sub1 t, sub2 t, sub3 t, sub4 t, sub5 t, sub6 t)
end

structure Real64Block8 :> REAL64_BLOCK8 = struct
open B8
fun pack (r0:real,r1:real,r2:real,r3:real,r4:real,r5:real,r6:real,r7:real) : t =
    prim ("__blockf64", (r0,r1,r2,r3,r4,r5,r6,r7))
fun unpack t = (sub0 t, sub1 t, sub2 t, sub3 t, sub4 t, sub5 t, sub6 t, sub7 t)
end

structure Real64Block9 :> REAL64_BLOCK9 = struct
open B9
fun pack (r0:real,r1:real,r2:real,r3:real,r4:real,r5:real,r6:real,r7:real,r8:real) : t =
    prim ("__blockf64", (r0,r1,r2,r3,r4,r5,r6,r7,r8))
fun unpack t = (sub0 t, sub1 t, sub2 t, sub3 t, sub4 t, sub5 t, sub6 t, sub7 t, sub8 t)
end

structure Real64Block10 :> REAL64_BLOCK10 = struct
open B10
fun pack (r0:real,r1:real,r2:real,r3:real,r4:real,r5:real,r6:real,r7:real,r8:real,r9:real) : t =
    prim ("__blockf64", (r0,r1,r2,r3,r4,r5,r6,r7,r8,r9))
fun unpack t = (sub0 t, sub1 t, sub2 t, sub3 t, sub4 t, sub5 t, sub6 t, sub7 t, sub8 t, sub9 t)
end

structure Real64Block11 :> REAL64_BLOCK11 = struct
open B11
fun pack (r0:real,r1:real,r2:real,r3:real,r4:real,r5:real,r6:real,r7:real,r8:real,r9:real,r10:real) : t =
    prim ("__blockf64", (r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10))
fun unpack t = (sub0 t, sub1 t, sub2 t, sub3 t, sub4 t, sub5 t, sub6 t, sub7 t, sub8 t, sub9 t, sub10 t)
end

end
