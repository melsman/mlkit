fun f (a:real) = ()            (* f makes no use of its argument *)
(*
f: all r7,e8.(real,r7)-e8(U(U))->unit
*)

functor F1() = struct end      (* line inserted to avoid in-lining *)

fun g v = (fn () => f v)       (* g returns a closure containing v, which is a 
			        * pointer if v is boxed. *)
(*
g: all r7,r8,e9,e10.(real,r8)-e9(put(r7))->(unit-e10(U(get(r1)))->unit,r7) 
*)

functor F2() = struct end

val h = g (1.0+1.0)            (* boxed real in finite region; h is now bound 
				* to a closure containing a dangling pointer, 
				* because region inference puts a letregion 
				* around the call to g to hold the real. *)
(*
h: (unit-e6(put(r3),put(r4),put(r5),get(r1),get(@r3),
            get(@r4),get(@r5),put(@r1))->unit,r1)
*)


fun mk 0 = nil                 (* do some work to trigger gc *)
  | mk i = i :: mk(i-1)
fun revn (0,l) = l
  | revn (n,l) = revn(n-1,rev l) 
val _ = revn(1000, mk 10000)


(*
fun f :all r7,e8.(real,r7)-e8(U(U))->unit attop r1 [] [r7:inf] (a)= ()

fun g :all r7,r8,e9,e10.(real,r8)-e9(put(r7))->
                        (unit-e10(U(get(r1)))->unit,r7) 
  attop r1 
  [r7:4] 
  [r8:inf] 
  (v)= 
  fn e10 attop r7 v190074:unit => jmp f[] [] [r8] [e11] v

 val h:(unit-e6(put(r3),put(r4),put(r5),get(r1),get(@r3),
                get(@r4),get(@r5),put(@r1))->unit,r1) = 
           letregion r8:4 
           in funcall g[attop r1] 
                       [] 
                       [r1,r8] 
                       [e7(put(r1)),e6(put(r3),put(r4),put(r5),get(r1),
                           get(@r3),get(@r4),get(@r5),put(@r1))] 
                      letregion r10:0, r11:0 
                      in ccall(__plus_real,  atbot r8, 1.0atbot r11, 
                               1.0atbot r10):(real,r8) 
                      end (*r10:0, r11:0*) 
           end (*r8:4*); 
*)