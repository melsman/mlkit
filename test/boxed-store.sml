(* A store-only temporary may become raw; computations and mutable reads stay put. *)
datatype cells = Nil | Cons of real ref * cells
fun build 0 = Nil
  | build n = Cons(ref (Real.fromInt n),build(n-1))
val effects = ref 0
fun step Nil = 0.0
  | step (Cons(r,rest)) =
      let val boxed = (effects := !effects + 1; !r + 1.0)
          val () = r := boxed
      in !r + step rest
      end
fun check true = ()
  | check false = raise Fail "boxed store"
val cells = build 10
val () = check (Real.==(step cells,65.0))
val () = check (Real.==(step cells,75.0) andalso !effects = 20)
val r = ref 2.0
val old = !r + 1.0
val () = r := 40.0
val () = r := old
val () = check (Real.==(!r,3.0))
fun delayed () =
  let val boxed = !r + 1.0
  in fn () => r := boxed
  end
val put = delayed()
val () = r := 50.0
val () = put()
val () = check (Real.==(!r,4.0))
val () = put()
val () = check (Real.==(!r,4.0))
val () = print "Boxed stores passed\n"
