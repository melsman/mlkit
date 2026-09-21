(* Use the production Thread wrapper: its types keep parent regions alive. *)
fun build__noinline(n:word,acc) =
  if eq(n,0w0) then acc else build__noinline(n-0w1,n::acc)
fun sum__noinline(nil,acc:word) = acc
  | sum__noinline(x::xs,acc) = sum__noinline(xs,acc+x)
fun work () = build__noinline(0w8192,nil)
fun shared () =
  Thread.spawn work (fn a =>
  Thread.spawn work (fn b =>
  Thread.spawn work (fn c =>
  Thread.spawn work (fn d =>
    let fun verify t = check(eq(sum__noinline(Thread.get t,0w0),0w33558528))
        val () = verify a
        val () = verify b
        val () = verify c
        val () = verify d
    in verify a end))))
fun nested n = if eq(n,0w0) then 0w7 else
  Thread.spawn (fn () => nested(n-0w1))
    (fn t => Thread.get t + Thread.get t)
exception E of word
fun exceptions () =
  let val v = (Thread.spawn (fn () => raise E 0w19)
                (fn t => Thread.get t) handle E n => n)
      val () = check(eq(v,0w19))
      val done = ref 0w0
      fun worker () = let val xs=work()
                      in (prim(":=",(done,sum__noinline(xs,0w0))):unit) end
      val () = (Thread.spawn worker (fn _ => raise E 0w1)
                handle E _ => check(eq(!done,0w33558528)))
  in () end
fun repeat n = if eq(n,0w0) then ()
               else (shared(); exceptions(); repeat(n-0w1))
(* Each worker creates a distinct dynamic exception constructor. *)
fun fresh__noinline () =
  let exception Local of word
      fun throw () : unit = raise Local 0w1
      fun catches (f:unit -> unit) =
        (f(); false) handle Local _ => true | _ => false
  in (throw,catches) end
fun names n = if eq(n,0w0) then () else
  (Thread.spawn fresh__noinline (fn a => Thread.spawn fresh__noinline (fn b =>
    let val (ta,ca) = Thread.get a
        val (tb,cb) = Thread.get b
    in check(ca ta); check(cb tb);
       check(if ca tb then false else true);
       check(if cb ta then false else true)
    end)); names(n-0w1))
val () = names 0w64
val () = repeat 0w8
val () = check(eq(nested 0w5,0w224))
val () = prim("printStringML","parallel ML passed\n")

(* A callback registered by the parent must use the worker's context. *)
exception Callback of int
fun hook (old:int) =
  ((let val n:int = prim("arm64_parallel_context",old)
    in raise Callback n end) handle Callback n => n)
val () = _export("arm64_parallel_hook",hook)
fun callback () =
  let val n:int = prim("arm64_parallel_callback",())
  in check(22 < n andalso n < 24) end
val () = Thread.spawn callback (fn a => Thread.spawn callback (fn b =>
  (Thread.get a; Thread.get b)))
val () = prim("printStringML","parallel callback passed\n")
