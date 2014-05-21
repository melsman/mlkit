signature ARROW0 = sig
  type ('b,'c) arr
  val arr : (''b -> ''c) -> (''b,''c) arr
  val >>> : (''b,''c)arr * (''c,''d)arr -> (''b,''d)arr                                       
  val fst : (''b,''c)arr -> (''b*''d,''c*''d)arr
end

signature ARROW = sig
  include ARROW0
  val snd : (''b,''c)arr -> (''d*''b,''d*''c)arr
  val *** : (''b,''c)arr * (''d,''e)arr -> (''b*''d,''c*''e)arr
  val &&& : (''b,''c)arr * (''b,''d)arr -> (''b,''c*''d)arr
end

signature BEHAVIOR =
sig
  type 'a t
  include ARROW where type ('a,'b)arr = 'a t -> 'b t

  val addListener : 'a t -> ('a -> unit) -> unit
  val insertDOM   : string t * string -> unit
  val timer       : int -> Time.time t
  val textField   : string -> string t
  val mouseOver   : string -> bool t
  val mouse       : unit -> (int*int)t
  val new         : ''a -> ''a t
  val pair        : ''a t * ''b t -> (''a * ''b) t
  val delay       : int -> (''a,''a)arr
  val calm        : int -> (''a,''a)arr
  val fold        : (''a * ''b -> ''b) -> ''b -> (''a,''b)arr
end

(*
functor Arrow0( X : sig type 'a t 
                        val arr : ('a -> 'b) -> 'a t -> 'b t 
                    end ) : ARROW0 =
struct
  open X
  type ('a,'b)arr = 'a t -> 'b t
  infix >>>
  fun a1 >>> a2 = a2 o a1
  fun fst (f:('b,'c)arr) : ('b*'d,'c*'d)arr = fn (a,b):('b*'d)t => ((f a,b):('c*'d)t)
end

functor Arrow (A0 : ARROW0) : ARROW = struct
  fun swap (a,b) = (b,a)
  open A0 infix >>> *** &&&
  fun snd f = arr swap >>> fst f >>> arr swap
  fun f *** g = fst f >>> snd g
  fun f &&& g = arr (fn b => (b,b)) >>> (f *** g) 
end
*)

structure Behavior :> BEHAVIOR =
struct
type 'a t = {listeners: ('a -> unit) list ref,
             newValue : 'a -> unit,
             current: 'a ref, 
             eq: 'a * 'a -> bool}

fun new (init:''a) : ''a t =
    let val listeners = ref nil
        val current = ref init
    in
      {listeners=listeners,
       eq=op =,
       current=current,
       newValue=fn v => if v = !current then ()
                        else (current := v; 
                              app (fn f => f v) (rev(!listeners)))}
    end

fun addListener ({listeners,...}:'a t) f =
    listeners := (f :: (!listeners))

fun fstT (eP : (''a*''b)t) : ''a t =
    let val (v1,_) = !(#current eP)
        val e : ''a t = new v1
        val _ = addListener eP (#newValue e o #1)
    in e
    end

fun sndT (eP : (''a*''b)t) : ''b t =
    let val (_,v2) = !(#current eP)
        val e : ''b t = new v2
        val _ = addListener eP (#newValue e o #2)
    in e
    end

fun pairT (e1: ''a t, e2: ''b t) : (''a*''b)t =
    let 
      val e : (''a*''b) t = new(!(#current e1),!(#current e2))
      val _ = addListener e1 (fn v1: ''a => #newValue e (v1,!(#current e2)))
      val _ = addListener e2 (fn v2: ''b => #newValue e (!(#current e1),v2))
    in e
    end
val pair = pairT

fun insertDOM (b : string t, id : string) =
    case Js.getElementById Js.document id of
      SOME e => (Js.innerHTML e (!(#current b));
                 addListener b (Js.innerHTML e))
    | NONE => raise Fail "Behavior.insertDOM error"

fun timer (milis:int) : Time.time t =
    let val b = new(Time.now())
      (* This could  be optimized so that we don't do unnecessary 
       * Time.now work when there is no listeners... *)
        val _ = Js.setInterval milis (fn() => #newValue b (Time.now()))
    in b
    end

fun delay (milis:int) (b : ''a t) : ''a t =
    let val b' = new(!(#current b))
        val _ = addListener b (fn v =>
                                  (Js.setTimeout milis (fn () => #newValue b' v); ()))
    in b'
    end

fun calm (milis:int) (b : ''a t) : ''a t =
    let val b' = new(!(#current b))
        val c = ref 0
        fun incr c = c := !c + 1
        fun decr c = (c := !c - 1; !c = 0) 
        val _ = addListener b (fn v => 
                                  (incr c;
                                  (Js.setTimeout milis (fn () => 
                                                           if decr c then #newValue b' v
                                                           else ())); ()))
    in b'
    end

fun textField (id:string) : string t =
    case Js.getElementById Js.document id of
      SOME e => let
                  val b = new (Js.value e)
                  fun f () = (#newValue b (Js.value e); true)
                  val () = Js.installEventHandler e Js.onkeyup f                                                   
                in b
                end 
    | NONE => raise Fail "Behavior.textField error"

fun mouseOver (id:string) : bool t =
    case Js.getElementById Js.document id of
      SOME e => let
                  val b = new false
                  fun f over () = (#newValue b over; true)
                  val () = Js.installEventHandler e Js.onmouseover (f true)
                  val () = Js.installEventHandler e Js.onmouseout (f false)
                in b
                end 
    | NONE => raise Fail "Behavior.mouseOver error"

fun mouse() : (int*int)t =
    let val b = new(0,0)
        val () = Js.onMouseMove Js.document (#newValue b)
    in b
    end

fun fold (f:''a*''b -> ''b) (x:''b) (a:''a t) : ''b t =
    let val b : ''b t = new x
      val _ = addListener a (fn v => #newValue b (f(v,!(#current b))))
    in b
    end

type ('a,'b) arr = 'a t -> 'b t

fun arr (f: ''a -> ''b) (b0:''a t) =
    let val b = new(f(!(#current b0)))
      val _ = addListener b0 (fn v => #newValue b (f v))
    in b
    end

fun fst f = fn p => pairT(f(fstT p),sndT p)

infix >>> *** &&&

fun a1 >>> a2 = a2 o a1

fun snd f = 
    let fun swap (a,b) = (b,a)
    in arr swap >>> fst f >>> arr swap
    end

fun f *** g = fst f >>> snd g

fun f &&& g = arr (fn b => (b,b)) >>> (f *** g) 

end
