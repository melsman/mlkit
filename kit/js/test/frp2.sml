signature ARROW0 = sig
  type ('b,'c,'k) arr
  val arr : (''b -> ''c) -> (''b,''c,'k) arr
  val >>> : (''b,''c,'k)arr * (''c,''d,'k)arr -> (''b,''d,'k)arr                                       
  val fst : (''b,''c,'k)arr -> (''b*''d,''c*''d,'k)arr
end

signature ARROW = sig
  include ARROW0
  val snd : (''b,''c,'k)arr -> (''d*''b,''d*''c,'k)arr
  val *** : (''b,''c,'k)arr * (''d,''e,'k)arr -> (''b*''d,''c*''e,'k)arr
  val &&& : (''b,''c,'k)arr * (''b,''d,'k)arr -> (''b,''c*''d,'k)arr
end

signature TIME_VAL =
sig
  type B type E (* kinds: Behaviors (B) and Events (E) *)
  type ('a,'k)t
  include ARROW where type ('a,'b,'k)arr = ('a,'k)t -> ('b,'k)t

  val insertDOM : (string,B)t * string -> unit
  val timer     : int -> (Time.time,B)t
  val textField : string -> (string,B)t
  val mouseOver : string -> (bool,B)t
  val mouse     : unit -> (int*int,B)t
  val pair      : (''a,B)t * (''b,B)t -> (''a * ''b,B)t
  val merge     : (''a,E)t * (''a,E)t -> (''a,E)t
  val delay     : int -> (''a,''a,B)arr
  val calm      : int -> (''a,''a,B)arr
  val fold      : (''a * ''b -> ''b) -> ''b -> (''a,'k)t -> (''b,B)t
  val click     : string -> ''a -> (''a,E)t
  val changes   : (''a,B)t -> (''a,E)t
  val hold      : ''a -> (''a,E)t -> (''a,B)t
  val const     : ''a -> (''a,B)t
  val empty     : unit -> (''a,E)t
  val current   : (''a,B)t -> ''a

  val addListener : (''a,'k)t -> (''a -> unit) -> unit 
end

structure TimeVal :> TIME_VAL =
struct

fun idError s id = 
    raise Fail (s ^ ": element with id=" ^ id ^ " not in dom")

datatype kind = Behavior | Event
type B = kind
type E = kind
type ('a,'k) t = 
     {listeners: ('a -> unit) list ref,
      newValue : 'a -> unit,
      current: 'a ref option}

fun new (init:''a option) : (''a,'k) t =
    let val listeners = ref nil
    in case init of
         SOME a =>
         let val current = ref a
         in {listeners=listeners,
             current=SOME current,
             newValue=fn v => if v = !current then ()
                              else (current := v; 
                                    app (fn f => f v) (rev(!listeners)))}
         end
       | NONE =>
         {listeners=listeners,
          current=NONE,
          newValue=fn v => app (fn f => f v) (rev(!listeners))}
    end

fun current ({current,...}:(''a,B)t) : ''a =
    case current of
      SOME(ref v) => v
    | NONE => raise Fail "current.impossible"

fun addListener ({listeners,...}: ('a,'k)t) f =
    listeners := (f :: (!listeners))

fun fstT (eP : (''a*''b,'k)t) : (''a,'k)t =
    let val v1opt = case #current eP of
                      SOME(ref(v1,_)) => SOME v1
                    | NONE => NONE
        val e : (''a,'k)t = new v1opt
        val _ = addListener eP (#newValue e o #1)
    in e
    end

fun sndT (eP : (''a*''b,'k)t) : (''b,'k)t =
    let val v2opt = case #current eP of
                      SOME(ref(_,v2)) => SOME v2
                    | NONE => NONE
        val e : (''b,'k)t = new v2opt
        val _ = addListener eP (#newValue e o #2)
    in e
    end

local 
  fun get(r as ref(x::xs)) = SOME x before r:=xs
    | get(ref nil) = NONE
  fun add(r) x = r := rev(x::(rev(!r)))
in
fun pairT (e1: (''a,'k)t, e2: (''b,'k)t) : (''a*''b,'k)t =
    case (#current e1, #current e2) of
      (SOME v1r, SOME v2r) => (* behaviors *)
      let val e : (''a*''b,'k) t = new (SOME(!v1r,!v2r))
          val _ = addListener e1 (fn v1: ''a => #newValue e (v1,!v2r))
          val _ = addListener e2 (fn v2: ''b => #newValue e (!v1r,v2))
      in e
      end      
    | (NONE,NONE) => (* event streams *)
      let val e1s = ref (nil : ''a list)
          val e2s = ref (nil : ''b list)
          val e : (''a*''b,'k)t = new NONE
          val _ = addListener e1 (fn v1: ''a => case get e2s of 
                                                  NONE => add e1s v1
                                                | SOME v2 => #newValue e (v1,v2))
          val _ = addListener e2 (fn v2: ''b => case get e1s of
                                                  NONE => add e2s v2
                                                | SOME v1 => #newValue e (v1,v2))
      in e
      end
    | _ => raise Fail "pairT.impossible"
end

val pair = pairT

fun merge (e1: (''a,E)t, e2: (''a,E)t) : (''a,E)t =
    let val e = new NONE
        val _ = addListener e1 (#newValue e)
        val _ = addListener e2 (#newValue e)
    in e
    end

fun insertDOM (b : (string,B)t, id : string) =
    case Js.getElementById Js.document id of
      SOME e => 
      (case #current b of
         SOME(ref v) => (Js.innerHTML e v;
                         addListener b (Js.innerHTML e))
       | NONE => raise Fail "insertDOM impossible")
    | NONE => idError "insertDOM" id

fun timer (milis:int) : (Time.time,B)t =
    let val b = new(SOME(Time.now()))
      (* This could  be optimized so that we don't do unnecessary 
       * Time.now work when there is no listeners... *)
        val _ = Js.setInterval milis (fn() => #newValue b (Time.now()))
    in b
    end

fun delay (milis:int) (b : (''a,B)t) : (''a,B)t =
    let val b' = new(case #current b of 
                       SOME(ref v) => SOME v 
                     | NONE => raise Fail "delay.impossible")
        val _ = addListener b (fn v =>
                                  (Js.setTimeout milis (fn () => #newValue b' v); ()))
    in b'
    end

fun calm (milis:int) (b : (''a,B)t) : (''a,B)t =
    let val b' = new(case #current b of
                       SOME(ref v) => SOME v 
                     | NONE => raise Fail "calm.impossible")
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

fun textField (id:string) : (string,B) t =
    case Js.getElementById Js.document id of
      SOME e => let
                  val b = new (SOME(Js.value e))
                  fun f () = (#newValue b (Js.value e); true)
                  val () = Js.installEventHandler e Js.onkeyup f                                                   
                in b
                end 
    | NONE => idError "textField" id

fun mouseOver (id:string) : (bool,B)t =
    case Js.getElementById Js.document id of
      SOME e => let
                  val b = new(SOME false)
                  fun f over () = (#newValue b over; true)
                  val () = Js.installEventHandler e Js.onmouseover (f true)
                  val () = Js.installEventHandler e Js.onmouseout (f false)
                in b
                end 
    | NONE => idError "mouseOver" id

fun mouse() : (int*int,B)t =
    let val b = new(SOME(0,0))
        val () = Js.onMouseMove Js.document (#newValue b)
    in b
    end

fun click (id:string) (a:''a) : (''a,E)t =
    case Js.getElementById Js.document id of
      SOME e => let val t = new NONE
                    val () = Js.installEventHandler e Js.onclick (fn() => (#newValue t a; true))
                in t
                end
    | NONE => idError "click" id

fun changes (b: (''a,B)t) : (''a,E)t =
    let val t = new NONE
        val _ = addListener b (#newValue t)
    in t
    end

fun hold (a : ''a) (e: (''a,E)t) : (''a,B)t =
    let val b = new(SOME a)
        val _ = addListener e (#newValue b)
    in b
    end

fun fold (f:''a*''b -> ''b) (x:''b) (a:(''a,'k)t) : (''b,B)t =
    let val b : (''b,B)t = new(SOME x)
        val _ = addListener a (fn v => 
                                  case #current b of
                                    SOME (ref c) => #newValue b (f(v,c))
                                  | NONE => raise Fail "fold.impossible")
    in b
    end

fun empty() : (''a,E)t = new NONE

fun const (a:''a) : (''a,B)t = new (SOME a)

type ('a,'b,'k) arr = ('a,'k)t -> ('b,'k)t

fun arr (f: ''a -> ''b) (b0:(''a,'k)t) =
    let val b = new(case #current b0 of
                      SOME(ref v) => SOME (f v)
                    | NONE => NONE)
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
