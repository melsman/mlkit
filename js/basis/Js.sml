signature JS = 
sig
  (* dom *)
  eqtype doc eqtype elem
  val document        : doc
  val documentElement : doc -> elem
  val getElementById  : doc -> string -> elem option
  val parent          : elem -> elem option
  val firstChild      : elem -> elem option
  val lastChild       : elem -> elem option
  val nextSibling     : elem -> elem option
  val previousSibling : elem -> elem option
  val innerHTML       : elem -> string -> unit
  val value           : elem -> string
  val setAttribute    : elem -> string -> string -> unit
  val removeAttribute : elem -> string -> unit
(*  val setStyle        : elem -> string -> unit *)
  val createElement   : string -> elem
  val createTextNode  : string -> elem
  val createFragment  : unit -> elem
  val appendChild     : elem -> elem -> unit
  val removeChild     : elem -> elem -> unit
  val replaceChild    : elem -> elem -> elem -> unit

  (* events *)
  datatype eventType = onclick | onchange | onkeypress 
                     | onkeyup | onmouseover | onmouseout
  val installEventHandler : elem -> eventType -> (unit -> bool) -> unit
  val getEventHandler     : elem -> eventType -> (unit -> bool) option
  val onMouseMove         : doc -> (int*int -> unit) -> unit

  (* timers *)
  type intervalId
  val setInterval     : int -> (unit -> unit) -> intervalId
  val clearInterval   : intervalId -> unit

  type timeoutId
  val setTimeout      : int -> (unit -> unit) -> timeoutId
  val clearTimeout    : timeoutId -> unit

  (* styles *)
  val setStyle        : elem -> string * string -> unit

  structure XMLHttpRequest : sig
    type req
    val new              : unit -> req
    val openn            : req -> {method: string, url: string, async: bool} -> unit
    val setRequestHeader : req -> string * string -> unit
    val send             : req -> string option -> unit
    val state            : req -> int        (* 0,1,2,3,4 *)
    val status           : req -> int option (* 200, 404, ... *)
    val onStateChange    : req -> (unit -> unit) -> unit
    val response         : req -> string option
    val abort            : req -> unit
  end 

  val random             : unit -> real
(*
  val rpc : 'a T -> 'b T -> {url: string, method: string} 
	    -> 'a -> 'b

  val rpcAsync : 'a T -> 'b T -> {url: string, method: string} 
	    -> ('b -> unit) -> 'a -> unit
*)
end

(*
  [parent e] returns SOME p, if p is the parent of e. Returns NONE if
  e has no parent.

  [appendChild e child] appends child to e.

  [removeChild e child] removes child from e.

  [replaceChild e new old] replaces old child from e with new child.

  [random()] returns a random real in the interval [0.0,1.0[.
*)

structure JsSecret :> sig 
  include JS 
  val fromDoc : doc -> foreignptr 
end =
struct

structure J = JsCore

(* dom *)
type doc = foreignptr
fun fromDoc a = a

type elem = foreignptr

val document = J.exec0 {stmt="return document;", res=J.fptr} ()

fun getElementById (d:doc) (id:string) : elem option =
    J.exec2 {stmt="return SmlPrims.option(d.getElementById(id));",
             arg1=("d",J.fptr),
             arg2=("id",J.string),
             res=J.option J.fptr} (d,id)

fun parent(e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.parentNode);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun firstChild(e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.firstChild);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun lastChild(e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.lastChild);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun nextSibling(e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.nextSibling);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun previousSibling(e:elem) : elem option =
    J.exec1 {stmt="return SmlPrims.option(e.previousSibling);",
             arg1=("e",J.fptr),
             res=J.option J.fptr} e

fun innerHTML (e:elem) (s:string) : unit =
    J.exec2 {stmt="e.innerHTML = s;",
             arg1=("e",J.fptr),
             arg2=("s",J.string),
             res=J.unit} (e,s)

(* events *)
datatype eventType = onclick | onchange | onkeypress | onkeyup | onmouseover | onmouseout
                     
fun installEventHandler (e: elem) (et: eventType) (f: unit -> bool) : unit =
    let val arg1 = ("e", J.fptr)
        val arg2 = ("f", J.==>(J.unit,J.bool))
        val stmt =
            case et of
              onclick     => "e.onclick = f;"
            | onchange    => "e.onchange = f;"
            | onkeypress  => "e.onkeypress = f;"
            | onkeyup     => "e.onkeyup = f;"
            | onmouseover => "e.onmouseover = f;"
            | onmouseout  => "e.onmouseout = f;"
    in
      J.exec2 {stmt=stmt, arg1=arg1, 
               arg2=arg2, res=J.unit} (e,f)                      
    end
    
fun getEventHandler (e: elem) (et: eventType) : (unit -> bool) option =
    NONE


fun onMouseMove (d: doc) (f : int*int->unit) : unit =
    J.exec2 {stmt="return d.onmousemove = function(ev) { f([ev.pageX,ev.pageY]); };",
             arg1=("d",J.fptr), arg2=("f",J.===>(J.int,J.int,J.unit)), res=J.unit} (d,f)
    

type intervalId = foreignptr
fun setInterval (i: int) (f: unit -> unit) : intervalId =
    let val arg1 = ("i", J.int)
        val arg2 = ("f", J.==>(J.unit,J.unit))
    in
      J.exec2 {stmt="return setInterval(f,i);", 
               arg1=arg1, arg2=arg2,
               res=J.fptr} (i,f)
    end
    
fun clearInterval (id: intervalId) : unit =
    let val arg1 = ("id", J.fptr)
    in
      J.exec1 {stmt="return clearInterval(id);", 
               arg1=arg1, res=J.unit} id
    end    

type timeoutId = foreignptr
fun setTimeout (i: int) (f: unit -> unit) : timeoutId =
    let val arg1 = ("i", J.int)
        val arg2 = ("f", J.==>(J.unit,J.unit))
    in
      J.exec2 {stmt="return setTimeout(f,i);", 
               arg1=arg1, arg2=arg2,
               res=J.fptr} (i,f)
    end

fun clearTimeout (id: timeoutId) : unit =
    let val arg1 = ("id", J.fptr)
    in
      J.exec1 {stmt="return d.clearTimeout(id);", 
               arg1=arg1, res=J.unit} id
    end

fun value (e:elem) : string =
    J.exec1{stmt="return e.value;",
            arg1=("e",J.fptr),
            res=J.string} e

fun documentElement (d:doc) : elem =
    J.exec1{stmt="return d.documentElement;",
            arg1=("d",J.fptr),
            res=J.fptr} d

fun setStyle (e : elem) (a: string) : unit =
    J.exec2 {stmt="e.setAttribute('style',a); e.style.cssText = a;",
             arg1=("e",J.fptr), arg2=("a",J.string), res=J.unit} (e,a)

fun setAttribute (e : elem) (a: string) (b:string) : unit =
      J.exec3 {stmt="return e.setAttribute(a,b);",
               arg1=("e",J.fptr), arg2=("a",J.string), arg3=("b",J.string), res=J.unit} (e,a,b)

fun removeAttribute (e : elem) (a: string) : unit =
    J.exec2 {stmt="return e.removeAttribute(a);",
             arg1=("e",J.fptr), arg2=("a",J.string), res=J.unit} (e,a)

fun createElement (t : string) : elem =
    J.call1 ("document.createElement", J.string, J.fptr) t

fun createFragment () : elem =
    J.call0 ("document.createDocumentFragment", J.fptr)

fun createTextNode (t : string) : elem =
    J.call1 ("document.createTextNode", J.string, J.fptr) t

fun appendChild (e : elem) (a: elem) : unit =
    J.exec2 {stmt="return e.appendChild(a);",
             arg1=("e",J.fptr), arg2=("a",J.fptr), res=J.unit} (e,a)

fun removeChild (e : elem) (a: elem) : unit =
    J.exec2 {stmt="return e.removeChild(a);",
             arg1=("e",J.fptr), arg2=("a",J.fptr), res=J.unit} (e,a)

fun replaceChild (e : elem) (a: elem) (old: elem) : unit =
    J.exec3 {stmt="return e.replaceChild(a,old);",
             arg1=("e",J.fptr), arg2=("a",J.fptr), arg3=("old",J.fptr), 
             res=J.unit} (e,a,old)

fun setStyle (e: elem) (s:string,v:string) : unit =
    let val st = J.getProperty e J.fptr "style"
    in J.setProperty st J.string s v
    end


structure XMLHttpRequest =
  struct
      type req = foreignptr
      fun new() : req =
          J.call0 ("SmlPrims.newRequest", J.fptr)

      fun openn(r:req) {method: string, url: string, async: bool} : unit =
          J.exec4 {stmt="return r.open(m,u,a);",
                   arg1=("r",J.fptr),arg2=("m",J.string),arg3=("u",J.string),
                   arg4=("a",J.bool),res=J.unit} (r,method,url,async)
      
      fun send (r:req) (SOME s) : unit =
          J.exec2 {stmt="return r.send(s);",
                   arg1=("r",J.fptr),arg2=("s",J.string),res=J.unit} (r,s)
        | send r NONE = 
          J.exec1 {stmt="return r.send(null);",
                   arg1=("r",J.fptr),res=J.unit} r

      fun setRequestHeader (r:req) (k:string,v:string) : unit =
          J.exec3 {stmt="return r.setRequestHeader(k,v);",
                   arg1=("r",J.fptr),arg2=("k",J.string),arg3=("v",J.string),
                   res=J.unit} (r,k,v)

      fun state (r:req) : int =
          J.exec1 {stmt="return r.readyState;",
                   arg1=("r",J.fptr),res=J.int} r

      fun status (r:req) : int option =
          J.exec1 {stmt="return SmlPrims.option(r.status);",
                   arg1=("r",J.fptr),
                   res=J.option J.int} r

      fun onStateChange (r:req) (f: unit -> unit) : unit =
          J.exec2{stmt="r.onreadystatechange = f;",
                  arg1=("r",J.fptr),arg2=("f",J.==>(J.unit,J.unit)),
                  res=J.unit} (r,f)

      fun response (r:req) : string option =
          J.exec1 {stmt="return SmlPrims.option(r.responseText);",
                   arg1=("r",J.fptr),
                   res=J.option J.string} r

      fun abort (r:req) : unit =
          J.exec1 {stmt="return r.abort();",
                   arg1=("r",J.fptr),
                   res=J.unit} r
  end

fun random() : real =
    J.exec0 {stmt="return Math.random();",
             res=J.real} ()
end

structure Js : JS = JsSecret
